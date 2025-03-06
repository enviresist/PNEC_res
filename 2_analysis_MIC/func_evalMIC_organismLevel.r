# Returns a data frame with MIC quantiles and possibly extrapolated
# estimates for all tested organisms.
# The returned indicators may be NA if the filter criteria were not
# met (i.e. data were insufficient).

evalMIC_organismLevel <- function(
  drug,      # drug identifier
  dbMIC,     # database of MIC values
  dbECOFF,   # database of ECOFF values
  nCut=10,   # require at least 10 data points to support lMIC values
  pCut=0.01  # define lMIC as the MIC observed in 1% of the cases
) {
  # check databases
  needed <- c("drug","organism","mg_per_L","count")
  if (!all(needed %in% names(dbMIC))) stop("missing column(s) in MIC database")
  needed <- c("drug","organism","mg_per_L","comment")
  if (!all(needed %in% names(dbECOFF))) stop("missing column(s) in ECOFF database")
  # filter for drug
  if (!drug %in% unique(dbMIC[,"drug"])) stop(paste0("no data for drug '",drug,"'"))
  x <- dbMIC[dbMIC[,"drug"] == drug,]
  x <- x[names(x)[names(x) != "drug"]]
  # transform to wide table
  x <- reshape2::acast(organism ~ mg_per_L, value.var="count", data=x)
  stopifnot(!is.unsorted(as.numeric(colnames(x))))
  # step 1: remove values greater than ECOFF
  ecf <- dbECOFF[dbECOFF[,"drug"] == drug, names(dbECOFF) != "drug"]
  ecf <- with(ecf, setNames(mg_per_L, organism))
#  print(sort(rownames(x)))
#  print(sort(names(ecf)))
  stopifnot(identical(sort(rownames(x)), sort(names(ecf))))
  ecf <- ecf[rownames(x)]
  ## handle missing ecoff data
  ecf[!is.finite(ecf)] <- Inf   # assume all MICs to be from susceptible isolates
  ## drop counts greater than ecoff since those represent resistant isolates
  concs <- as.numeric(colnames(x))
  for (org in rownames(x)) {
    x[org, concs > ecf[org]] <- 0   # set counts to zero
  }
  # step 2 + 3: determine the concentration containing the lowest pCut*100% of
  # the MICs while making sure that it is supported by at least nCut cases
  fn <- function(x) {
    sel <- (cumsum(x) >= nCut) & (cumsum(x)/sum(x) >= pCut)
    if (any(sel)) concs[which(sel)[1]] else NA
  }
  MIC.quantile <- apply(x, 1, fn)
  # step 4: extrapolation
  fn <- function(x) {
    concs[which.max(x)]
  }
  MIC.quantile.extrapol <- MIC.quantile
  peakMIC <- apply(x, 1, fn)
  iExtrapol <- which(is.finite(MIC.quantile) & (MIC.quantile == min(concs)))
  iOther <-  which(is.finite(MIC.quantile) & (MIC.quantile > min(concs)))
  if ((length(iExtrapol) > 0) && (length(iOther) > 0)) {
    avg_log2dists <- mean(log2(peakMIC[iOther]) - log2(MIC.quantile[iOther]), na.rm=T)
    MIC.quantile.extrapol[iExtrapol] <- pmin(min(concs),
      2^(log2(peakMIC[iExtrapol]) - avg_log2dists))
  }
  stopifnot(identical(names(MIC.quantile), names(MIC.quantile.extrapol)))
  out <- data.frame(organism=names(MIC.quantile),
    MIC.quantile=MIC.quantile, MIC.quantile.extrapol=MIC.quantile.extrapol)
  rownames(out) <- NULL
  out
}
