
# implements the non-standard rounding scheme used by EUCAST
roundDownEUCAST <- function(x) {
  concs <- c("0.002", "0.004", "0.008", "0.016", "0.03", "0.06", "0.125",
    "0.25", "0.5", "1", "2", "4", "8", "16", "32", "64", "128", "256", "512")
  concs <- as.numeric(concs)
  f <- function(x) {
    x <- approx(x=concs, y=concs, xout=x, method="constant",
      yleft=x, yright=NA, f=0)$y
  }
  sapply(x, f)
}

# computes "MIC lowest" for a particular drug considering data from
# all tested organisms; besides "MIC lowest" useful metadata is returned
evalMIC_globalLevel <- function(
  drug,      # drug identifier
  dbMIC,     # database of MIC values
  dbECOFF,   # database of ECOFF values
  dbOrga,    # database of organisms names (for scaling)
  nCut=10,   # passed to "lowestMIC_organismLevel"
  pCut=0.01, # passed to "lowestMIC_organismLevel"
  nScale=41  # critical number of species in scaling approach
) {
  
  # get MIC quantiles at organism level
  x <- evalMIC_organismLevel(
    drug=drug, dbMIC=dbMIC, dbECOFF=dbECOFF, nCut=nCut, pCut=pCut
  )
  
  n.organisms.tested <- nrow(x)
  
  # drop NA values resulting from too small data sets
  x <- x[is.finite(x[,"MIC.quantile"]),]
  n.organisms.evaluated <- nrow(x)

  # get number of unique species names for the scaling
  # note: Bengtsson-Palme & Larsson rather filtered for species names
  #   found in the SILVA database, but this appears sub-optimal as it
  #   creates another depencency on a possibly dynamic datasource
  stopifnot(all(x[,"organism"] %in% dbOrga[,"name"]))
  n.valid.species <- sum((dbOrga[,"name"] %in% x[,"organism"]) & dbOrga[,"valid_species"])
  if (n.valid.species == 0) {
    # can happen, if only type strains or specific linages were tested
    # but no actual populations; this is currently (2024) only the case
    # for M. tuberculosis (lineages only) and (independently) the drug
    # cefiredocol (ATCC strains only)
    z <- gsub(x[,"organism"], pattern="(^[A-Z][a-z]+ [a-z]+)[^a-z].*$", replacement="\\1")
#    print(unique(z))
    n.valid.species <- length(unique(z))
    print(paste("number of species for drug ",drug," corrected from 0 t0 ",n.valid.species))
  }

  if (nrow(x) > 0) {
    # get lowest MICs at global level
    lowest.MIC.quantile <- min(x[,"MIC.quantile"])
    lowest.MIC.quantile.extrapol <- min(x[,"MIC.quantile.extrapol"])
    most.sensitive <- x[x[,"MIC.quantile.extrapol"] == lowest.MIC.quantile.extrapol, "organism"]
    # we use the same re-scaling as in the Bengtsson-Palme & Larsson 2016 paper
    lowest.MIC.quantile.extrapol.scaled <- if (n.valid.species >= (nScale-1)) {
      lowest.MIC.quantile.extrapol
    } else {
      lowest.MIC.quantile.extrapol * n.valid.species / nScale
    }
  } else {
    lowest.MIC.quantile <- NA
    lowest.MIC.quantile.extrapol <- NA
    most.sensitive <- NA
    lowest.MIC.quantile.extrapol.scaled <- NA
  }  
  data.frame(
    drug=drug,
    organisms.tested=n.organisms.tested,
    organisms.evaluated=n.organisms.evaluated,
    valid.species=n.valid.species,
    lowest.MIC.quantile= lowest.MIC.quantile,
    lowest.MIC.quantile.extrapol= lowest.MIC.quantile.extrapol,    
    lowest.MIC.quantile.extrapol.scaled= lowest.MIC.quantile.extrapol.scaled,
    lowest.MIC.quantile.extrapol.scaled.rounded= roundDownEUCAST(lowest.MIC.quantile.extrapol.scaled),
    most.sensitive=paste(most.sensitive, collapse=",")
  )
}
