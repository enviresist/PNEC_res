rm(list=ls())

# output directory
odir <- "output"

# minimum number of organisms to include data in resampling
min_org <- 30

# number of resamplings
nrep <- 100

# per-organism MIC quantiles
data <- read.table(file=paste0(odir,"/MIC_quantiles.tsv"), sep="\t", header=TRUE)


plt <- function(x, version) {  
  omar <- par("mar")
  par(mar=c(4.5,5.5,1,1))
  plot(min_sample/min_true ~ norg, data=x, type="n", log="xy",
    yaxt="n", xlab="Number of organisms in subsample", ylab="", bty="L")
  fn <- function(z) {
    x <- unique(z[,"norg"])
    y <- quantile(z[,"min_sample"] / z[,"min_true"], probs=c(0.95, 0.75, 0.5, 0.25, 0.05))
    lines(c(x,x), c(y["5%"],y["95%"]), col="grey")
    points(x, y["50%"], pch=20, col="grey")
    points(x, mean(z[,"min_sample"] / z[,"min_true"]), pch=21, bg="white", col="grey")
  }
  ignore <- by(x, x[,"norg"], fn)
  axis(2, at=c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000), las=2)
  mtext(side=2, line=3.7, expression(paste("MIC",{}[min]," for subsample  /  MIC",{}[min])))

#  fit <- nls(log(min_sample/min_true) ~ log(k/norg), data=x, start=c(k=41))
#  print(coef(fit)["k"])
#  lines(norg, coef(fit)["k"]/norg)
  
  # fit continuous model
  fit <- nls(log(min_sample/min_true) ~ log(k/(norg^b)+1), data=x, start=c(k=41, b=1))
  #print(coef(fit)[c("k","b")])
  
  # show model and reference
  color_red <- rgb(206,31,94,maxColorValue=255)
  color_blue <- rgb(0,95,133,maxColorValue=255)
  n <- 1:100
  lines(n, coef(fit)["k"]/(n^coef(fit)["b"])+1, lty=4, col=color_red)
  lines(n, ifelse(n > 40, 1, 41/n), lty=2, col=color_blue)
  
  legend("bottomleft", bty="n", legend=paste0(version," EUCAST data"))
#  legend("topright", bty="n", pch=c(1, 20, NA, NA, NA),
#    lty=c(NA, NA, 1, 2, 4), col=c("grey","grey","grey","blue","red"),
#    legend=c("Mean", "Median", "5% - 95% Q.", "Legacy model",
#      "Continuous fit"))
  legend("topright", bty="n", title="Variation across drugs",
    pch=c(1, 20, NA), lty=c(NA, NA, 1), col="grey",
    legend=c("Mean", "Median", "5% - 95% Quantile"))
  legend("right", bty="n", lty=c(2, 4), col=c(color_blue,color_red),
    legend=c("Bengtsson-Palme\n& Larsson (2016)", "Continuous fit"))
  par(mar=omar)
  
  rbind(
    data.frame(
      version = version,
      approach = "legacy",
      scalefunc = "scalefunc <- function(n) { if (n < 40) n / 41 else 1 }"
    ),
    data.frame(
      version = version,
      approach = "continuous",
      scalefunc = paste0("scalefunc <- function(n) { n^",
        coef(fit)['b']," / (",coef(fit)['k']," + n^",coef(fit)['b'],") }")
    )
  )
}

append <- FALSE
for (v in unique(data[,"version"])) {

  # data of particular version
  x <- data[data[,"version"] == v,]
  
  # filter to antibiotics with at least "min_org" tested organisms
  num_org <- tapply(x[,"organism"], x[,"drug"], function(x) {length(unique(x))})
  x <- x[x[,"drug"] %in% names(num_org)[num_org >= min_org],]

  # subsample MIC quantiles
  fn <- function(x) {
    min_true <- min(x[,"MIC.quantile.extrapol"], na.rm=T)
    orgs <- unique(x[,"organism"])
    out <- NULL
    for (norg in 1:length(orgs)) {
      min_sample <- c()
      for (irep in 1:nrep) {
        min_sample <- c(min_sample, min(x[x[,"organism"] %in% sample(orgs, size=norg), "MIC.quantile.extrapol"]))
      }
      out <- rbind(out, data.frame(version=v, drug=unique(x[,"drug"]),
        norg=norg, repl=1:nrep, min_true=min_true,
        min_sample=mean(min_sample)   # NOTE: We only output the average
      ))
    }
    out
  }
  res <- do.call(rbind, by(x, x[,"drug"], fn))
  res <- res[is.finite(res[,"min_sample"]),]
  #print(head(res))
  
  svg(paste0(odir,"/scaling_",v,".svg"), width=5, height=4)
  models <- plt(res, version=v)
  graphics.off()
  
  write.table(x=models, file=paste0(odir,"/scaling_models.tsv"),
    sep="\t", col.names=!append, row.names=F, quote=F, append=append)
  append <- TRUE

}

