rm(list=ls())

# helper functions
f <- list.files(path=".", pattern="^func_.+[.]r$")
sapply(f, source)

# version of EUCAST data (year, either 2014 or 2024)
version <- "2024"

# output directory
odir <- "output"

# import databases
rd <- function(f) { read.table(file=f, sep="\t", header=TRUE) }
db <- list(
  mic= rd(paste0("../1_databases/db_mic_",version,".tsv")),
  ecoff= rd(paste0("../1_databases/db_ecoff_",version,".tsv")),
  orga= rd(paste0("../1_databases/db_organisms_",version,".tsv")),
  drugs= rd("../1_databases/db_drugs.tsv")
)

# general settings for Bengtsson-Palme & Larsson algorithm
settings <- list(
  nCut = 10,
  pCut = 0.01,
  nScale = 41
)

# collect lowest MIC estimates for all drugs in summary table
local({
  drugs <- sort(unique(db$mic[,"drug"]))
  for (d in drugs) {
    x <- evalMIC_globalLevel(drug=d,
      dbMIC=db$mic, dbECOFF=db$ecoff, dbOrga=db$orga,
      nCut=settings[["nCut"]],  pCut=settings[["pCut"]],
      nScale=settings[["nScale"]]
    )
    for (i in which(grepl(x=names(x), pattern="^lowest.MIC"))) {
      x[,i] <- signif(x[,i], 3)
    }
    write.table(x, file=paste0(odir,"/MIC_lowest.tsv"), sep="\t",
      col.names=(match(d, drugs)==1), row.names=F, quote=F, append=(match(d, drugs) >1))
  }
})

# visualization of the distribution of MIC quantiles (per organism)
# together with the final estimate of MIC_lowest

plt <- function(
  drug,     # drug name
  values,   # vector of MIC quantiles for tested organisms
  globEst   # global lowest observed MIC marked
) {
  values <- values[is.finite(values)]
  values <- floor(log2(values))
  globEst <- floor(log2(globEst))
  rng <- range(c(values, globEst, -9, 9))
  classes <- min(rng):max(rng)
  if (length(values) > 0) {
    stopifnot(all(values %in% classes))
    x <- sapply(classes, function(x) { sum(values == x) })
    layout(matrix(1:2, ncol=1), heights=0.2, 0.8)
    omar <- par("mar")
    par(mar=c(1,5,1,1))
    plot(0, 0, type="n", axes=FALSE, ann=FALSE)
    legend("topright", bty="n", col="red", lty=1,
      legend=expression(paste("MIC",{}["lowest"])))
    par(mar=c(5.5,5,1,1))    
    z <- barplot(x,
#      names.arg=as.expression(sapply(classes, function(x) {substitute(2^p, list(p=x))})),
      names.arg=classes,
      col="oldlace", border="grey", xaxt="n", yaxt="n",
      xlab="", ylab="Cases in bin")
    mtext(side=1, line=4, paste0(drug, " (mg/L)"))
    axis(side=1, at=z, labels=signif(2^classes,3), line=-0.5, lwd=0, las=2)
    axis(side=2, at=c(0, 1, 2, 5, 10, 20, 20, 40, 50, 75, 100), las=2)
    abline(v=z[match(globEst, classes)], col="red")
    par(mar=omar)
    layout(1)
  } else {
    plot(0, 0, type="n", axes=FALSE, ann=FALSE)
    legend("center", legend="no finite data for plotting")
  }
}

local({
  drugs <- sort(unique(db$mic[,"drug"]))
  for (d in drugs) {
    orga <- evalMIC_organismLevel(drug=d,
      dbMIC=db$mic, dbECOFF=db$ecoff,
      nCut=settings[["nCut"]],  pCut=settings[["pCut"]]
    )
    orga <- setNames(orga[,"MIC.quantile"], orga[,"organism"])
    glob <- evalMIC_globalLevel(drug=d,
      dbMIC=db$mic, dbECOFF=db$ecoff, dbOrga=db$orga,
      nCut=settings[["nCut"]],  pCut=settings[["pCut"]],
      nScale=settings[["nScale"]]
    )
    svglite::svglite(file=paste0(odir,"/visual_rawMinMIC_",d,".svg"), width=6, height=4.5)
    plt(d, orga, glob[,"lowest.MIC.quantile.extrapol.scaled.rounded"])
    graphics.off()
  }
})

