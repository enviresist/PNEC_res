rm(list=ls())

# output directory
odir <- "output"

# cost database
db <- read.table("../1_databases/db_cost.tsv", header=T, sep="\t")

# colors for plotting
colOther <- rgb(170/255, 130/255, 100/255, alpha=0.3)
colPlasm <- rgb(120/255, 170/255, 200/255, alpha=0.3)

# genera represented in the database
svg(paste0(odir,"/cost_genera.svg"), width=4.5, height=4.0)
x <- db
x[,"resistance_type"] <- ifelse(x[,"resistance_type"] == "plasmid", "plasmid", "other")
x <- cbind(genus=gsub(x[,"species"], pattern=" .+$", replacement=""), x)
x <- aggregate(list(count=1:nrow(x)), x[,c("resistance_type","genus")], length)
x <- reshape2::acast(genus ~ resistance_type, data=x, value.var="count", fill=0)
x <- t(x)
omar <- par("mar")
par(mar=c(4.2,7.5,0.5,1))
pos <- barplot(x, col=c(colOther, colPlasm),
  names.arg=as.expression(sapply(colnames(x), function(x) {substitute(italic(n), list(n=x))})),
  horiz=T, axes=F, las=2, border="darkgrey", xlab="Reported cases")
axis(1)
legend("topright", bty="n", fill=c(colPlasm, colOther), border="darkgrey",
  legend=c("Plasmid-borne", "Chromosomal"), title="Localization of resistance")
par(mar=omar)
graphics.off()

# distribition of costs as histogram
svg(paste0(odir,"/cost_histo.svg"), width=4.5, height=4.5)
breaks <- seq(-0.7, 1, 0.1)
plasm <- hist(db[db$resistance_type == "plasmid", "cost"], breaks=breaks, plot=F)
other <- hist(db[db$resistance_type != "plasmid", "cost"], breaks=breaks, plot=F)
plot(range(breaks), range(plasm$count, other$count), type="n", bty="n",
  xlab="Cost", ylab="Frequency")
fn <- function(i, histo, col) {
  rect(xleft=histo$breaks[i], xright=histo$breaks[i+1],
    ybottom=0, ytop=histo$counts[i], col=col, border="darkgrey")
}
sapply(1:length(other$counts), fn, other, colOther)
sapply(1:length(plasm$counts), fn, plasm, colPlasm)
legend("topright", bty="n", fill=c(colPlasm, colOther), border="darkgrey",
  legend=c("Plasmid-borne", "Chromosomal"))
wt <- wilcox.test(
  x=db[db$resistance_type == "plasmid", "cost"],
  y=db[db$resistance_type != "plasmid", "cost"]
)
legend("right", bty="n", legend=substitute(paste(italic("p"),"=",x),
  list(x=signif(wt$p.value,2))))
graphics.off()
