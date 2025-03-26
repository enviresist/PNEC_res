rm(list=ls())

# output directory
odir <- "output"

scaling_approach <- "continuous"  # either "legacy" or "continuous"

# get input data
rd <- function(f) { read.table(file=f, sep="\t", header=TRUE) }
mic_quantiles <- rd(paste0(odir,"/MIC_quantiles.tsv"))          # output of previous step
scaling_models <- rd(paste0(odir,"/scaling_models.tsv"))        # output of previous step
organisms <- rd("../1_databases/db_organisms.tsv")

filter_version <- function(x, v) { x[x[,"version"] == v, names(x) != "version"] }
filter_drug <- function(x, d) { x[x[,"drug"] == d, names(x) != "drug"] }

out <- NULL
for (v in unique(mic_quantiles[,"version"])) {
  
  mod <- filter_version(scaling_models, v)
  eval(parse(text=mod[mod[,"approach"] == scaling_approach, "scalefunc"]))
  
  org <- filter_version(organisms, v)
  
  qtl_all <- filter_version(mic_quantiles, v)
  
  for (d in unique(qtl_all[,"drug"])) {
    qtl <- filter_drug(qtl_all, d)

    n.organisms.tested <- nrow(qtl)
    
    # drop NA values resulting from too small data sets
    qtl <- qtl[is.finite(qtl[,"MIC.quantile"]),]
    n.organisms.evaluated <- nrow(qtl)

    # get number of unique species names for the scaling
    # note: Bengtsson-Palme & Larsson rather filtered for species names
    #   found in the SILVA database, but this appears sub-optimal as it
    #   creates another depencency on a possibly dynamic datasource
    stopifnot(all(qtl[,"organism"] %in% org[,"name"]))
    n.valid.species <- sum((org[,"name"] %in% qtl[,"organism"]) & org[,"valid_species"])
    if (n.valid.species == 0) {
      # can happen, if only type strains or specific linages were tested
      # but no actual populations; this is currently (2024) only the case
      # for M. tuberculosis (lineages only) and (independently) the drug
      # cefiredocol (ATCC strains only)
      z <- gsub(qtl[,"organism"], pattern="(^[A-Z][a-z]+ [a-z]+)[^a-z].*$", replacement="\\1")
  #    print(unique(z))
      n.valid.species <- length(unique(z))
      print(paste("number of species for drug ",d," corrected from 0 t0 ",n.valid.species))
    }

    if (nrow(qtl) > 0) {
      # get lowest MICs at global level
      lowest.MIC.quantile <- min(qtl[,"MIC.quantile"])
      lowest.MIC.quantile.extrapol <- min(qtl[,"MIC.quantile.extrapol"])
      most.sensitive <- qtl[qtl[,"MIC.quantile.extrapol"] == lowest.MIC.quantile.extrapol, "organism"]
      # correction based on the number of tested species
      scaling.factor <- scalefunc(n.valid.species)
      lowest.MIC.quantile.extrapol.scaled <- lowest.MIC.quantile.extrapol * scaling.factor
        
    } else {
      lowest.MIC.quantile <- NA
      lowest.MIC.quantile.extrapol <- NA
      most.sensitive <- NA
      scaling.factor <- NA
      lowest.MIC.quantile.extrapol.scaled <- NA
    }
    out <- rbind(out, data.frame(
      version=v,
      drug=d,
      organisms.tested=n.organisms.tested,
      organisms.evaluated=n.organisms.evaluated,
      valid.species=n.valid.species,
      lowest.MIC.quantile= lowest.MIC.quantile,
      lowest.MIC.quantile.extrapol= signif(lowest.MIC.quantile.extrapol, 3),
      lowest.MIC.quantile.extrapol.scaled= signif(lowest.MIC.quantile.extrapol.scaled, 3),
      lowest.MIC.quantile.extrapol.scaled.rounded= signif(lowest.MIC.quantile.extrapol.scaled,3),
      scaling.factor= signif(scaling.factor, 3),
      most.sensitive=paste(most.sensitive, collapse=",")
    ))
  }
}

write.table(x=out, file=paste0(odir,"/MIC_lowest.tsv"),
  sep="\t", col.names=T, row.names=F, quote=F)

























