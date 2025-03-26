rm(list=ls())

source("generate_functions.r")

version <- "2024"

crit_scaling_factor <- 0.1  # for warnings

odir <- "generatedHTML"

cssfile <- "input_static/styles.css"

headline <- "Predicted no-effect concentrations of antibiotics regarding resistance selection (PNEC<sub>res</sub>)"

links <- c(
  accessibility="https://enviresist.github.io/#accessibility",
  legalnotice="https://enviresist.github.io/#legal_notice"
)

rd <- function(f, ...) { read.table(file=f, sep="\t", header=TRUE, ...=...) }

drugs <- rd("../1_databases/db_drugs.tsv")

bpl16 <- rd("../1_databases/BengtssonPalmeAndLarsson2016.tsv")
bpl16 <- cbind(bpl16, scaling.factor=sapply(bpl16[,"valid.species"],
function(n) { if (!is.finite(n)) NA else if (n < 40) signif(n/41,3) else 1 }) )

qmics <- rd("../2_analysis_MIC/output/MIC_quantiles.tsv")
qmics <- qmics[qmics[,"version"] == version, names(qmics) != "version"]

lmics <- rd("../2_analysis_MIC/output/MIC_lowest.tsv")
lmics <- lmics[lmics[,"version"] == version, names(lmics) != "version"]

costs <- rd("../3_analysis_cost/output/cost_quantiles.tsv", check.names=F)
names(costs)[grepl(names(costs), pattern="profile")] <- "95% CI <sup>x</sup>"
names(costs)[grepl(names(costs), pattern="bootstrap")] <- "95% CI <sup>y</sup>"

if (!identical(sort(drugs$drug), sort(lmics$drug))) { stop("mismatch in listed drugs") }
if (!identical(sort(drugs$drug), sort(bpl16$drug))) { stop("mismatch in listed drugs") }

stopifnot(file.copy(cssfile, odir, overwrite=TRUE))
cssfile <- basename(cssfile)

########################################################################
# start registry of pages accessible from main menu
########################################################################

navi <- data.frame(
  menulabel = character(0),
  pagetitle = character(0),
  htmlcontents = character(0),
  targetfile = character(0)
)

########################################################################
# register entry start page
########################################################################

navi <- rbind(navi, data.frame(
  menulabel="About",
  pagetitle="About",
  htmlcontents= paste(readLines("input_static/about.html"), collapse="\n"),
  targetfile="index.html"
))

########################################################################
# register drug selection page
########################################################################

x <- drugs
x[,"drug"] <- paste0("<a href='drug_",
  gsub(x[,"drug"],pattern=" ", replacement="_", fixed=TRUE),".html'>",
  x[,"drug"],"</a>")
x <- x[,c("drug", "drugclass", "drugfamily", "ATC_code")]
names(x) <- c("Drug", "Class", "Sub-class", "ATC code")
x <- apply(x, 1:2, function(x) if (is.na(x)) "" else x)

html <- paste0(
    "<p>Use the search field to filter table records. Click on a drug
    name to show details.</p>",
    table.interactive(x, id="drug_selection_table")
)

navi <- rbind(navi, data.frame(
  menulabel="Select drug",
  pagetitle="Select drug",
  htmlcontents= html,
  targetfile="select.html"
))

########################################################################
# register auxiliary pages
########################################################################

navi <- rbind(navi, data.frame(
  menulabel="Contact",
  pagetitle="Contact",
  htmlcontents= paste(readLines("input_static/contact.html"), collapse="\n"),
  targetfile="contact.html"
))

########################################################################
# create all pages accessible from main menu
########################################################################

pages.create(headline=headline, navi=navi, links=links,
  cssfile=cssfile, odir=odir)

########################################################################
# per drug summary data sheets - they are not linked to the main menu
########################################################################

x <- merge(lmics, drugs, by="drug")
x <- merge(x, bpl16, by="drug", suffixes=c(".current",".reference"))
rownames(x) <- x[,"drug"]
x <- x[,names(x) != "drug"]

as.df <- function(x) {data.frame(category=paste0(names(x),":"), value=x)}

tmpfile <- tempfile()

for (d in rownames(x)) {
  
  # drug metadata
  meta <- c(
    `Drug name`=d,
    `Drug class`=if (x[d,"drugclass"] != d) x[d,"drugclass"] else "-",
    `ATC code`=if (is.na(x[d,"ATC_code"])) "not available" else x[d,"ATC_code"]
  )

  # info
  info <- NULL
  info <- rbind(info, list("Distinct organisms tested in total",
    x[d,"organisms.tested"], ""))
  info <- rbind(info, list("Organisms with sufficient data to estimate a robust minimum MIC",
    x[d,"organisms.evaluated"], ""))

  info <- rbind(info, list(paste("Robust minimum MIC (mg/L)",
    tooltip("This is the minimum of the smallest MICs observed for individual test organisms.
      The underlying per-organism estimates are computed as empirical 1% quantiles of the individual
      MIC distributions. If the 1% quantile is supported by less than 10 records, the smallest MIC
      supported by at least 10 records is used instead.")),
    x[d,"lowest.MIC.quantile.current"], x[d,"lowest.MIC.quantile.reference"]))

  speclist <- paste0("<span style='font-style:italic'>", gsub(x[d,"most.sensitive"],
    pattern=",", replacement=",<br>", fixed=T),"</span>")
  info <- rbind(info, list(paste("Most sensitive organism(s)",
      tooltip("Test organism(s) to which the robust minimum MIC refers. These can be multiple
        organisms if their sensitivity is very similar.")),
    speclist, ""))


  info <- rbind(info, list(paste("Extrapolated robust minimum MIC (mg/L)",
    tooltip("Extrapolation is only performed if the robust minimum MIC is identical
      to the lowest concentration considered in standard MIC assays (0.002 mg/L).")),
    x[d,"lowest.MIC.quantile.extrapol.current"],
    if (is.finite(x[d,"lowest.MIC.quantile.extrapol.reference"]))
      x[d,"lowest.MIC.quantile.extrapol.reference"] else x[d,"lowest.MIC.quantile.reference"]
  ))

  info <- rbind(info, list(paste("Valid species considered for species coverage adjustment",
    tooltip("Represents the number of unique species names. Type strains and organisms
      with ambiguous species information were left out of considerations. However,
      Salmonella serovars were counted as independent species.")),
    x[d,"valid.species.current"], x[d,"valid.species.reference"]))

  warn <- function(x, crit) {
    if (is.finite(x) && (x <= crit)) "<span style='color:red'>(!)</span>" else ""
  }
  info <- rbind(info, list(paste("Species coverage adjustment factor (-)",
    tooltip("The factor corrects the (possibly extrapolated) robust minimum MIC to account for the number of tested species.
      The respective factor has been estimated by resampling and the value differs slightly between approaches.
      Small factors with an attached warning symbol indicate increased uncertainty.")),
    paste(x[d,"scaling.factor.current"], warn(x[d,"scaling.factor.current"], crit_scaling_factor)),
    paste(x[d,"scaling.factor.reference"], warn(x[d,"scaling.factor.reference"], crit_scaling_factor))
  ))  

  info <- rbind(info, list(paste("MIC<sub>lowest</sub> (mg/L)",
    tooltip("This is the (possibly extrapolated) robust minimum MIC after adjustment
      for species coverage. In the reference approach, numbers were subject to rounding.")),
    x[d,"lowest.MIC.quantile.extrapol.scaled.rounded.current"],x[d,"lowest.MIC.quantile.extrapol.scaled.rounded.reference"]))

  cost_quantile <- costs[costs[,"Probability"] == 0.05, "Cost"]
  info <- rbind(info, list(paste("Conversion factor to turn MIC<sub>lowest</sub> into a predicted MSC (-)",
    tooltip("The value of 0.004 represents the 5% quantile of the cost associated with plasmid-borne resistance.
      The value of 0.1 employed in the reference approach lacks a direct ecological interpretation and was originally
      understood as an assessment factor.")),
    cost_quantile, 1/10))

  info <- rbind(info, list(paste0("Predicted MSC a.k.a. PMSC (mg/L)",
    tooltip("The product of MIC<sub>lowest</sub> and the above conversion factor. The PMSC represents a minimum estimate
      of the MSC for a multi-species and possibly multi-strain community. It does not refer to a particular pair of
      resistant and susceptible competitors.")),  
    cost_quantile * x[d,"lowest.MIC.quantile.extrapol.scaled.rounded.current"], x[d,"PNECR"]))

  info <- rbind(info, list(paste0("Assessment factor to convert PMSC into PNEC<sub>res</sub> (-)",
    tooltip("The assessment factor of 1 reflects that uncertainties are sufficiently covered by the
      algorithms underlying the calculation of the PMSC.")),
    "1", "not used"))

  info <- rbind(info, list(paste0("PNEC<sub>res</sub> (mg/L)",
    tooltip("The value is identical to PMSC by intention, given the assessment factor of 1.")),
    cost_quantile * x[d,"lowest.MIC.quantile.extrapol.scaled.rounded.current"], x[d,"PNECR"]))

  info <- as.data.frame(apply(info,2,unlist))
  names(info) <- c("", "Proposed <sup>1</sup>", "Reference <sup>2</sup>") 

  footnotes <- rbind(
    c("1", "<span style='color:red';>Based on EUCAST MIC data of 2024. Link to publication to be added here.</span>"),
    c("2", "From Table 1 of the <a href='https://doi.org/10.1016/j.envint.2015.10.015' target='_blank'>
      Bengtsson-Palme & Larsson (2016)</a> paper. The corresponding EUCAST MIC data are from 2014.")
  )
  footnotes <- paste0("<div style='font-size:smaller;'>","\n",
    paste("<sup>",footnotes[,1],"</sup> ",footnotes[,2],"<br>", collapse="\n"),"\n",
  "</div>","\n")
 
  svg(tmpfile, width=6, height=4)
  plot_MIC_quantiles(drug=d, q=qmics[qmics[,"drug"] == d, "MIC.quantile"],
    e=lmics[lmics[,"drug"] == d, "lowest.MIC.quantile.extrapol.scaled.rounded"])
  graphics.off()

  html <- paste0(

    page.head(headline=headline, title=d, navi=cbind(navi, active=FALSE),
      links=links, cssfile=cssfile),
    
    "<h1>Drug meta data</h1>","\n",
    table.static(as.df(meta), colnames=FALSE),"\n\n",
    
    "<h1>Estimates</h1>","\n",
    table.static(info, colnames=TRUE, class="stripedTable"),"\n\n",
    footnotes,"\n\n",

    "<h1>Details</h1>","\n",
    "<h2>Estimation of MIC<sub>lowest</sub></h2>","\n",
    expandableSection(
      embedSVG(file=tmpfile,
        caption="Distribution of original MIC quantiles of the tested organisms
          in comparison to MIC<sub>lowest</sub>."),
      labelIfClosed="[Show details]", labelIfOpen="[Hide details]"
    ),"\n\n",
    "<h2>Cost-based MSC to MIC conversion factor</h2>","\n",
    expandableSection(
        table.static(costs, colnames=TRUE, class="stripedTable",
          caption="Quantiles of the fitness cost attributable to plasmid-borne
          antibiotic resistance. The cost is a dimensionless number in range
          [0,1] expressing the loss of fitness upon acquisition of resistance
          in comparison to a susceptible bacterial isolate. The cost corresponding
          to 5% probability is currently employed to convert MIC into MSC and
          thus to translate MIC<sub>lowest</sub> into PMSC.
          Superscripts x and y denote parametric and bootstrap confidence
          intervals, respectively."),
      labelIfClosed="[Show details]", labelIfOpen="[Hide details]"
    ),"\n\n",
    
    page.foot()
  )
  write(html, paste0(odir,"/drug_",
    gsub(d, pattern=" ", replacement="_", fixed=TRUE),".html"))
}

file.remove(tmpfile)
