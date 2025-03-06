rm(list=ls())

source("generate_functions.r")

odir <- "generatedHTML"

headline <- "Predicted no-effect concentrations of antibiotics regarding resistance selection (PNEC<sub>R</sub>)"

rd <- function(f) { read.table(file=f, sep="\t", header=TRUE) }
drugs <- rd("../1_databases/db_drugs.tsv")
bpl16 <- rd("../1_databases/BengtssonPalmeAndLarsson2016.tsv")
lmics <- rd("../2_analysis_MIC/output/MIC_lowest.tsv")
costs <- rd("../3_analysis_cost/output/cost_quantiles.tsv")

if (!identical(sort(drugs$drug), sort(lmics$drug))) {
  stop("mismatch in listed drugs") 
}

expandableSection <- function(content, labelIfOpen="Collapse", labelIfClosed="Open") {
  paste0(
   "<details>","\n",
   "  <summary>","\n",
   "     <span class='clickToHide'>",labelIfOpen,"</span>",
   "     <span class='clickToShow'>",labelIfClosed,"</span>",
   "  </summary>","\n",
   "    ",content,"\n",
    "</details>","\n\n"
  )
}

embedSVG <- function(file, caption) {
  paste0(   
    "<figure>","\n",
    paste(readLines(file), collapse="\n"),"\n",
    "<figcaption>",caption,"</figcaption>",
    "</figure>"
  )
}

########################################################################
# start registry of pages accessible from main menu
########################################################################

navi <- data.frame(
  menulabel = character(0),
  menusep = integer(0),
  pagetitle = character(0),
  htmlcontents = character(0),
  targetfile = character(0)
)

########################################################################
# register entry start page
########################################################################

navi <- rbind(navi, data.frame(
  menulabel="About",
  menusep= 0,
  pagetitle="About",
  htmlcontents= paste(readLines("input_static/about.html"), collapse="\n"),
  targetfile="index.html"
))

########################################################################
# register drug selection page
########################################################################

x <- drugs
x[,"drug"] <- paste0("<a href='drug_",x[,"drug"],".html'>",x[,"drug"],"</a>")
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
  menusep= 0,
  pagetitle="Select drug",
  htmlcontents= html,
  targetfile="select.html"
))

########################################################################
# register auxiliary pages
########################################################################

navi <- rbind(navi, data.frame(
  menulabel="Contact",
  menusep= 15,
  pagetitle="Contact",
  htmlcontents= paste(readLines("input_static/contact.html"), collapse="\n"),
  targetfile="contact.html"
))

navi <- rbind(navi, data.frame(
  menulabel="Accessibility",
  menusep= 0,
  pagetitle="Accessibility",
  htmlcontents= paste(readLines("input_static/accessibility.html"), collapse="\n"),
  targetfile="accessibility.html"
))

navi <- rbind(navi, data.frame(
  menulabel="Legal notice",
  menusep= 0,
  pagetitle="Legal notice",
  htmlcontents= paste(readLines("input_static/legal_notice.html"), collapse="\n"),
  targetfile="legal_notice.html"
))

########################################################################
# create all pages accessible from main menu
########################################################################

pages.create(headline=headline, navi=navi, odir=odir)

########################################################################
# per drug summary data sheets - they are not linked to the main menu
########################################################################

x <- merge(lmics, drugs, by="drug")
x <- merge(x, bpl16, by="drug", suffixes=c(".current",".reference"))
rownames(x) <- x[,"drug"]
x <- x[,names(x) != "drug"]

as.df <- function(x) {data.frame(category=paste0(names(x),":"), value=x)}

for (d in rownames(x)) {
  
  # drug metadata
  meta <- c(
    `Drug name`=d,
    `Drug class`=if (x[d,"drugclass"] != d) x[d,"drugclass"] else "-",
    `ATC code`=if (is.na(x[d,"ATC_code"])) "not available" else x[d,"ATC_code"]
  )

  # info
  info <- NULL
  info <- rbind(info, list("Organisms tested in total",
    x[d,"organisms.tested"], ""))
  info <- rbind(info, list("Organisms with sufficient n for quantile estimation",
    x[d,"organisms.evaluated"], ""))
  info <- rbind(info, list("Valid species considered for rescaling",
    x[d,"valid.species.current"], x[d,"valid.species.reference"]))
  speclist <- paste0("<span style='font-style:italic'>", gsub(x[d,"most.sensitive"],
    pattern=",", replacement=",<br>", fixed=T),"</span>")
  info <- rbind(info, list("Most sensitive organism(s)",
    speclist, ""))
  info <- rbind(info, list("Original MIC quantile <sup>*,1</sup> (mg/L)",
    x[d,"lowest.MIC.quantile.current"], x[d,"lowest.MIC.quantile.reference"]))
  info <- rbind(info, list("Extrapolated MIC quantile <sup>*,2</sup> (mg/L)",
    x[d,"lowest.MIC.quantile.extrapol.current"],
    if (is.finite(x[d,"lowest.MIC.quantile.extrapol.reference"]))
      x[d,"lowest.MIC.quantile.extrapol.reference"] else x[d,"lowest.MIC.quantile.reference"]
  ))
  info <- rbind(info, list("MIC<sub>lowest</sub> <sup>*,3</sup> (mg/L)",
    x[d,"lowest.MIC.quantile.extrapol.scaled.rounded.current"],x[d,"lowest.MIC.quantile.extrapol.scaled.rounded.reference"]))
  cost_quantile <- costs[costs[,"Probability"] == 0.05, "Cost"]
  info <- rbind(info, list("MSC / MIC<sub>lowest</sub> conversion factor <sup>4</sup> (-)",
    cost_quantile, 1/10))
  info <- rbind(info, list("PNEC<sub>R</sub> <sup>b</sup> (mg/L)",
    cost_quantile * x[d,"lowest.MIC.quantile.extrapol.scaled.rounded.current"], x[d,"PNECR"]))

  info <- as.data.frame(apply(info,2,unlist))
  names(info) <- c("", "Proposed (2024) <sup>a</sup>", "Reference (2014) <sup>b</sup>")

  footnotes <- rbind(
    c("a", "<span style='color:red';>TODO: Enter DOI of preprint.</span>"),
    c("b", "From Table 1 of the <a href='https://doi.org/10.1016/j.envint.2015.10.015' target='_blank'>Bengtsson-Palme & Larsson (2016)</a> paper.
      The corresponding MIC data are from 2014."),
    c("*", "Estimates were computed from data published in the <a href='https://mic.eucast.org/search/' target='_blank'>EUCAST MIC database</a>     using the algorithm proposed by <a href='https://doi.org/10.1016/j.envint.2015.10.015' target='_blank'>Bengtsson-Palme & Larsson (2016)</a>."),
    c("1", "1% MIC quantile of the most sensitive species supported by a sufficient number of observations."),
    c("2", "Extrapolation is only performed if the original MIC quantile is identical to the lowest tested concentration."),
    c("3", "Estimate obtained after rescaling and rounding. Scaling is only performed if the number of valid species is less than 40."),
    c("4", "The value of 0.004 represents the 5% quantile of the cost associated with plasmid-borne resistance.
      The value of 0.1 was originally termed an 'assessment' factor.")
  )
  footnotes <- paste0("<div style='font-size:smaller;'>","\n",
    paste("<sup>",footnotes[,1],"</sup> ",footnotes[,2],"<br>", collapse="\n"),"\n",
  "</div>","\n")

  html <- paste0(

    page.head(headline=headline, title=d, navi=cbind(navi, menusep=NA, active=FALSE)),
    
    "<h1>Drug meta data</h1>","\n",
    table.static(as.df(meta), colnames=FALSE),"\n\n",
    
    "<h1>Estimates</h1>","\n",
    table.static(info, colnames=TRUE, class="stripedTable"),"\n\n",
    expandableSection(
      footnotes,
      labelIfClosed="[Show footnotes]", labelIfOpen="[Hide footnotes]"
    ),"\n\n",

    "<h1>Graphics</h1>","\n",
    expandableSection(
      paste0(
        embedSVG(file=paste0("../2_analysis_MIC/output/visual_rawMinMIC_",d,".svg"),
          caption="Distribution of original MIC quantiles of the tested organisms
            in comparison to MIC<sub>lowest</sub>."),
        embedSVG(file=paste0("../3_analysis_cost/output/cost_fitted.svg"),
          caption="Distribution of the cost associated with plasmid-borne
            resistance. The mixture distribution model is a weighted sum of a
            Gaussian and an exponential component. The proposed MSC /
            MIC<sub>lowest</sub> conversion factor represents the 5% quantile
            of the exponential component.")
      ),
      labelIfClosed="[Show figure]", labelIfOpen="[Hide figure]"
    ),"\n\n",
    
    page.foot()
  )
  write(html, paste0(odir,"/drug_",d,".html"))
  
}
