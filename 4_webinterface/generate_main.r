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
#x <- x[,c("drug", "drugclass", "drugfamily", "drugcode")]
#names(x) <- c("Drug", "Class", "Sub-class", "Drug code")
x <- x[,c("drug", "drugclass", "drugfamily")]
names(x) <- c("Drug", "Class", "Sub-class")
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
    `Drug name`= d,
    `Drug class`= if (x[d,"drugclass"] != d) x[d,"drugclass"] else "-",
    `Drug code`= paste(x[d,"drugcode"], tooltip("The list of codes is
       not necessarily exhaustive. For drugs with multiple applications,
       preference was given to the code for systemic use. A complete list
       of codes can be obtained from the WHO Collaborating Centre For
       Drug Statistics Methodology (https://atcddd.fhi.no)."))
  )

  # info related to mic_lowest and pnec_res
  
  formatSpeciesList <- function(x) {
    paste0("<span style='font-style:italic'>", gsub(x, pattern=",",
      replacement=",<br>", fixed=T),"</span>")
  }

  warn.current <- is.finite(x[d,"scaling.factor.current"]) && (x[d,"scaling.factor.current"] <= crit_scaling_factor)
  warn.reference <- is.finite(x[d,"scaling.factor.reference"]) && (x[d,"scaling.factor.reference"] <= crit_scaling_factor)
  warningSuperscript <- function(bool) { if (bool) "<sup style='color:red;'>#</sup>" else ""}
  
  cost_quantile <- costs[costs[,"Probability"] == 0.05, "Cost"]
  bold <- function(x) { paste0("<span style='font-weight: bold;'>",x,"</span>") }
  
  info <- rbind(
    data.frame(
      sbj = "<span style='font-style: italic'><br>Information extracted from <a href='https://mic.eucast.org/'>EUCAST MIC data</a></span>",
      new =  "",
      old =  ""
    ),
    data.frame(
      sbj = "Data version (year)",
      new =  version,
      old =  "2014"
    ),
    data.frame(
      sbj = "Distinct organisms tested in total",
      new =  x[d,"organisms.tested"],
      old =  ""
    ),
    data.frame(
      sbj = "Organisms with sufficient data to estimate a robust minimum MIC",
      new =  x[d,"organisms.evaluated"],
      old =  ""
    ),
    data.frame(
      sbj = paste("Robust minimum MIC (mg/L)", tooltip("This is the minimum of
            the smallest MICs observed for individual test organisms. To
            compensate for exceptional uncertainty in reported extremes, the
            underlying per-organism estimates are computed as empirical 1%
            quantiles of the individual MIC distributions. If the 1% quantile is
            supported by less than 10 records, the probability level is shifted
            upwards until the respective quantile is supported by at least 10
            records.")),
      new = x[d,"lowest.MIC.quantile.current"],
      old = x[d,"lowest.MIC.quantile.reference"]
    ),
    data.frame(
      sbj = paste("Most sensitive organism(s)", tooltip("Test organism(s) to
            which the robust minimum MIC refers. These can be multiple
            organisms if their sensitivities are very similar.")),
      new = formatSpeciesList(x[d,"most.sensitive"]),
      old = ""
    ),
    data.frame(
      sbj = paste("Robust minimum MIC after extrapolation (mg/L)",
            tooltip("Extrapolation is only performed if the robust minimum MIC
            is identical to the lowest concentration considered in standard MIC
            assays (0.002 mg/L). This is to compensate for a possible positive
            bias originating from the limitation of the MIC test scale. Only
            very few antibiotics are currently affected.")),
      new = if (x[d,"lowest.MIC.quantile.extrapol.current"] != x[d,"lowest.MIC.quantile.current"])
              x[d,"lowest.MIC.quantile.extrapol.current"] else "extrapolation not performed",
      old = if (is.finite(x[d,"lowest.MIC.quantile.extrapol.reference"]))
            x[d,"lowest.MIC.quantile.extrapol.reference"] else "extrapolation not performed"
    ),
    data.frame(
      sbj = paste("Valid species considered for species coverage adjustment",
            tooltip("Represents the number of unique species names.
            Bengtsson-Palme & Larsson looked up valid names in the SILVA database.
            Kneis et al. accepted all non-ambiguous species names plus the names
            of Salmonella serovars.")),
      new = paste(x[d,"valid.species.current"], warningSuperscript(warn.current)),
      old = paste(x[d,"valid.species.reference"], warningSuperscript(warn.reference))
    ),
    data.frame(
      sbj = paste("Species coverage adjustment factor (-)", tooltip("The
            factor adjusts the (possibly extrapolated) robust minimum MIC to
            account for the number of tested species. The respective factor has
            been estimated by resampling and the value differs slightly between
            the approaches of Kneis et al. and Bengtsson-Palme & Larsson.
            A low number of tested species translates into a small
            factor and thus results in a more pronounced adjustment.")),
      new = paste(x[d,"scaling.factor.current"], warningSuperscript(warn.current)),
      old = paste(x[d,"scaling.factor.reference"], warningSuperscript(warn.reference))
    ),
    data.frame(
      sbj = paste("MIC<sub>lowest</sub> (mg/L)", tooltip("This is the (possibly
            extrapolated) robust minimum MIC after adjustment for species
            coverage. In the Bengtsson-Palme & Larsson approach, numbers were
            subject to rounding.")),
      new = paste(bold(x[d,"lowest.MIC.quantile.extrapol.scaled.rounded.current"]), warningSuperscript(warn.current)),
      old = paste(bold(x[d,"lowest.MIC.quantile.extrapol.scaled.rounded.reference"]), warningSuperscript(warn.reference))
    ),
    data.frame(
      sbj = "<span style='font-style: italic'><br>Derivation of PNEC<sub>res</sub></span>",
      new = "",
      old = ""
    ),
    data.frame(
      sbj = paste("Conversion factor to turn MIC<sub>lowest</sub> into
            PNEC<sub>res</sub> (-)", tooltip(paste("The value of",cost_quantile,
            "proposed by Kneis et al. represents the approximate 5% quantile of
            the cost associated with plasmid-borne resistance. The value of 1/10
            employed by Bengtsson-Palme & Larsson lacks a direct ecological
            interpretation and was originally understood as an assessment factor.
            The conversion factors represent constant defaults which apply
            to all antibiotics."))),
      new = cost_quantile,
      old = 1/10
    ),
    data.frame(
      sbj = paste("PNEC<sub>res</sub> (mg/L)", tooltip("The product of
            MIC<sub>lowest</sub> and the above conversion factor. The product
            represents a minimum estimate of the MSC for a multi-species
            and possibly multi-strain community. It does not refer to a
            particular pair of resistant and susceptible competitors. Note that
            PNEC<sub>res</sub> calculated by Kneis et al. is rooted in a closed
            theory and, therefore, its derivation does not involve a global
            'assessment factor' (contrary to common ecotoxicological approaches).")),
      new = paste(bold(signif(cost_quantile * x[d,"lowest.MIC.quantile.extrapol.scaled.rounded.current"], 2)), warningSuperscript(warn.current)),
      old = paste(bold(x[d,"PNECR"]), warningSuperscript(warn.reference))
    )
  )

  names(info) <- c("", "Kneis et al. (2025) <sup>1</sup>", "Bengtsson-Palme & Larsson (2016) <sup>2</sup>")
  
  footnotes <- rbind(
    c("1", "Available as preprint, <a href='https://doi.org/10.1101/2025.04.04.647007' target='_blank'>
      DOI:10.1101/2025.04.04.647007</a>."),
    c("2", "From Table 1 of Bengtsson-Palme & Larsson (2016), <a href='https://doi.org/10.1016/j.envint.2015.10.015' target='_blank'>
      DOI:10.1016/j.envint.2015.10.015</a>.")
  )
  if (warn.current || warn.reference) {
    footnotes <- rbind(footnotes,
      c(warningSuperscript(TRUE), "MIC data is available for very few species.
        Estimates of MIC<sub>lowest</sub> and PNEC<sub>res</sub> are strongly
        adjusted for species coverage and subject to increased uncertainty."))
  }
  footnotes <- paste0("<div style='font-size:smaller;'>","\n",
    paste("<sup>",footnotes[,1],"</sup>",footnotes[,2], collapse="<br>\n"),"\n",
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
    
    "<h1>Data summary</h1>","\n",
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
    "<h2>Cost-based conversion factors</h2>","\n",
    expandableSection(
        table.static(costs, colnames=TRUE, class="stripedTable",
          caption="Quantiles of the fitness cost attributable to plasmid-borne
          antibiotic resistance. The cost is a dimensionless number in range
          [0,1] expressing the loss of fitness upon acquisition of resistance
          in comparison to a susceptible bacterial isolate. In line with common
          precautionary principles, the 5% quantile of the cost is currently
          employed to convert MIC<sub>lowest</sub> into PNEC<sub>res</sub>.
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
