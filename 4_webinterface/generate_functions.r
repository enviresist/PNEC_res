  
page.head <- function(headline, title, navi, links, cssfile) {
  paste0('
  <!DOCTYPE html>
  <html lang="en">
  <head>
    <title>',title,'</title>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <link rel="stylesheet" type="text/css" href="',cssfile,'">
  </head>
  <body>
  <div class="pagecontainer">  
    <div class="pagetop">
      <div class="headline">',headline,
      '</div>
      <div class="topnav">
        <nav>',
          paste(paste0(
          "<a href='", navi[,"targetfile"],
          "' class='",ifelse(navi[,"active"],"topnavActive","topnavInactive"),"'>",
          navi[,"menulabel"], "</a>"), collapse=" \n"),
          paste(rep("&nbsp;", 10), collapse=""),
          "<a href='",links["accessibility"],"' target='_blank' class='topnavLink'>Accessibility</a>",
          "<a href='",links["legalnotice"],"' target='_blank' class='topnavLink'>Legal notice</a>",
        '</nav>
      </div>
    </div> <!-- end of pagetop div -->
    <div class="pagemain">
  ')
}

page.foot <- function() {
  paste0('
  </div> <!-- end of pagemain div -->
  </div> <!-- end of pagecontainer div -->
  </body>
  </html>
  ')
}

# navi must be a data frame with columns:
# - contents: html code representing the body contents below the menu
# - label: used for menu label and page title
# - href:  generated html file

pages.create <- function(headline, navi, links, cssfile, odir) {
  for (i in 1:nrow(navi)) {
    html <- paste0(
      page.head(
        headline=headline,
        title=navi[i,"pagetitle"],
        navi=cbind(navi[,c("menulabel","targetfile")], active=(1:nrow(navi))==i),
        links=links,
        cssfile=cssfile
      ),
      navi[i,"htmlcontents"],
      page.foot()
    )
    write(html, file=paste0(odir,"/",navi[i,"targetfile"]))
  }
}

table.static <- function(df, colnames=TRUE, id=NULL, class=NULL, caption="") {
  fn <- function(i, x) {
    paste0("<tr><td>", paste0(x[i,], collapse="</td><td>"), "</td></tr>")
  }
  paste0(
    "<table ",if (!is.null(id)) paste0("id='",id,"'"),
      if (!is.null(class)) paste0(" class='",class,"'"),">\n",
    "<caption>",caption,"</caption>","\n",
    if (colnames) {
      paste0("  <thead>\n",
        paste0("    <tr><td>", paste0(colnames(df), collapse="</td><td>"), "</td></tr>\n"),
        "  </thead>\n"
      )
    },
    "  <tbody>\n",
      paste0(sapply(1:nrow(df), fn, x=df), collapse="\n"), "\n",
    "  </tbody>\n",
    "</table>"
  )
}

table.interactive <- function(df, id="myTable", caption="") {
  # NOTE: We do not use the "scrollX" and "scrollY" options of
  #       data table because of a known issue (table header shifted
  #       relative to contents). We wrap the datatable in a "div" with
  #       scrolling enabled instead.
  paste0(
    "<link href='https://cdn.datatables.net/v/dt/dt-2.0.2/datatables.min.css' rel='stylesheet'>
    <script src='https://code.jquery.com/jquery-3.7.1.js'></script>
    <script src='https://cdn.datatables.net/v/dt/dt-2.0.2/datatables.min.js'></script>
    <script>
      $(document).ready( function () {
        $('#",id,"').dataTable( {
          pageLength: 10,
          lengthMenu: [5, 10, 20, 100]
        } );
      } );
    </script>",
    "\n",
    table.static(df, id=id, class="display compact", caption=caption),
    "\n"
  )
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

tooltip <- function(x) {
 paste0("<span class='tooltip'> &#128712;",
  "<span class='tooltiptext'>",x,"</span></span>")  
}

embedSVG <- function(file, caption) {
  paste0(   
    "<figure>","\n",
    paste(readLines(file), collapse="\n"),"\n",
    "<figcaption>",caption,"</figcaption>",
    "</figure>"
  )
}

plot_MIC_quantiles <- function(
  drug,  # drug name
  q,     # vector of MIC quantiles for tested organisms
  e      # final estimate of MIC_lowest
) {
  q <- round(log2(q[is.finite(q)]))
  e <- round(log2(e))
  rng <- range(c(q, e, -9, 9))
  classes <- min(rng):max(rng)
  if (length(q) > 0) {
    stopifnot(all(q %in% classes))
    x <- sapply(classes, function(x, qtl) { sum(qtl == x) }, qtl=q)
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
      xlab="", ylab="Cases")
    mtext(side=1, line=4, paste0(drug, " (mg/L)"))
    axis(side=1, at=z, labels=signif(2^classes,3), line=-0.5, lwd=0, las=2)
    axis(side=2, at=c(0, 1, 2, 5, 10, 20, 20, 40, 50, 75, 100), las=2)
    abline(v=z[match(e, classes)], col="red")
    par(mar=omar)
    layout(1)
  } else {
    plot(0, 0, type="n", axes=FALSE, ann=FALSE)
    legend("center", legend="no finite data for plotting")
  }
}
