  
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

embedSVG <- function(file, caption) {
  paste0(   
    "<figure>","\n",
    paste(readLines(file), collapse="\n"),"\n",
    "<figcaption>",caption,"</figcaption>",
    "</figure>"
  )
}
