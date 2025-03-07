
styles <- function() {"
  html body {font-family: sans-serif;}
  h1 {font-size: 1.5em;}
  h2 {font-size: 1.2em;}
  h3 {font-size: 1.0em;}
  p {max-width: 40em; text-align: justify;}
/*  .headline { padding: 4px 8px 4px 15px; background-color: #CEDFE4; display:block;} */
  .headline { padding: 4px 8px 4px 15px; background-color: #E2EDF0; display:block;}
  .topnav {
    display: flex;
    flex-flow: row wrap;
    justify-content: flex-start;
    align-content: space-around;
    background-color: #E2EDF0;
    padding: 4px 4px 4px 15px;
  }
  .topnavopened {
    background-color: #42548B; color: #FFFFFF;
    font-size: 1em; line-height: 2em;
    padding: 4px; margin-right: 4px;
    border-radius: 5px;
    border-top: 2px solid #E5E5E5;
    border-left: 2px solid #E5E5E5;
    border-bottom: 2px solid #7F7F7F;
    border-right: 2px solid #7F7F7F;
    text-decoration: none;
    white-space: nowrap;
  }
  .topnavclosed {
    color: #42548B; background-color: #FFFFFF;
    font-size: 1em; line-height: 2em;
    padding: 4px; margin-right: 4px;
    border-radius: 5px;
    border-top: 2px solid #E5E5E5;
    border-left: 2px solid #E5E5E5;
    border-bottom: 2px solid #7F7F7F;
    border-right: 2px solid #7F7F7F;
    text-decoration: none;
    white-space: nowrap;
  }

  a { color: #415EB5; } 

   table {border-collapse: collapse;}
   th, td {
    padding-top: 2px;
    padding-bottom: 2px;
    padding-left: 5px;
    padding-right: 5px;  
   }
  
  .stripedTable {
    thead {font-weight: bold; background-color: #f2f2f2;}
    tr:nth-child(even) {background-color: #f2f2f2;}  
  }
  
  div.text { max-width: 600px; }

  img {
    max-width: 100%;
    height: auto;
  }  
  figcaption {
    font-style: italic;
  }
  caption {
    font-style: italic;
    text-align: justify;
    padding-bottom: 8px;
  }

  /* source: https://dev.to/rouilj/how-to-change-details-label-when-open-closed-without-javascript-1n3c */
  /* https://www.sitepoint.com/style-html-details-element/#creatingacustommarkerforthesummaryelement */
  /* details summary { padding: 0px 0px 0px 0px } */
  details { padding: 4px 4px 4px 4px; }
  /* if details are hidden */
  details summary span.clickToHide {display:none; color:#415EB5}
  details summary span.clickToShow {color:#415EB5}
  /* if details are shown (open) */
  details[open] summary span.clickToHide {display:inline}
  details[open] summary span.clickToShow {display:none}
  details summary::marker {
    display: none;
    content: '';
  }
  
  .pagecontainer {
    display: flex;
    flex-flow: column;  
    width: 100%;
    height: 100%;
  }
  .pagetop {
    display: flex;
    flex-flow: column;
    position: sticky;
    z-index: 1;
    top: 0;
    left: 0;
    width: 100%;
  }
  .pagemain {
    display: flex;
    flex-flow: column;
    width: 95wv;
    margin-left: 10px;
    margin-top: 5px;
    padding: 10px 10px;
    overflow: auto;
  }
"}
  
page.head <- function(headline, title, navi) {
  paste0('
  <!DOCTYPE html>
  <html lang="en">
  <head>
    <title>',title,'</title>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />
    <style>',
    styles(),
    '</style>
  </head>
  <body>
  <div class="pagecontainer">  
    <div class="pagetop">
      <div class="headline">',headline,
      '</div>
      <div class="topnav">
        <nav>',
          paste(paste0(
          sapply(navi[,"menusep"], function(n) {paste(rep("&nbsp;", n), collapse="")}),
          "<a href='", navi[,"targetfile"],
          "' class='",ifelse(navi[,"active"], "topnavopened", "topnavclosed"),"'>",
          navi[,"menulabel"], "</a>"), collapse=" \n"),
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

pages.create <- function(headline, navi, odir) {
  for (i in 1:nrow(navi)) {
    html <- paste0(
      page.head(
        headline=headline,
        title=navi[i,"pagetitle"],
        navi=cbind(navi[,c("menulabel","menusep","targetfile")], active=(1:nrow(navi))==i)
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
