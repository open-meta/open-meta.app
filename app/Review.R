### open-meta.app Review.R
### Tom Weishaar - placeholder

# When we arrive here:
#   * S$U is a 1x5 tibble with userID, userName, email, sPowers, and emailOK
#   * S$PRJ is a 1x5 tibble with projectID, projectName, status, and privacy PLUS "userRole" field from membership table
#   * S$PG is a 1x2 tibble with pageName (stripped of ? and .R) and spReq
source("chokidar.R", local=TRUE)
# When we leave here:
#   * S$P has permissions in it
#   * output$uiHead has been rendered with any necessary error messages and "hrm" menus
#   * if(S$P$Msg=="") the user has permission to see the page and you can render the Meat
#   * S$P$Modify  - whether the user has permission to modify things on this page

# Session Globals for this page
S$REV <- list()
S$WHERE <- tibble(c("catalogID", ">", 0))  # default WHERE

S$PGN$start <- '<nav aria-label="Citation list pagination"><ul class="pagination"><li class="page-item"><a id="p_0" class="page-link">First Page</a></li>'
S$PGN$end <- '<li class="page-item"><a id="p_999" class="page-link">Last Page</a></li></ul></nav>'

# Init reactive variables
rv$render = 0         # Trigger to render page after dealing with input initializatons

# Only needed on this page:
# catalogGet = function(db=S$db, SELECT="", WHERE=S$WHERE) {
#    return(recGet(db, "catalog", SELECT, WHERE))
# }

rv$menuActive = 1    # Start out on first sub-menu
#####

# observeEvent(c(rv$menuActive, rv$limn), {
#    if(rv$menuActive==1) {
#       if(S$P$Modify) {
#          S$REV$pageTitle <<- "Review your catalog of citations"
#       } else {
#          S$REV$pageTitle <<- "View catalog of citations"
#       }
#    }
#    rv$render = rv$render + 1                     # Not editing, just render
# })


S$PGN$F <- recGet(S$db, "catalog", c("catalogID", "reviewBest"), tibble(c("catalogID", "<", 10)))


pages = c(5:9)
activePage = 7
S$PGN$middle <- ""
for(p in pages) {
   S$PGN$middle <- paste0(S$PGN$middle, ifelse(p==activePage, '<li class="page-item active"><a id="', '<li class="page-item"><a id=p_"'),
                          p, '" class="page-link">', p, '</a></li>')
}

#if(rv$limn && S$P$Msg=="") {
if(S$P$Msg=="") {
   r = recGet(S$db, "catalog", c("catalogID", "title", "author", "journal", "Y", "reviewBest", "reviewCount"), tibble(c("catalogID", "<", 10)))
   if(S$P$Modify) {
      r$btn = paste0("<button id=cat_", r$catalogID, " class='btn border-dark btn-success'>Review</button>")
   } else {
      r$btn = paste0("<button id=cat_", r$catalogID, " class='btn border-dark btn-primary'>View</button>")
   }
   output$uiMeat <- renderUI({rv$render; isolate({
      switch(as.character(rv$menuActive),
         "1" = {
            r = recGet(S$db, "catalog", c("catalogID", "reviewBest", "reviewCount"), tibble(c("catalogID", ">", 0)))
            if(r$catalogID>0) {                                                            # Prepare data for doughnut charts
               noText <- ""
               aData <- c(sum(r$reviewBest==0), sum(r$reviewBest==1),sum(r$reviewBest==2))   # Have cites, chart 1
               aColors <- c("#1997c6","#9F86FF","#1BC98E")
               if(sum(r$reviewCount>0)==0) {                                           # Have cites, but no reviews, chart 2
                  bData <- c(1,1,1)
                  bColors <- c("#6c757d","#6c757d","#6c757d")
               } else {                                                                # Have cites & reviews, chart 2
                  bData <- c(sum(r$reviewCount==1), sum(r$reviewCount==2), sum(r$reviewCount>2))
                  bColors <- c("#1997c6","#9F86FF","#1BC98E")
               }
            } else {                                                                   # No cites, no reviews, both charts
               noText <- "<h5>Nothing to review yet</h5>"
               aData <- c(1,1,1)
               aColors <- c("#6c757d","#6c757d","#6c757d")
               bData <- c(1,1,1)
               bColors <- c("#6c757d","#6c757d","#6c757d")
            }
            restOfPage = tagList(
               uiOutput("dashboard")
            )
            output$dashboard <- renderUI({rv$render; isolate({                         # Here come the doughnut charts
               tagList(
                  bs4("r",
                     bs4("chart", c=6, id=paste0("cA"), labels=c("Not Reviewed", "Failed", "Passed"),
                         data=aData,
                         legend="false", zeroText=noText,
                         title1="Stage 1 Reviews", title2="Pass vs Fail vs<br>Not Reviewed",
                         colors=aColors
                     ),
                     bs4("chart", c=6, id=paste0("cB"), labels=c("1 review", "2 reviews", "3 or more"),
                         data=bData,
                         legend="false", zeroText=noText,
                         title1="Fails Only", title2="Number of<br>Reviews",
                         colors=bColors
                     )
                  ),
bs4("r", bs4("c1"), bs4("c10", bs4("cd", q="y", bs4("cdb", bs4("cdt", HTML0(
"<p>The first graph shows reviewing progress and results. The second graph - which is for Stage 1 <b>Fails</b> only
(the article does not meet project criteria) - divides the Fails into groups by how many times they've been reviewed.
This graph shows <i>Fails</i> only because for <i>Not Reviewed</i> the number is zero and for <i>Stage 1 Pass</i>
the number of reviews doesn't matter - any article passed by one reviewer moves on to Stage 2 Review.</p>
<p>Hover over the doughnuts to see the actual number of article citations in each category. A gray graph means that
no data is available for that graph at this time (no searches have been processed or no reviews have been completed).</p>
"))))))
)
})})
         },
         "2" = {
            restOfPage = tagList(
               uiOutput("showCatalog")
            )
            output$showCatalog <- renderUI({rv$render; isolate({
               tagList(
                  bs4("r",
                     bs4("c9",
                        HTML("<span style='font-size: 1.25rem; color:#fff;'>Filter citations</span><br>"),
                        ttextInput("tabFilter", "Phrase in title or abstract", value="", groupClass="w-75"),
                        bs4("r",
                           bs4("c5",
                              ttextInput("aFilter", "Author", value="", groupClass="w-100")),
                           bs4("c2",
                              ttextInput("YFilter", "Year", value="", groupClass="w-100")),
                           bs4("c5",
                              ttextInput("jFilter", "Journal", value="", groupClass="w-100"))
                        )
                     ),
                     bs4("c1"),
                     bs4("c2",
                        HTML("Review status<br>"),
                        bs4("cbx", id="rFilter_0", q="ck", "Not reviewed"),
                        bs4("cbx", id="rFilter_1", q="ck", "Stage 1 Fail"),
                        bs4("cbx", id="rFilter_2", q="ck", "Stage 1 Pass"),
                        bs4("btn", id="Filter_0", q="b", class="ml-4 mt-4", "Filter"))
                  ),
                  bs4("c12", bs4("hr0", class="pb-4")),
                  bs4("r",
                     bs4("c12", class="mb-2",
                        HTML0("<span style='font-size: 1.25rem; color:#fff;'>", length(S$PGN$F$reviewBest), " results; ",
                           sum(S$PGN$F$reviewBest==0), " not reviewed; ",
                           sum(S$PGN$F$reviewBest==1), " failed; ",
                           sum(S$PGN$F$reviewBest==2), " passed</span><br>")
                  )),
                  bs4("r",
                     bs4("c12", HTML(S$PGN$start, S$PGN$middle, S$PGN$end),
                     bs4("c12", bs4("hr0", class="pb-4"))
                  )),
                  bs4("r",
                     bs4("c12",
                         HTML(
paste0("<div class='row justify-content-center'><div class='col-11'>",
       "<b>Review Status:</b> ", ifelse(r$reviewBest==0, "Not reviewed",
         ifelse(r$reviewBest==1, "Stage 1 Fail", "Stage 1 Pass")),
       " <b>Number of reviews:</b> ", r$reviewCount, "<br>",
       "<span style='font-size: 1.25rem; color:#fff;'>", r$title, "</span><br>",
       "<b>By: </b>", r$author, "<br>",
       "<b>Year:</b> ", r$Y, " <b>Journal:</b> ", r$journal,  "<br>",
       "</div>",
       "<div class='col-1'>", r$btn,
       "</div>", bs4("c12", bs4("hr")), "</div>", collapse = "")
                         )
                  )),
                  bs4("r",
                     bs4("c12", HTML(S$PGN$start, S$PGN$middle, S$PGN$end)
                  ))
               )
            })})
         }
      )
   reviewPageMenu = {
      if(S$hideMenus) {
         ""
      } else {
         bs4("md", id="sub", n=1:2, active=rv$menuActive, text=c("Dashboard", "Citation List"))
      }
   }
   return(tagList(
      bs4("r", align="hc",
         bs4("c10", tagList(
            reviewPageMenu,
            restOfPage
         ))
      )
   ))
   })})
}

### observer for omclick
observeEvent(input$js.omclick, {
   if(debugON) {
      cat(paste0("Click on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "sub" = {
         rv$menuActive = n
         rv$render = rv$render+1
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)


