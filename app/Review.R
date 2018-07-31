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

S$PGN$FR = recGet(S$db, "catalog", c("catalogID", "reviewBest"), tibble(c("dupOf", "=", 0)))
S$PGN$norecs = S$PGN$FR$catalogID[1] == 0
S$PGN$filteredIDs = S$PGN$FR$catalogID
S$PGN$chunkedIDs = chunker(S$PGN$filteredIDs, 30)
S$PGN$pageCount = length(S$PGN$chunkedIDs)
S$PGN$activePage = 1

S$fil$abstract = ""
S$fil$author = ""
S$fil$year = ""
S$fil$journal = ""
S$fil$review_0 = TRUE
S$fil$review_1 = TRUE
S$fil$review_2 = TRUE

# Init reactive variables
rv$render = 0         # Trigger to render page after dealing with input initializatons
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



#if(rv$limn && S$P$Msg=="") {
if(S$P$Msg=="") {
   output$uiMeat <- renderUI({rv$render; isolate({
      switch(as.character(rv$menuActive),
         "1" = {
            r = recGet(S$db, "catalog", c("catalogID", "dupOf", "reviewBest", "reviewCount"), tibble(c("catalogID", ">", 0)))
            if(r$catalogID[1]>0) {                                                         # Prepare data for doughnut charts
               noText <- ""
               aData <- c(sum(r$reviewBest==0), sum(r$reviewBest==1),sum(r$reviewBest==2)) # Have cites, chart 1
               aColors <- c("#1997c6","#9F86FF","#1BC98E")
               cData <- c(sum(r$dupOf==0), sum(r$dupOf!=0))
               cColors <- c("#1997c6","#9F86FF")
               if(sum(r$reviewCount>0)==0) {                                               # Have cites, but no reviews, chart 2
                  bData <- c(1,1,1)
                  bColors <- c("#6c757d","#6c757d","#6c757d")
               } else {                                                                    # Have cites & reviews, chart 2
                  bData <- c(sum(r$reviewCount==1), sum(r$reviewCount==2), sum(r$reviewCount>2))
                  bColors <- c("#1997c6","#9F86FF","#1BC98E")
               }
            } else {                                                                       # No cites, no reviews, both charts
               noText <- "<h5>Nothing to review yet</h5>"
               aData <- c(1,1,1)
               aColors <- c("#6c757d","#6c757d","#6c757d")
               bData <- c(1,1,1)
               bColors <- c("#6c757d","#6c757d","#6c757d")
               cData <- c(1,1)
               cColors <- c("#6c757d","#6c757d")
            }
            restOfPage = tagList(
               uiOutput("dashboard")
            )
            output$dashboard <- renderUI({rv$render; isolate({                             # Here come the doughnut charts
               tagList(
                  bs4("r",
                     bs4("chart", c=4, id=paste0("cC"), labels=c("Unique", "Duplicates"),
                         data=cData,
                         legend="false", zeroText=noText,
                         title1="All Citations", title2="Unique vs</br>Duplicates",
                         colors=cColors
                     ),
                     bs4("chart", c=4, id=paste0("cA"), labels=c("Not Reviewed", "Failed", "Passed"),
                         data=aData,
                         legend="false", zeroText=noText,
                         title1="Stage 1 Reviews", title2="Pass vs Fail vs<br>Not Reviewed",
                         colors=aColors
                     ),
                     bs4("chart", c=4, id=paste0("cB"), labels=c("1 review", "2 reviews", "3 or more"),
                         data=bData,
                         legend="false", zeroText=noText,
                         title1="Stage 1 Fails Only", title2="Number of<br>Reviews",
                         colors=bColors
                     )
                  ),
bs4("r", bs4("c1"), bs4("c10", bs4("cd", q="y", bs4("cdb", bs4("cdt", HTML0(               # The yellow box
"<p>The first graph shows the number of duplicates that have been identified so far in the project's list of citations.
The second graph shows reviewing progress and results.</p>
<p>The third graph - which is for Stage 1 <b>Fails</b> only
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
            if(S$PGN$norecs) {           # Nothing in the catalog yet.
               restOfPage = {
                  tagList(
                     bs4("r",
                        bs4("c12", HTML0("<h5>No citations found</h5>")
                        )
                     )
                     # ),
                     # ybox
                  )
               }
            } else {
               r = recGet(S$db, "catalog", c("catalogID", "title", "author", "journal", "Y", "reviewBest", "reviewCount"),
                     tibble(c("catalogID", " IN ", paste0("(", paste0(S$PGN$chunkedIDs[[S$PGN$activePage]], collapse=","), ")"))))
               if(S$P$Modify) {
                  r$btn = paste0("<button id=cat_", r$catalogID, " class='btn border-dark btn-success'>Review</button>")
               } else {
                  r$btn = paste0("<button id=cat_", r$catalogID, " class='btn border-dark btn-primary'>View</button>")
               }
               restOfPage = tagList(
                  uiOutput("showCatalog")
               )
               output$showCatalog <- renderUI({rv$render; isolate({
                  tagList(
                     bs4("r",
                        bs4("c9",
                           HTML("<span style='font-size: 1.25rem; color:#fff;'>Filter citations</span><br>"),
                           ttextInput("abstract", "Phrase in title or abstract", value=S$fil$abstract, groupClass="w-75"),
                           bs4("r",
                              bs4("c5",
                                 ttextInput("author", "An author", value=S$fil$author, groupClass="w-100")),
                              bs4("c2",
                                 ttextInput("year", "Year", value=S$fil$year, groupClass="w-100")),
                              bs4("c5",
                                 ttextInput("journal", "Journal", value=S$fil$journal, groupClass="w-100"))
                           )
                        ),
                        bs4("c1"),
                        bs4("c2",
                           HTML("Review status<br>"),
                           HTML(ifelse(S$fil$review_0, bs4("cbx", id="review_0", q="ck", "Not reviewed"), bs4("cbx", id="review_0", "Not reviewed"))),
                           HTML(ifelse(S$fil$review_1, bs4("cbx", id="review_1", q="ck", "Stage 1 Fail"), bs4("cbx", id="review_1", "Stage 1 Fail"))),
                           HTML(ifelse(S$fil$review_2, bs4("cbx", id="review_2", q="ck", "Stage 1 Pass"), bs4("cbx", id="review_2", "Stage 1 Pass"))),
                           bs4("btn", id="filBTN_0", q="b", class="ml-4 mt-4", "Filter"))
                     ),
                     if(S$PGN$filteredIDs[1]>0) {
                        tagList(
                           bs4("c12", bs4("hr0", class="pb-4")),
                           bs4("r",
                              bs4("c12", class="mb-2",
                                 HTML0("<span style='font-size: 1.15rem; color:#fff;'>",
                                    format(length(S$PGN$FR$reviewBest), big.mark = ","), " results; ",
                                    format(sum(S$PGN$FR$reviewBest==0), big.mark = ","), " not reviewed; ",
                                    format(sum(S$PGN$FR$reviewBest==1), big.mark = ","), " failed; ",
                                    format(sum(S$PGN$FR$reviewBest==2), big.mark = ","), " passed</span><br>")
                           )),
                           bs4("pgn", np=S$PGN$pageCount, ap=S$PGN$activePage),
                           bs4("c12", bs4("hr0", class="pb-4")),
                           bs4("r",
                              bs4("c12",
HTML(paste0("<div class='row justify-content-center'><div class='col-11'>",
       "<b>Review Status:</b> ", ifelse(r$reviewBest==0, "Not reviewed",
         ifelse(r$reviewBest==1, "Stage 1 Fail", "Stage 1 Pass")),
       " <b>Number of reviews:</b> ", r$reviewCount, "<br>",
       "<span style='font-size: 1.25rem; color:#fff;'>", r$title, "</span><br>",
       "<b>By: </b>", r$author, "<br>",
       "<b>Year:</b> ", r$Y, " <b>Journal:</b> ", r$journal,  "<br>",
       "</div>",
       "<div class='col-1'>", r$btn,
       "</div>", bs4("c12", bs4("hr")), "</div>", collapse = ""))
                           )),
                           bs4("pgn", np=S$PGN$pageCount, ap=S$PGN$activePage)
                        )
                     } else {
                        HTML("<h5>Nothing found</h5>")
                     }
                  )
               })})
            }
         })
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
         rv$menuActive = as.numeric(n)
         rv$render = rv$render+1
      },
      "pgn" = {
         S$PGN$activePage <<- as.numeric(n)
         rv$render = rv$render+1
      },
      "filBTN" = {
         S$fil$abstract <<- stripHTML(input$abstract)
         S$fil$author   <<- stripHTML(input$author)
         S$fil$year     <<- stripHTML(input$year)
         S$fil$journal  <<- stripHTML(input$journal)
         S$fil$review_0 <<- input$review_0
         S$fil$review_1 <<- input$review_1
         S$fil$review_2 <<- input$review_2
         rs = sum(c(S$fil$review_0, S$fil$review_1, S$fil$review_2))
         if(rs==0) {
            S$modal_title <<- "Nothing found"
            S$modal_text <<- HTML0("<p>You must check one or more of the <i>Review status</i> checkboxes. Unchecking them ",
                                   "all will return no results.</p>")
            S$modal_size <<- "m"
            rv$modal_warning <- rv$modal_warning + 1
         } else {
            WHERE = tibble(dupOf=c("dupOf", "=", 0))
            if(rs==1) {
               if(S$fil$review_0) { WHERE$rBest = c("reviewBest", "=", 0) }
               if(S$fil$review_1) { WHERE$rBest = c("reviewBest", "=", 1) }
               if(S$fil$review_2) { WHERE$rBest = c("reviewBest", "=", 2) }
            }
            if(rs==2) {
               if(S$fil$review_0 && S$fil$review_1) { WHERE$rBest = c("reviewBest", "!=", 2) }
               if(S$fil$review_0 && S$fil$review_2) { WHERE$rBest = c("reviewBest", "!=", 1) }
               if(S$fil$review_1 && S$fil$review_2) { WHERE$rBest = c("reviewBest", "!=", 0) }
            }
            if(S$fil$author!="") { WHERE$author=c("author", "LIKE", paste0("%", S$fil$author, "%"))}
            if(S$fil$year!="") { WHERE$year=c("Y", "LIKE", paste0("%", S$fil$year, "%"))}
            if(S$fil$journal!="") { WHERE$journal=c("journal", "LIKE", paste0("%", S$fil$journal, "%"))}
            dbLink <- poolCheckout(shiny.pool)                                        # get a dbLink from the pool
            wherePairs = wherez(WHERE, dbLink)
            if(S$fil$abstract!="") {                                                  # all this for an OR
               qabs = dbQuoteString(dbLink, paste0("%", S$fil$abstract, "%"))
               wherePairs = paste0(wherePairs, " AND (`abstract` LIKE ", qabs, " OR `title` LIKE ", qabs, ")")
            }
            selects = dbQuoteIdentifier(dbLink, c("catalogID", "reviewBest"))
            selects = paste0(selects, collapse=",")
            QUERY = paste0("SELECT ", selects, " FROM ", dbt(S$db, "catalog", dbLink), " WHERE ", wherePairs, ";")
            S$PGN$FR <<- dbGetQuery(dbLink, QUERY)                                    # perform raw SQL Query
            poolReturn(dbLink)                                                        # return dbLink
            S$PGN$filteredIDs <<- S$PGN$FR$catalogID
            S$PGN$chunkedIDs <<- chunker(S$PGN$filteredIDs, 30)
            S$PGN$pageCount <<- length(S$PGN$chunkedIDs)
            S$PGN$activePage <<- 1
            rv$render = rv$render+1
         }
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)


wherezzz = function(WHERE, dbLink) {   # WHERE is a tibble
   w = ""
   for(i in 1:ncol(WHERE)) {                                  # Need all this to keep from putting quotes on numbers
      if(WHERE[[2,1]]!=" IN " && suppressWarnings(is.na(as.numeric(WHERE[[3,i]])))) { # An NA means it's a string
         v = dbQuoteString(dbLink, WHERE[[3,i]])
      } else {
         v = WHERE[[3,i]]
      }
      w[i] = paste0(dbQuoteIdentifier(dbLink, WHERE[[1,i]]), WHERE[[2,i]], v)
   }
   return(paste0(w, collapse = " AND "))
}


