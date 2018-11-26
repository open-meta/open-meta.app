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

# load the inputMeta.R code (also used by other pages)
source("inputMeta.R", local=TRUE)
source("citationFiltering.R", local=TRUE)

# Session Globals for this page
S$REV <- list()
S$WHERE <- tibble(c("catalogID", ">", 0))  # default WHERE

# Pagination globals
S$PKR <- list()
S$PKR$itemsPerPage <- 30
# S$PKR$FR = recGet(S$db, "catalog", c("catalogID", "reviewBest"), tibble(c("dupOf", "=", 0)))
# S$PKR$norecs = S$PKR$FR$catalogID[1] == 0
# S$PKR$filteredIDs = S$PKR$FR$catalogID
# S$PKR$chunkedIDs = chunker(S$PKR$filteredIDs, S$PKR$itemsPerPage)
# S$PKR$pageCount = length(S$PKR$chunkedIDs)
S$PKR$activePage = 1
S$PKR$pointer = 1

rv$limnCites <- 0
S$PRK$Cites$activePage <- 1

# Init reactive variables
rv$render = 0        # Trigger to render page after dealing with input initializatons
rv$menuActive = 1    # Start out on first sub-menu
#####

if(S$P$Msg=="") {
   output$uiMeat <- renderUI({rv$render; isolate({
      reviewWidgets = ""                           # In returned tagList, but not in all pages!
      switch(as.character(rv$menuActive),
         "1" = {                                   # 1 is the Dashboard
            cites = recGet(S$db, "catalog", c("catalogID", "dupOf", "reviewBest", "reviewCount"), tibble(c("catalogID", ">", 0)))
            if(cites$catalogID[1]>0) {                                                     # Prepare data for doughnut charts
               noText <- ""
               aData <- c(sum(cites$dupOf==0), sum(cites$dupOf!=0))
               aColors <- c("#1997c6","#9F86FF")
               cites = cites[cites$dupOf==0,]
               bData <- c(sum(cites$reviewBest==0), sum(cites$reviewBest==1),sum(cites$reviewBest==2)) # Have cites, chart 1
               bColors <- c("#1997c6","#9F86FF","#1BC98E")
               if(sum(cites$reviewCount>0)==0) {                                           # Have cites, but no reviews, chart 2
                  cData <- c(1,1,1)
                  cColors <- c("#6c757d","#6c757d","#6c757d")
               } else {                                                                    # Have cites & reviews, chart 2
                  cData <- c(sum(cites$reviewCount==1), sum(cites$reviewCount==2), sum(cites$reviewCount>2))
                  cColors <- c("#1997c6","#9F86FF","#1BC98E")
               }
            } else {                                                                       # No cites, no reviews, both charts
               noText <- "<h5>Nothing to review yet</h5>"
               bData <- c(1,1,1)
               bColors <- c("#6c757d","#6c757d","#6c757d")
               cData <- c(1,1,1)
               cColors <- c("#6c757d","#6c757d","#6c757d")
               aData <- c(1,1)
               aColors <- c("#6c757d","#6c757d")
            }
            restOfPage = tagList(
               bs4("r",
                  bs4("chart", c=4, id=paste0("cC"), labels=c("Unique", "Duplicates"),
                      data=aData,
                      legend="false", zeroText=noText,
                      title1="All Citations", title2="Unique vs</br>Duplicates",
                      colors=aColors
                  ),
                  bs4("chart", c=4, id=paste0("cA"), labels=c("Not Reviewed", "Failed", "Passed"),
                      data=bData,
                      legend="false", zeroText=noText,
                      title1="Stage 1 Reviews", title2="Pass vs Fail vs<br>Not Reviewed",
                      colors=bColors
                  ),
                  bs4("chart", c=4, id=paste0("cB"), labels=c("1 review", "2 reviews", "3 or more"),
                      data=cData,
                      legend="false", zeroText=noText,
                      title1="Stage 1 Fails Only", title2="Number of<br>Reviews",
                      colors=cColors
                  )
               ),
bs4("r", bs4("c1"), bs4("c10", bs4("cd", q="y", bs4("cdb", bs4("cdt", HTML0(               # The yellow box
"<p>The first graph shows the number of duplicates that have been identified so far in the project's list of citations.
The second graph shows reviewing progress and results.</p>
<p>The third graph - which is for Stage 1 <b>Fails</b> only
(the cited article does not meet project criteria) - divides the Fails into groups by how many times they've been reviewed.
This graph shows <i>Fails</i> only because for <i>Not Reviewed</i> the number is zero and for <i>Stage 1 Pass</i>
the number of reviews doesn't matter - any article passed by one reviewer moves on to Stage 2 Review.</p>
<p>Hover over the doughnuts to see the actual number of article citations in each category. A gray graph means that
no data is available for that graph at this time (no searches have been processed or no reviews have been completed).</p>
"))))))
)
         },
         "2" = {                         # 2 is the Duplicates page
            restOfPage = (HTML("<h5>More to come...</h5>"))
         },
         "3" = {                         # 3 is the list of citations
            S$PKR$pointer <<- 1          # Always reset to beginning of list
            return(tagList(
               bs4("d", id="citationPickR")
            ))
         },
         "4" = {                                      # 4 is review a citation
            S$REV$cite2 <<- recGet(S$db, "catalog", "**", tibble(c("catalogID", "=", S$REV$id)))
            if(S$P$Modify) {
   # Reviewers see this:
               S$REV$rev2 <<- recGet(S$db, "review", "**", tibble(c("catalogID", "=", S$REV$id), c("verUser", "=", S$U$userName)))
               fbn = recGet(S$db, "settings", "value", tibble(c("name", "=", "failBoxNames")))
               S$REV$failBoxes <<- fromJSON(fbn$value)
               fbIDs = paste0("cbx", 1:length(S$REV$failBoxes))
               if(S$REV$rev2$reviewID[2]==0) {
                  editText = ""                       # This is for the box around the Fail/Pass buttons
                  editColor = ""                      #   when reviewing a citation that the user has already
                  fbChecked = rep(FALSE, length(S$REV$failBoxes))
               } else {                               #   reviewed.
                  editText = "Edit your previous review"
                  editColor = "y"
                  fbChecked = S$REV$failBoxes %in% fromJSON(S$REV$rev2$detail[2])
               }
               if(length(S$REV$failBoxes)==0) {
                  chexArea = ""
               } else {
                  chexArea <- tagList(
                     HTML0("<p><b>Reasons for failure to meet project criteria:</b>&nbsp;&nbsp;&nbsp;"),
                     bs4("btn", id="raison", q=c("y", "xs"), "?"),
                     HTML0("</p>"),
                     bs4("r", bs4("c12", style="column-count: 3;",
                        bs4("cbx", id=fbIDs, ck=fbChecked, il=FALSE, S$REV$failBoxes))
                     )
                  )
               }
               S$REV$buttonRow <<- tagList(
                  bs4("r", class="my-3",
                     bs4("c3", align="sb", bs4("btn", id="prev", q="b", class="mb-2", "<- Skip")),
                     bs4("c6", align="sb", q=editColor,
                        bs4("r",
                           bs4("c12", class="text-center", HTML0('<b>', editText, '</b>'))),
                        bs4("r",
                           bs4("c1"),
                           bs4("c5", class="text-left",  bs4("btn", id="fail", q="r", class="mb-2", "Fail")),
                           bs4("c5", class="text-right", bs4("btn", id="pass", q="g", class="mb-2", "Pass")),
                           bs4("c1")
                        )
                     ),
                     bs4("c3", align="sb", class="text-right", bs4("btn", id="next", q="b", class="mb-2", "Skip ->"))
                  )
               )
               reviewWidgets = tagList(
                  bs4("r",
                     bs4("c5", HTML0("<p><b>Comments on this review:</b></p>"),
                        bs4("cmt")),
                     bs4("c7", chexArea, S$REV$buttonRow))
               )
            } else {
   # Browsers see this
               S$REV$buttonRow <<- tagList(
                  bs4("r", class="my-3",
                     bs4("c3", align="sb", bs4("btn", id="prev", q="b", class="mb-2", "<- Skip")),
                     bs4("c6", align="sb"),
                     bs4("c3", align="sb", class="text-right", bs4("btn", id="next", q="b", class="mb-2", "Skip ->"))
                  )
               )
               reviewWidgets <- bs4("r", bs4("c5"), bs4("c7", S$REV$buttonRow)) # Also need buttonRow variable for bottom of page
   ###
            }
            restOfPage = {
               tagList(
                  bs4("r",
                     bs4("c12", HTML0("<h5>", S$REV$cite2$title[2],"</h5>")),
                     bs4("c12", HTML0("<b>By:</b> ", S$REV$cite2$author[2],"<br>",
                                      "<b>Year:</b> ", S$REV$cite2$Y[2], " ",
                                      "<b>Journal:</b> ", S$REV$cite2$journal[2], "<br>",
                                      "<b>Full text links:</b> ",
                                      ifelse(S$REV$cite2$pmid[2]=="", "",
                                         paste0('<a href="https://www.ncbi.nlm.nih.gov/pubmed/', S$REV$cite2$pmid[2],
                                                '" target="_blank">PMID</a>; ')),
                                      ifelse(S$REV$cite2$pmcid[2]=="", "",
                                         paste0('<a href="https://www.ncbi.nlm.nih.gov/pmc/articles/', S$REV$cite2$pmcid[2],
                                                '" target="_blank">PMCID</a>; ')),
                                      ifelse(S$REV$cite2$doi[2]=="", "",
                                         paste0('<a href="https://doi.org/', S$REV$cite2$doi[2],
                                                '" target="_blank">DOI</a>; ')),
                                      paste0('<a href="https://scholar.google.com/scholar?q=',
                                             urlEncodePath(S$REV$cite2$title[2]),
                                             '" target="_blank">Google Scholar</a><br>'))),
                     bs4("c12", bs4("hr")),
                     bs4("c12", HTML0(S$REV$cite2$abstract[2]))
                  ),
                  bs4("r", bs4("c5"), bs4("c7", S$REV$buttonRow)),
                  bs4("r", bs4("c12", HTML0("<p><b>Comments on this citation:</b></p>"), bs4("cmt")))
               )
            }
         })
   reviewPageMenu = {
      if(S$hideMenus) {
         ""
      } else {
         bs4("md", id="sub", n=1:3, active=rv$menuActive, text=c("Dashboard", "Duplicates", "Citation List"))
      }
   }
   return(tagList(
      bs4("r", align="hc",
         bs4("c10", tagList(
            reviewPageMenu,
            reviewWidgets,
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
      "pgn" = {                    # For pgn, [2] or n is the form name, [3] is the recID
         S$PKR[[n]]$activePage <<- as.numeric(uid[[1]][3])
         limnID = paste0("limn", n)         # The pickR render should respond to this rv$limn...;
         rv[[limnID]] <- rv[[limnID]] + 1    # rv$limn... also needs to be pre-defined at the top of the script
      },
      "sub" = {
         rv$menuActive = as.numeric(n)
         rv$render = rv$render+1
      },
      "filter" = {
         rv$render = rv$render+1
      },
      "cite" = {
         S$REV$id <<- as.integer(n)
         rv$menuActive = 4
         rv$render = rv$render+1
      },
      "next" = {
         nextCite()
      },
      "prev" = {
         prevCite()
      },
      "fail" = {
         chex <- getChex()
         forceFail2 <- recGet(S$db, "settings", "value", tibble(c("name", "=", "forceFail")))
         if(forceFail2$value=="T" && length(S$REV$failBoxes)>0 && !any(chex)) {
            S$modal_title <<- "Review conflict"
            S$modal_text <<- HTML0("<p>If your review is <b>Fail</b>, at least one reason for failure checkbox must be checked.</p>")
            S$modal_size <<- "s"
            rv$modal_warning <- rv$modal_warning + 1
         } else {
            saveReview(1, chex)
            nextCite()
         }
      },
      "pass" = {
         chex <- getChex()
         forcePass2 <- recGet(S$db, "settings", "value", tibble(c("name", "=", "forcePass")))
         if(forcePass2$value=="T" && any(chex)) {
            S$modal_title <<- "Review conflict"
            S$modal_text <<- HTML0("<p>If your review is <b>Pass</b>, all reason for failure checkboxes must be unchecked.</p>")
            S$modal_size <<- "s"
            rv$modal_warning <- rv$modal_warning + 1
         } else {
            saveReview(2, chex)
            nextCite()
         }
      },
      "raison" = {
         S$modal_title <<- "All about <i>reasons for failure</i>"
         S$modal_text <<- HTML0("<p>Your project's PI can add to  the list of reasons for failure until there are 20 reasons.
Existing reasons can also be edited <i>before they've been used</i> (changing them after they've been used would create a hot
mess). It's also possible to blank out all of the reasons from the start and just review for Pass and Fail without recording
a reason.</p>
<p>Make sure you understand whether your project's protocol is to look for <b>any</b> reason for failure, to check that box,
and to move on, or to thoroughly examine the title and abstract to find <b>all</b> the reasons the citation doesn't meet
your project's criteria. That takes a lot longer. Checking the first reason you see and moving on is fast and good for
documenting your choice but doesn't create meaningful aggregate data on the reasons for failure. That kind of data may be
useful for some projects but often is not worth the extra effort.</p>
<p>The Open-Meta app allows you to change your own review of a citation as many times as you like. You will see a very
obvious visual cue to let you know when you are working on a citation you've already reviewed earlier. The <b>Skip</b>
buttons let you come back to a citation later. Citations come up in the same order as in the citation list on the previous
page. If you filter that list to just a few citations, those are the only citations you will see in your current reviewing
episode.</p>")
         S$modal_size <<- "l"
         rv$modal_warning <- rv$modal_warning + 1
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

prevCite = function() {
   S$PKR$pointer <<- S$PKR$pointer - 1
   if(S$PKR$pointer<1) {
      rv$menuActive = 3
   } else {
      S$REV$id <<- S$PKR$filteredIDs[S$PKR$pointer]
   }
   rv$render = rv$render+1
}

nextCite = function() {
   S$PKR$pointer <<- S$PKR$pointer + 1
   if(S$PKR$pointer>length(S$PKR$filteredIDs)) {
      rv$menuActive = 3
   } else {
      S$REV$id <<- S$PKR$filteredIDs[S$PKR$pointer]
   }
   rv$render = rv$render+1
}

saveReview = function(decision, chex) {
   if(any(chex)) {
      S$REV$rev2$detail[2] <<- toJSON(S$REV$failBoxes[which(chex)])
   } else {
      S$REV$rev2$detail[2] <<- toJSON("")                                         # Nothing is checked
   }
   S$REV$rev2$catalogID[2] <<- S$REV$cite2$catalogID[2]
   S$REV$rev2$decision[2] <<- decision
   S$REV$rev2$comment[2] <<- ""
   S$REV$rev2 <<- recSave(S$REV$rev2, S$db)
   S$REV$cite2$reviewCount[2] <<- S$REV$cite2$reviewCount[2] + 1
   S$REV$cite2$reviewBest[2] <<- max(decision, S$REV$cite2$reviewBest[2])         # 0=not reviewed, 1=fail, 2=pass,
   S$REV$cite2 <<- recSave(S$REV$cite2, S$db)                                     #   3=fail stage 2; 4=pass stage 2
}

getChex = function() {
   if(length(S$REV$failBoxes)==0) {
      return(FALSE)                                                               # Nothing exists so nothing is checked
   } else {
      return(c(input$cbx1, input$cbx2, input$cbx3, input$cbx4, input$cbx5,
               input$cbx6, input$cbx7, input$cbx8, input$cbx9, input$cbx10,
               input$cbx11, input$cbx12, input$cbx13, input$cbx14, input$cbx15,
               input$cbx16, input$cbx17, input$cbx18, input$cbx19, input$cbx20))  # Any NULLs chop themselves off the vector
   }
}


output$citationPickR <- renderUI({c(rv$limn, rv$render, rv[["limnCites"]]); isolate({
   # Note: pickR runs a filter that saves form values in FORM, thus we need to run it first, then render the FORM
   ID = "Cites"                                                            # Fix rv[["limn"+ID]] buzzer
   TABLE = "catalog"
   WHERE = tibble(c("catalogID", ">", "0"))
   FilterF = citesFilter                                                   # typically whereFilter
   HeadlineF = citesHead                                                   # typically THRUb
   SELECT = c("title", "author", "journal", "Y", "reviewBest", "reviewCount")
   if(S$P$Modify) {                                                        # Review or View depends on permissions
     ButtonData <- list(review=list(id="cite", q="g", class="mr-2", label="Review"))
   } else {
     ButtonData <- list(view=list(id="cite", q="b", label="View"))
   }
   ButtonF = stdButtons                                                    # typically stdButtons
   FixDataF = THRU                                                         # THRU for no changes
   FormatF = prf_cites                                                     # search files for "prf_" to see choices
   NOtext = "This filter didn't find anything."
   activePage = ifelse(is.null(S$PKR[[ID]]$activePage), 1, S$PKR[[ID]]$activePage)
   itemsPerPage = S$PKR$itemsPerPage                                       # Modifiable pickR-by-pickR
   scroll = FALSE                                                          # Modifiable pickR-by-pickR
   results <- pickR(ID, S$db, TABLE, WHERE, FilterF, HeadlineF, SELECT, ButtonData, ButtonF,
                    FixDataF, FormatF, NOtext, activePage, itemsPerPage, scroll)

   ids=c("notRev", "s1Fail", "s1Pass")
   ck=c(S$FIL$notRev, S$FIL$s1Fail, S$FIL$s1Pass)
   cbxNames = c("Not Reviewed",  "Stage 1 Fail", "Stage 1 Pass")
   restOfPage <- tagList(
      bs4("r", align="hc", bs4("c10",
         bs4("r",
            bs4("c8",
               HTML("<span style='font-size: 1.25rem; color:#fff;'>Filter citations</span><br>"),
               imForm2HTML(S$FIL$FORM)
            ),
            bs4("c4", class="pl-5",
               HTML("Review status<br>"),
               bs4("d", class="pl-1", radioButtons("allRnot", "",
                                                   c("All Reviews" = "all", "My Reviews" = "my"),
                                                   selected=S$FIL$allRnot)),
               bs4("cbx", id=ids, ck=ck, cbxNames),
               bs4("btn", uid="filter_0", q="b", class="ml-4 mt-4", "Filter")
            ),
            results
         )
      ))
   )
})})

prf_cites = function(r) {
# In this particular example, there's one row with a col-11 containing all the data, using <br> to start new
#    lines, and col-1 for the button. Note that cites$btn isn't stored in MySQL, but is added to "cites" above.
   return(paste0(
'<div class="row">
   <div class="col-11">
      <b>Review Status:</b> ',
         ifelse(r$reviewBest==0, 'Not reviewed', ifelse(r$reviewBest==1, 'Stage 1 Fail', 'Stage 1 Pass')), '
         <b>Number of reviews:</b> ', r$reviewCount, '<br>
      <span style="font-size: 1.25rem; color:#fff;">', r$title, '</span><br>
      <b>By: </b>', r$author, '<br>
      <b>Year:</b> ', r$Y, ' <b>Journal:</b> ', r$journal,  '<br>
   </div>
   <div class="col-1">', r$Action, '</div>',
   bs4('c12', bs4('hr')), '
</div>', collapse = ''))
}


