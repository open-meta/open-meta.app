### open-meta.app Search.R
### Tom Weishaar - Apr 2018 - v0.1

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

# Single place to define these.
databases = c("PubMed", "Cochrane Central", "Web of Science", "All Other")
citeFormats = list(c("Live", "PMID", "MEDLINE or .nbib"), "Cochrane Central Export", "EndNote Desktop (.ciw)", c("RIS", "BibTeX", "MEDLINE or .nbib", "PMID"))
S$SRCH$id = 0         # Initialize for new search; edit will update
rv$processFile = 0    # Trigger to begin processFile observer
rv$hitCounter = 0     # Trigger to update "Number of citations in this search" readonly field
rv$render = 0         # Trigger to render page after dealing with input initializatons
S$PF$trigger = 1      # Which section we're processing now (used with invalidateLater())
S$uploadMaxCites <- as.numeric(settingsGet(c("value","comment"), tibble(c("name", "=", "uploadMaxCites")))$value)

S$PM = list()         # Variables needed for PubMed Search
S$PM$lastTime = now()
S$PM$search <- FALSE
S$PM$cascade <- "a"
S$PM$url <-""
S$PM$Chunksize = 250  # How many we get per fetch
S$PM$Chunks = 1       # Number of fetches; multiple fetches allow a better progress bar and experience, although
S$PM$Chunk = 1
S$PM$Medline = ""     #    it might be slightly slower because of the pause between requests
S$PM$progress
rv$PMsearch <- 0

S$SRCH$saveFlag <- FALSE
S$SRCH$processFlag <- FALSE
S$msg = ""

# Only needed on this page:
searchGet = function(db=S$db, SELECT="", WHERE=tibble(c("searchID", ">", "0"))) {
   return(recGet(db, "search", SELECT, WHERE))
}

rv$menuActive = 1     # Start out on first sub-menu

# Need this to make choices stick without changing other inputs
# Not sure how to keep this from running twice when changing the database as that also
#    changes citeFormat. But need both triggers here or things don't work right.
observeEvent(c(rv$menuActive, rv$limn, input$database, input$citeFormat), {
   if(rv$menuActive==2) {                           # Bypass all this if we're not editing a Search
      if(S$SRCH$justArrived) {                      # first time through
         S$SRCH$justArrived <<- FALSE
         if(S$P$Modify && S$SRCH$id==0) {           # Get a new record to save this search in
            S$SRCH$pageTitle <<- "Enter your search details"
            S$SRCH2 <<- searchGet()                 # These become the initial values displayed on a new search
            S$SRCH2$searchName[2] <<- ""
            S$SRCH2$database[2]   <<- "PubMed"
            S$SRCH2$otherDB[2]    <<- ""
            S$SRCH2$beginDate[2]  <<- "1000-01-01"
            S$SRCH2$endDate[2]    <<- sTime()
            S$SRCH2$terms[2]      <<- ""
            S$SRCH2$query[2]      <<- ""
            S$SRCH2$CFchosen[2]   <<- "Live"
            S$SRCH2$citeCount[2]  <<- 0
            S$SRCH2$fileName[2]   <<- ""
            S$SRCH2$comment[2]    <<- ""
            S$SRCH2$createDate[2] <<- sTime()
         } else {                                   # Get the record to view or edit
            S$SRCH2 <<- searchGet(SELECT="**", WHERE=tibble(c("searchID", "=", S$SRCH$id)))
            if(S$P$Modify && S$SRCH2$status[2]==0) {
               S$SRCH$pageTitle <<- "Edit your search details"
            } else {
               S$SRCH$pageTitle <<- "View search details"
            }
         }
      } else {                                   # NOT first time through; use current inputs
         return(js$getEdit("terms"))             # Goto next observeEvent() to collect those inputs
      }
   }
   rv$render = rv$render + 1                     # Not editing, just render
})

# fills S$SRCH2$... fields with current inputs; used by
#    PubMed Search (S$PM$search=TRUE)
#    Render        (S$PM$search=FALSE, S$SRCH$saveFlag=FALSE, S$SRCH$processFlag=FALSE)
#    Save          (S$PM$search=FALSE, S$SRCH$saveFlag=TRUE,  S$SRCH$processFlag=FALSE)
#    Process       (S$PM$search=FALSE, S$SRCH$saveFlag=TRUE,  S$SRCH$processFlag=TRUE)
# To call this use:
   # S$SRCH$saveFlag <<- TRUE    (Any flags not set to TRUE can be assumed to be FALSE)
   # js$getEdit("terms")
observeEvent(input$js.editorText, {
   id = input$js.editorText[1]         # The quill id
   t = input$js.editorText[2]          # The edited text
   switch(id,
      "terms" = {
         S$SRCH2$terms[2] <<- stripHTML(t)
         if(S$PM$search) {                  # We're about to search PubMed
            S$PM$search <<- FALSE
            S$SRCH2$beginDate[2]  <<- stripHTML(as.character(input$searchDates[1]))  # Need these for the search
            S$SRCH2$endDate[2]    <<- stripHTML(as.character(input$searchDates[2]))
            S$PM$cascade <<- "a"            # Start observer at first section of the cascade
            rv$PMsearch <- rv$PMsearch + 1  # Run the PMsearch observer next
            return()                        # that's all we need here
         } else {
            if(S$SRCH2$database[2]=="PubMed" && S$SRCH2$CFchosen[2]=="Live") { # If this search is PubMed Live
               return(js$getEdit("comment"))                                   #   we got the Query from PubMed;
            } else {                                                           #   don't overwrite it.
               return(js$getEdit("query"))
            }
         }
      },
      "query" = {
         S$SRCH2$query[2] <<- t
         return(js$getEdit("comment"))
      },
      "comment" = {
         S$SRCH2$comment[2] <<- t
      },
      message(paste0("In input$js.editorText observer, no handler for ", id, "."))
   )
   S$SRCH2$searchName[2] <<- stripHTML(str_sub(input$searchName, 1, 254))    # VARCHAR(254) in SQL
   S$SRCH2$database[2]   <<- stripHTML(input$database)
   if(!is.null(input$otherDB)) {
      S$SRCH2$otherDB[2] <<- stripHTML(str_sub(input$otherDB, 1, 60))        # VARCHAR(60) in SQL
   }
   S$SRCH2$CFchosen[2]   <<- stripHTML(input$citeFormat)
   S$SRCH2$beginDate[2]  <<- stripHTML(as.character(input$searchDates[1]))
   S$SRCH2$endDate[2]    <<- stripHTML(as.character(input$searchDates[2]))
   # $status filled in by "Check Search"
   # $terms  handled above
   # $query  handled above
   # comment handled above
   # $CFactual  filled in by citeFile
   # $citeCount filled in by citeFile
   # $fileName  filled in by citeFile
   # $fileSize  filled in by citeFile
   # $fileType  filled in by citeFile
   # $fileTime  filled in by citeFile
   if(S$SRCH$saveFlag) {                                # Can't save a search if any of these are bad.
      S$SRCH$saveFlag <<- FALSE
      msg=""
      if(S$SRCH2$database[2]=="All Other" && S$SRCH2$otherDB[2]=="") {
         msg = paste0(msg, "<li>Your Database Name can't be blank.</li>")
      }
      if(S$SRCH2$searchName[2]=="") {
         msg = paste0(msg, "<li>Your Search Name can't be blank.</li>")
      }
      if(is.na(input$searchDates[1])) {
         msg = paste0(msg, "<li>Initial Publication Date can't be blank.</li>")
      }
      if(is.na(input$searchDates[2])) {
         msg = paste0(msg, "<li>Final Publication Date can't be blank.</li>")
      }
      if(!any(is.na(input$searchDates)) && !(input$searchDates[1] < input$searchDates[2])) {
         msg = paste0(msg, "<li>Final Publication Date must be after Initial Publication Date.</li>")
      }
      if(S$SRCH$processFlag) {                          # A search can be saved if these are bad, but not processed.
         if(stripHTML(S$SRCH2$query[2])=="") {
            msg = paste0(msg, "<li>Your Query can't be blank.</li>")
         }
         if(S$SRCH2$CFchosen[2]=="Live") {              # PubMed
            if(S$SRCH2$citeCount[2]==0) {               # PubMed, no cites
               msg = paste0(msg, "<li>There are no citations to process. You need to modify your terms or dates ",
                                 "and search PubMed again.</li>")
            }
            if(S$SRCH2$citeCount[2]>S$uploadMaxCites) { # PubMed, too many cites
               msg = paste0(msg, "<li>Your search has over ", format(S$uploadMaxCites, big.mark=","), " citations, ",
                                 "which is too many to process. You need to modify your terms or dates and search ",
                                 "PubMed again. If you really want to review over ", format(S$uploadMaxCites, big.mark=","),
                                 " citations, create multiple searches, splitting them up by publication date ",
                                 "into smaller chunks.</li>")
            }
         } else {                                       # Not PubMed
            if(S$SRCH2$citeCount[2]>S$uploadMaxCites) { # Other, too many cites
               msg = paste0(msg, "<li>Your search has over ", format(S$uploadMaxCites, big.mark=","), " citations, ",
                                 "which is too many to process. You need to modify your terms or dates and upload ",
                                 "a smaller file. If you really want to review over ",
                                 format(S$uploadMaxCites, big.mark=","), " citations, split the citations up by ",
                                 "publication date into smaller chunks and create multiple searches.</li>")
            }
            if(S$SRCH2$citeCount[2]==0) {               # Other, no cites
               if(S$SRCH2$CFactual[2]=="") {            # Other, no file so no cites
                  msg = paste0(msg, "<li>There are no citations to process. You need to upload a citation file.</li>")
               } else {                                 # Other, file but no cites
                  msg = paste0(msg, "<li>There are no citations to process in the file you've uploaded.</li>")
               }
            } else {                              # We have a file, check its validity
               if(S$SRCH2$CFchosen[2]=="BAD" || S$SRCH2$CFchosen[2]=="BLANK") {
                     msg = paste0(msg, "<li>The file you uploaded isn't a valid citation file.</li>")
               } else {                           # Do chosen and actual formats match?
                  if(S$SRCH2$CFchosen[2]!="Live" && S$SRCH2$CFchosen[2]!=S$SRCH2$CFactual[2]) {
                     msg = paste0(msg, "<li>The file format you specified, ", S$SRCH2$CFchosen[2], " doesn't match the format ",
                                       "of the file you uploaded, which is ", S$SRCH2$CFactual[2], ".</li>")
                  }
               }
            }
         }
      }
      if(nchar(msg)) {                    # No render if there's a modal; put this on top of existing render
         S$modal_title <<- "Whoops!"
         if(S$SRCH$processFlag) {
            S$SRCH$processFlag <<- FALSE
            S$modal_text <<- HTML("<p>Can't process:<ul>", msg, "</ul></p>")
         } else {
            S$modal_text <<- HTML("<p>Can't save:<ul>", msg, "</ul></p>")
         }
         rv$modal_warning <- rv$modal_warning + 1
      } else {
         if(S$SRCH$processFlag) {
            S$SRCH$processFlag <<- FALSE
            processSearch()               # This is elsewhere to simplify code for this observer
         }
         S$SRCH2 <<- recSave(S$SRCH2, db=S$db)  # Save the search
         rv$menuActive = 1
         S$hideMenus <<- FALSE            # Back to regular programming
         rv$limn = rv$limn + 1            # Need to re-limn here to get menus back, but skip for modal dialog
      }
   } else {                               # Not a save or process, just a render
      rv$render = rv$render + 1           # If not a save or process, just proceed to next step of rendering the page
   }
})

#if(rv$limn && S$P$Msg=="") {
if(S$P$Msg=="") {
   output$uiMeat <- renderUI({rv$render; isolate({
      switch(as.character(rv$menuActive),
         "1" = {                          # Search List
            restOfPage = tagList(
               DTOutput("showSearches")
            )
            # NOTE: If the user has permission to Modify searches, there will be two columns of buttons.
            #    The first column is Edit when Search's status=0, View otherwise.
            #    The second column is Delete when Search's status=0, Update if status=1, Blank otherwise
            # If the user doesn't have Modify permission, there will be one column with View buttons only
            output$showSearches  <- renderDT({
               Rx = omRx(                 # Get project's searches
                  db = S$db,
                  table = "search",
                  SELECT = c("searchID", "searchName", "beginDate", "endDate", "citeCount", "status"),
                  WHERE = tibble(c("deleted", "=", 0)),
                  buttons = list(view=list(id="viewSearch", label="View", q="b", class=""),
                                 update=list(id="updateSearch", label="Update", q="i", class=""),
                                 edit=list(id="editSearch", label="Edit", q="g", class=""),
                                 delete=list(id="deleteAsk", label="Delete", q="r", class=""))
               )
               # Adjust which buttons will show in table
               if(nrow(Rx)>0) {                     # Skip this if nothing was returned.
                  if(!S$P$Modify) {                    # Without permission to modify all you can do is View
                     Rx = Rx[-c(8:10)]                 #    delete all button columns but View
                  } else {                                                  # User can modify Search
                     Rx[[7]] <- ifelse(Rx[["status"]]==0, Rx[[9]], Rx[[7]]) # Incomplete: Edit-Delete
                     Rx[[8]] <- ifelse(Rx[["status"]]==0, Rx[[10]],         # Complete: View-Update (default)
                                    ifelse(Rx[["status"]]==2, "", Rx[[8]])) # Updated: View-Blank
                     Rx = Rx[-c(9,10)]              # In either case, get rid of the final two button columns
                  }
               }
               Rx$citeCount <- format(Rx$citeCount, big.mark=",")
               ###
               Rx <- Rx[,-1]              # Remove ID column
               Rx$status = ifelse(Rx$status==0, "Incomplete",
                              ifelse(Rx$status==1, "Completed", "Updated"))
               omDT(Rx,
                  if(S$P$Modify) {
                     cnames = c("Search Name", "Begin Date", "End Date", "Cites", "Status", "View", "Action")
                  } else {
                     cnames = c("Search Name", "Begin Date", "End Date", "Cites", "Status", "View")
                  },
                  colesc = c(1:5),        # columns to escape (minus means don't escape)
                  noText = "No searches found for this project"      # What to say when there are no results
               )
            },
            # renderDT() parameters go here:
            server = FALSE
            )
         },
         "2" = {                          # View or Edit or New Search: S$SRCH2 has info to display in all cases
            if(S$P$Modify && S$SRCH2$status[2]==0) {             # Edit (or New)
               databaseFields = {
                  if(S$SRCH2$database[2]!="All Other") {      # Not "All Other"; no otherDB field
                     addOtherDB = tagList(
                        bs4('d', class='figure',
                           selectInput('database', 'Database', choices=databases,
                           selected=S$SRCH2$database[2], selectize=FALSE)))
                  } else {
                     addOtherDB = tagList(                    # "All Other"; change formatting, add field
                        bs4("d",                              # different class puts dropdown above text input
                           selectInput('database', 'Database', choices=databases,
                           selected=S$SRCH2$database[2], selectize=FALSE)),            # add text input-vvv
                        bs4("d", class="figure", ttextInput("otherDB", "Database Name", value=S$SRCH2$otherDB[2])))
                  }
                  n = which(databases==S$SRCH2$database[2])   # get index into databases to determine which
                  tagList(                                    #   file formats to display
                     addOtherDB,                              # add stuff from above
                     bs4("d", class="figure ml-1",
                        selectInput('citeFormat', 'File Format', choices=citeFormats[[n]],
                           selected=S$SRCH2$CFchosen[2], selectize=FALSE)),
                     bs4("d", class="figure ml-1", bs4("btn", id="details", q=c("p", "s", "y"), "Format Details"))
                  )
               }
               restOfPage =tagList(
                  bs4("r", align="hc",
                     bs4("c10", tagList(
                        h4(S$SRCH$pageTitle),
                        databaseFields,
                        ttextInput("searchName", "Search Name", value=S$SRCH2$searchName[2], groupClass="w-75"),
                        dateRangeInput("searchDates", label="Publication date range of this search",
                                 start=S$SRCH2$beginDate[2], end=S$SRCH2$endDate[2]),
                        if(S$SRCH2$CFchosen[2]=="Live") {
                           tagList(
                              HTML("Terms"),
                              bs4("quill", id="terms", paste0("<p>", S$SRCH2$terms[2], "</p>")),   # terms[2] has been through stripHTML()
                              bs4("d", bs4("btn", class="mb-3", id="searchpubmed", q=c("p", "s", "b"), "Search PubMed")),
                              if(S$SRCH2$query[2]!="") {
                                 HTML("PubMed translated these Publication Dates and Terms into the following Query",
                                      "<p><i>", S$SRCH2$query[2], "</i></p>")
                              }
                           )
                        } else {
                           if(S$SRCH2$database[2]!="All Other") {
                              thisDB <- paste0(" from ", stripHTML(input$database))
                           } else {
                              thisDB <- stripHTML(input$otherDB)
                              thisDB <- ifelse(thisDB=="", "", paste0(" from ", thisDB))
                           }
                           tagList(
                              HTML("Query"),
                              bs4("quill", id="query", S$SRCH2$query[2]),
                              fileInput("citeFile",
                                 paste0("Select the ", stripHTML(input$citeFormat) , " file you downloaded", thisDB),
                                 width="75%",
                                 placeholder = ifelse(S$SRCH2$fileName[2]=="", "No file selected", S$SRCH2$fileName[2])
                              )
                           )
                        },
                        #citeCount
                        bs4("r", align="hc",
                           bs4("c3",
                              HTML("Citations<br>",
                              "<input id='citeCount', type='text' class='form-control w-100' value='",
                              format(S$SRCH2$citeCount[2], big.mark=","), "' readonly='readonly'>")
                           ),
                           bs4("c3",
                              HTML("<a id='pmid-doi-info_1' style='color:yellow; cursor:pointer;'>Abstracts</a><br>",
                              "<input id='absCount', type='text' class='form-control w-100' value='",
                              S$SRCH2$absCount[2], "' readonly='readonly'>")
                           ),
                           bs4("c3",
                              HTML("<a id='pmid-doi-info_2' style='color:yellow; cursor:pointer;'>PMIDs</a><br>",
                              "<input id='pmidCount', type='text' class='form-control w-100' value='",
                              S$SRCH2$pmidCount[2], "' readonly='readonly'>")
                           ),
                           bs4("c3",
                              HTML("<a id='pmid-doi-info_3' style='color:yellow; cursor:pointer;'>DOIs</a><br>",
                              "<input id='doiCount', type='text' class='form-control w-100' value='",
                              S$SRCH2$doiCount[2], "' readonly='readonly'>")
                           )
                        ),
                        HTML("<div class='mt-3'>Comments</div>"),
                        bs4("quill", id="comment", S$SRCH2$comment[2]),
                        HTML('<div class="text-right mt-3">'),
                        bs4("btn", id="cancel", n=1, q="b", "Cancel"),
                        bs4("btn", id="save", n=1, q="b", "Save Search"),
                        bs4("btn", id="processCheck", n=1, q=ifelse(S$SRCH2$status[2]==0, "r", "g"), "Process Search"),
                        HTML('</div>')
                  )))
               )
               rv$hitCounter = rv$hitCounter + 1 # To display filename and number of hits on Edit load
            } else {
               #### View ONLY
               if(S$hideMenus) {          # Menus are hidden: at this point that means the user clicked View
                  restOfPage =tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                           h4(S$SRCH$pageTitle),
                           # Database and File Format
                           bs4("r", align="hc",
                              bs4("c6",
                                 HTML("Database<br>",
                                      "<input type='text', value='",
                                      ifelse(S$SRCH2$database[2]!="All Other", S$SRCH2$database[2], S$SRCH2$otherDB[2]),
                                      "', class='form-control figure w-100', readonly='readonly'><br>")
                              ),
                              bs4("c6",
                                  HTML("File Format<br>",
                                  "<input type='text', value='", S$SRCH2$CFchosen[2],
                                  "', class='form-control figure w-100', readonly='readonly'><br>")
                              )
                           ),
                           # Search Name
                           HTML("Search Name<br>",
                                "<input type='text', value='", S$SRCH2$searchName[2],
                                "'class='form-control w-100', readonly='readonly'><br>"),
                           # Pub date range
                           HTML("Publication date range of this search<br>",
                                "<input type='text', value='", S$SRCH2$beginDate[2],
                                "'class='d-inline form-control w-25', readonly='readonly'",
                                ">&nbsp;to&nbsp;<input type='text', value='", S$SRCH2$endDate[2],
                                "'class='d-inline form-control w-25', readonly='readonly'>"),
                           # PubMed Terms & Query
                           if(S$SRCH2$CFchosen[2]=="Live") {
                              tagList(
                                 HTML("<br><hr><p>Terms</p><p><i>", S$SRCH2$terms[2], "</i></p>"),
                                 if(S$SRCH2$query[2]!="") {
                                    HTML("<hr><p>PubMed translated these Publication Dates and Terms into the following Query</p>", "<p><i>",
                                         S$SRCH2$query[2], "</i></p><hr>")
                                 }
                              )
                           } else {
                           # Not PubMed Query & File Name
                              tagList(
                                 HTML("<br><hr><p>Query</p>"),
                                 HTML("<p><i>", S$SRCH2$query[2], "</i></p><hr>",
                                 "Citation file<br>",
                                 "<input type='text', value='",
                                 S$SRCH2$fileName[2],
                                 "'class='form-control w-100', readonly='readonly'><br>")
                              )
                           },
                           #citeCount
                           bs4("r", align="hc",
                              bs4("c3",
                                 HTML("Citations<br>",
                                 "<input type='text' class='form-control w-100' value='",
                                 format(S$SRCH2$citeCount[2], big.mark=","), "' readonly='readonly'>")
                              ),
                              bs4("c3",
                                 HTML("<a id='pmid-doi-info_4' style='color:yellow; cursor:pointer;'>Abstracts</a><br>",
                                 "<input type='text' class='form-control w-100' value='",
                                 S$SRCH2$absCount[2], "' readonly='readonly'>")
                              ),
                              bs4("c3",
                                 HTML("<a id='pmid-doi-info_5' style='color:yellow; cursor:pointer;'>PMIDs</a><br>",
                                 "<input type='text' class='form-control w-100' value='",
                                 S$SRCH2$pmidCount[2], "' readonly='readonly'>")
                              ),
                              bs4("c3",
                                 HTML("<a id='pmid-doi-info_6' style='color:yellow; cursor:pointer;'>DOIs</a><br>",
                                 "<input type='text' class='form-control w-100' value='",
                                 S$SRCH2$doiCount[2], "' readonly='readonly'>")
                              )
                           ),
                           # Cancel Button
                           HTML('<div class="text-right mt-3">'),
                           bs4("btn", id="cancel", n=1, q="b", "Cancel"),
                           HTML('</div>')
                     )))
                  rv$hitCounter = rv$hitCounter + 1 # To display filename and number of hits on Edit load
               } else {                   # Menus not hidden, at this point that means user clicked New Search w/o adequate permissions
                  if(S$P$O) {
                     restOfPage = bs4("r", bs4("ca", class="mt-3 text-center",
                        h5("Your project role doesn't include adding new searches.")
                     ))
                  } else {
                     restOfPage = bs4("r", bs4("ca", class="mt-3 text-center",
                        h5("Only project members with special roles can add or edit searches.")
                     ))
                  }
               }
            }
         },
         "3" = {                          # Search Analysis
            if(S$P$O) {
               restOfPage = bs4("r", bs4("ca", "Search Analysis to come..."))
            } else {
               restOfPage = bs4("r", bs4("ca", class="mt-3 text-center",
                  h5("You must be a member of the project to see the search analysis.")
               ))
            }
         }
      )
   searchPageMenu = {
      if(S$hideMenus) {
         ""
      } else {
         bs4("md", id="sub", n=1:3, active=rv$menuActive, text=c("Search List", "New Search", "Search Analysis"))
      }
   }
   return(tagList(
      bs4("r", align="hc",
         bs4("c10", tagList(
            searchPageMenu,
            restOfPage
         ))
      )
   ))
   })})
}

# This observer watches for text longer than the database definition on VARCHAR fields
observeEvent(c(input$searchName, input$otherDB), {
      msg=""
      if(!is.null(input$searchName) && nchar(input$searchName)>254) {
         msg = "The Search Name is limited to 254 characters."
      }
      if(!is.null(input$otherDB) && nchar(input$otherDB)>60) {
         msg = "The Database Name is limited to 60 characters."
      }
      if(nchar(msg)) {
         S$modal_title <<- "Whoops!"
         S$modal_text <<- HTML("<p>", msg, "</p>")
         rv$modal_warning <- rv$modal_warning + 1
      }
})

# Run this observer primarily to update the "Number of citations in this search" readonly text input
#    Also updates the filename readonly input to accomodate search editing
observeEvent(rv$hitCounter, {
   if(!is.null(input$database) && !is.null(S$SRCH2$citeCount[2]) && S$SRCH2$citeCount[2]>0) {
      updateTextInput(session, inputId="citeCount", value=format(S$SRCH2$citeCount[2], big.mark=","))  # Add commas
      updateTextInput(session, inputId="absCount", value=S$SRCH2$absCount[2])
      updateTextInput(session, inputId="pmidCount", value=S$SRCH2$pmidCount[2])
      updateTextInput(session, inputId="doiCount", value=S$SRCH2$doiCount[2])
   } else {
      updateTextInput(session, inputId="citeCount", value="")
      updateTextInput(session, inputId="absCount", value="")
      updateTextInput(session, inputId="pmidCount", value="")
      updateTextInput(session, inputId="doiCount", value="")
   }
    updateTextInput(session, inputId="citeFile", value=S$SRCH2$fileName[2])     # Update file name in any case
})

# This observer runs when a file upload has completed.
# It captures the file and its metadata, does an initial scan to determine
#    whether the file is in a valid format and how many cites in has.
# Then it does a final scan based on the actual file format and adds the
#    citation data to a citeN MySQL table, where N is the searchID. This
#    table will later be processed to add its data to the project's main citation table.
observeEvent(input$citeFile, {
   start.time <- Sys.time()
   on.exit({
      cat("### File upload ###\n")
      cat(paste0("File: ", S$SRCH2$fileName[2]), "\n")
      cat(paste0("Type: ", S$SRCH2$fileType[2]), "\n")
      cat(paste0("Size: ", S$SRCH2$fileSize[2]), "\n")
      cat(paste0("Cites: ", S$SRCH2$citeCount[2]), "\n")
      print(Sys.time() - start.time)
   })
   S$SRCH2$fileName[2] <<- stripHTML(input$citeFile$name)
   S$SRCH2$fileSize[2] <<- input$citeFile$size
   S$SRCH2$fileType[2] <<- input$citeFile$type
   S$SRCH2$fileTime[2] <<- sTime()
   msg <- ""                                                   # Assume no error
   fileOK <- TRUE                                              #   likewise
   mismatch = TRUE                                             # Assume an error
   # First check file's extension (But does this work on Macs???)
   fnParts = str_split(S$SRCH2$fileName[2], "[.]")
   fileExt = fnParts[[1]][[length(fnParts[[1]])]]
   if(!(str_to_lower(fileExt) %in% c("txt", "ciw", "ris", "bib", "nbib"))) {
      msg = paste0(msg, "<li>This file's extension is <b>.", fileExt, "</b>. That's not a file type Open-Meta can import.</li>")
      S$SRCH2$CFactual[2] <<- fileExt
   } else {
      S$SRCH2$CFactual[2] <<- "BAD"                            # Assume an error
      S$SRCH2$citeCount[2] <<- 0                               # Initialize
      # Read file as a string vector, one line of file per cell, then delete blank lines
      # Can still choke on odd files with nulls, like gifs.
      # Output of stri_read_lines is always UTF-8
      raw <- stri_read_lines(input$citeFile$datapath,          # "experimental" but seems to fix encoding issues
                             locale = as.character(NA),        # NA here forces fallback_encoding if not UTF-8
                             encoding="auto",                  # "auto" means use stri_detect2 to figure out encoding,
                             fallback_encoding="windows-1252") # if fail (not UTF), assume it's windows-1252 (Win7 US default)
      raw <- raw[str_trim(raw)!=""]                            # raw is character vector; trim and delete blank lines
      # This section figures out the file format
      if(length(raw)>0 && all(!is.na(suppressWarnings(as.numeric(raw))))) {  # PMID is all numeric but could be only 1 line!
         S$SRCH2$CFactual[2] <<- "PMID"
         S$SRCH2$citeCount[2] <<- length(raw)
         if(input$citeFormat=="PMID") mismatch = FALSE
      } else {                                                 # Otherwise needs to have at least 2 lines
         if(length(raw)<6) {                                   #    to test for CIW, but none of the remaining
            S$SRCH2$CFactual[2] <<- "BLANK"                    #    formats are less than 5 lines, probably more...
            mismatch = FALSE                                   # TRUE sends the wrong message
         } else {
            r <- str_sub(raw,1,6)                              # Cut the strings back to first 6 characters
            TF.vec = r=="PMID- "                               #    only for the remaining tests.
            if(any(TF.vec)) {
               S$SRCH2$CFactual[2] <<- "MEDLINE or .nbib"      # The sum tells us how many "PMID- " cells
               S$SRCH2$citeCount[2] <<- sum(TF.vec)            #    there are, which is the number of hits
               if(input$citeFormat=="MEDLINE or .nbib") mismatch = FALSE
            } else {
               TF.vec = r=="TY  - "
               if(any(TF.vec)) {
                  S$SRCH2$CFactual[2] <<- "RIS"
                  S$SRCH2$citeCount[2] <<- sum(TF.vec)         # Likewise
                  if(input$citeFormat=="RIS") mismatch = FALSE
               } else {
                  TF.vec = r=="Record"
                  if(any(TF.vec)) {
                     S$SRCH2$CFactual[2] <<- "Cochrane Central Export"
                     S$SRCH2$citeCount[2] <<- sum(TF.vec)      # Likewise
                     if(input$citeFormat=="Cochrane Central Export") mismatch = FALSE
                 } else {
                     TF.vec = str_sub(r, 1, 3)=="PT "
                     if(r[2]=="VR 1.0") {                      # This test is a little different (not any()), as
                        S$SRCH2$CFactual[2] <<- "EndNote Desktop (.ciw)"  #   .ciw files have a version number in row 2
                        S$SRCH2$citeCount[2] <<- sum(TF.vec)
                        r <- str_sub(r,1,2)                                          # take r down to 2 chars
                        while(length(i <- which(r == "  ")) > 0) { r[i] <- r[i-1] }  # fill in blanks with one above
                        if(input$citeFormat=="EndNote Desktop (.ciw)") mismatch = FALSE
                     } else {
                        TF.vec = str_sub(r, 1, 1)=="@"         # All we have to go on for BibTeX is lines that start
                        if(any(TF.vec)) {                      #   with @
                           S$SRCH2$CFactual[2] <<- "BibTeX"
                           S$SRCH2$citeCount[2] <<- sum(TF.vec)
                           if(input$citeFormat=="BibTeX") mismatch = FALSE
      }}}}}}}
   }
   if(S$SRCH2$CFactual[2]=="BAD") {
      msg = paste0(msg, "<li>This file is in an unknown format. It begins like this:</li></ul><pre>",
                  paste0(escHTML(str_sub(raw[1:6], 1, 95)), collapse="<br>"), "</pre><ul>")
   }
   if(S$SRCH2$CFactual[2]=="BLANK") {
      msg = paste0(msg, "<li>The file you've uploaded is either empty or a type of file Open-Meta doesn't support.</li>")
   }
   if(msg!="") {
      fileOK <- FALSE
      S$modal_title <<- "Warning!"
      S$modal_text <<- HTML("There are problems with this file:<ul>", msg,
                           "</ul><p>Most databases will let you export your references in RIS ",
                           "or BibTeX formats. Open-Meta supports both, as well as a few other ",
                           "specialized formats. You can click <i>Browse</i> again to upload ",
                           "the correct file.</p>")
      S$modal_size <<- "l"
      rv$modal_warning <- rv$modal_warning + 1
   }
   if(fileOK) {
      msg = saveCites(raw)                                           # File looks good, save in cites table
      f = recGet(S$db, "search", "fileName", tibble(c("searchID", "!=", S$SRCH$id)))
      if(S$SRCH2$fileName[2] %in% f$fileName) {                # Has this file already been uploaded in another search?
         msg <- paste0(msg, "<li>This file name is in an earlier search. Unless all your citation files ",
                       "have the same name, it may not be the file you want here.</li>")
      }
      if(mismatch) {
         msg <- paste0(msg, "<li>You have the File Format set to ", stripHTML(input$citeFormat), " but the actual ",
                       "format of this file is ", S$SRCH2$CFactual[2], ".</li>")
      }
      if(nchar(msg)>0) {
         S$modal_title <<- "Warning..."
         S$modal_text <<- paste0("Just so you know:<ul>", msg, "</ul>",
                                 "<p>We'll assume you uploaded the correct file, but are you sure? ",
                                 "You can click Browse again to upload a different file if necessary.</p>")
         S$modal_size <<- "l"
         rv$modal_warning <- rv$modal_warning + 1
      }
   }
   rv$hitCounter = rv$hitCounter + 1                           # update hits after uploading file
})

### This is the magic vectorized sauce for turning a cite file into a table.
###    Input is a cite file as a character vector, with one line of the file at each vector position
###    1.) The lines are split into codes (cd) and values at splitAt, which is different for various cite file formats.
###        NOTE: In theory, splitAt can be a regex ("="), but if the string appears more than once you get a mess.
###    2.) If the code has a hyphen (.nbib), remove it; then trim spaces, leaving "no code" cd's blank.
###    3.) Also trim values
###    4.) While loop will fail if code in 1st row is blank, fix it now
###    5.) While loop replaces blank codes with the preceeding valid code
###    4,) Lines belonging to a single cite are grouped using SIDmark (a code designating the start of a cite)
###    5.) Lines with the same code within a group are collapsed into one line with an "@'`#$%" separator for fixing later
###    6.) Ungroup
###    7.) Make a tibble with rows as cites and columns as codes
###    8.) If there is row with SID=0 (the rows before the first SIDmark), delete it
# Props: https://matthewlincoln.net/2016/06/06/tidying-a-crazy-single-column-table-with-readr-dplyr-and-tidyr.html
cites2table = function(r, splitAt, SIDmark) {
setProgress(.2)
   r <- separate(r, 1, into = c("cd", "value"), sep=splitAt)     # split line, cd=chars(1:splitAt), value=chars(splitAt+)
   r$cd = str_replace(r$cd, coll("-"), "")                       # get rid of dash in code (for .nbib)
   r$cd = str_trim(r$cd, side="right")                           # Make blanks blank and trim others
   r$value = str_trim(r$value, side="both")                      # Trim values
   r[[1,1]] = ifelse(r[[1,1]]=="", "zyzzy", r[[1,1]])            # Make sure 1st cell has a code for while loop
# dplyr has a fill(variable) function, but it requires NAs, not blanks. Moreover, this while
#    loop is very cool for filling in blank cds with the cd immediately above.
# Props: https://stackoverflow.com/questions/38470355/r-fill-empty-cell-with-value-of-last-non-empty-cell
   while(length(i <- which(r$cd == "")) > 0) { r$cd[i] <- r$cd[i-1] }
setProgress(.3)
   r <- r %>%
      mutate(SID = cumsum(cd == SIDmark)) %>%                     # create SID numbering
      group_by(SID, cd)                                           # group by SID
setProgress(.4)
   r <- summarize(r, value = paste(value, collapse = "@'`#$%"))   # collapse duplicate cds within SID; collapse must be a literal!
setProgress(.5)
   r <- r %>%
      ungroup() %>%                                               # remove grouping
      spread(cd, value)                                           # 1 row per SID, 1 col per cd
   r <- r[!r$SID==0,]                                             # Delete any rows with SID==0
   return(r)
}

saveCites = function(r) {                                         # incoming r is a character vector
   withProgress(message="Saving citations...", {                  # Progress bar so we know when it's done
setProgress(.1)
      msg=""                                                      # initialize msg for no errors
      r <- tibble(c=r)                                            # first change the character vector to a 1-col tibble
      switch(S$SRCH2$CFactual[2],                                 # process based on file type
         "BibTeX" = {
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\Eric.bib"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\WofK-bibtex.bib"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\WofK-bibtex2.bib"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\BibTeX-940 from Endnote.txt"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\badFormat.bib"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\biblatexExamples.bib"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\RJC.bib"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\test.bib"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\BibTex endnote long format.txt"
# S = list()
# S$SRCH2$fileName[2] = f
setProgress(.3)
            r <- suppressWarnings(ReadBib(input$citeFile$datapath))
            if(length(r)==0) {                                               # Any valid Bib entries in file?
               S$SRCH2$citeCount[2] <<- 0
               return("<li>This file has no valid BibTeX entries.</li>")
            }
setProgress(.4)
            t <- suppressWarnings(as.data.frame(r))
            if(nrow(t)==0) {                                                 # Did any survive df conversion?
               S$SRCH2$citeCount[2] <<- 0
               return("<li>This file has no valid BibTeX entries.</li>")
            }
setProgress(.5)
            if(S$SRCH2$citeCount[2] > nrow(t)) {                             # Adjust citeCount for any losses.
               msg <- paste0("<li>", S$SRCH2$citeCount[2] - nrow(t), " of the entries in this file weren't valid and were dropped.</li>")
            }
            for(c in 1:ncol(t)) {                                            # for each column of dataframe, remove all...
               while(length(i <- which(str_sub(t[[c]],1,1) == "{")) > 0) { t[i,c] <- str_sub(t[i,c],2,-1) }    # leading and
               while(length(i <- which(str_sub(t[[c]],-1,-1) == "}")) > 0) { t[i,c] <- str_sub(t[i,c],1,-2) }  # trailing brackets
               if(any(naf(!stri_enc_isutf8(t[[c]])))) {                               # check columns for non-utf8 characters
                  t[[c]] <- stri_encode(t[[c]], from = "windows-1252", to = "utf8") } #   if found, convert to utf8 assuming
            }                                                                         #   original file was windows-1252
            t$author = str_replace_all(t$author, " and ", "; ")              # change " and " in author field to "; "
            t$year = str_sub(t$year, 1, 4)                                   # Trim year to first four characters
            t = as.tibble(t)                                                 # tibble it
            cnames = names(t)                                                # RefManageR does str_to_lower on the names
            TYcode = "bibtype"
            TIcode = "title"
            AUcode = "author"
            Jcode  = "journaltitle"
            if("journal" %in% cnames) { Jcode  = "journal" }
            Ycode  = "year"
            Vcode  = "volume"
            Ncode  = "number"
            SPcode = "SP"
            EPcode = "EP"
            # Split page numbers into SP and EP
               p <- str_locate(t$pages, coll("--"))                # different databases use different separators
               if(all(is.na(p[,1]))) {
                  p <- str_locate(t$pages, coll(" - "))
               }
               if(all(is.na(p[,1]))) {
                  p <- str_locate(t$pages, coll("-"))
               }
               t$SP <- ifelse(is.na(p[,1]), t$pages, str_sub(t$pages, 1, p[,1]-1))
               t$EP <- ifelse(is.na(p[,2]), as.character(NA), str_sub(t$pages, p[,2]+1, -1))
            SNcode = "issn"
               p <- which(naf(nchar(t$issn)==8))                              # add "-" to 8-char ISSNs
               t$issn[p] <- paste0(str_sub(t$issn[p], 1, 4), "-", str_sub(t$issn[p], 5, 8))  #   numbers to speed this up.
            ABcode = "abstract"
            PMcode = "pmid"
            PCcode = "pmcid"
            DOcode = "doi"
         },
         "PMID" = {
         # PMID uploads end up here; the code turns the PMIDs into a PubMed Search;
         #    The PubMed search will return to this code, but on that trip S$SRCH2$CFactual[2] will be MEDLINE,
         #    so this code won't run a second time.
         # Format of terms for a PubMed PMID search looks like "26957379[uid] OR 25873267[uid]"
            S$SRCH2$terms[2] <<- paste0(paste0(r$c, collapse="[uid] OR "), "[uid]")
            S$PM$cascade <<- "a"            # Start PubMed observer at first section of the cascade
            rv$PMsearch <- rv$PMsearch + 1  # Run the PMsearch observer
            return("")                      # Caller expects a msg, which is blank in this case.
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\pubmed_PMID_result.txt"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\pmids.txt"
         },
      "RIS" = {
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\Ovid.ris"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\ProQuestDocuments-2018-05-12.ris"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\Ebsco-Academic Premier.ris"
# r <- tibble(stri_read_lines(f))
         # Some vendors, like EBSCO, allow a search over multiple databases that don't use the same RIS codes. That's
         #    why here we fill in NAs in the target column with data from RIS columns with redundant codes, all of
         #    which could theoretically be in the uploaded file.
            t = cites2table(r, splitAt=6, SIDmark="TY")
            cnames = names(t)                                        # Deal with coding issues:

            TYcode = "TY"                                                  # Type - used for citations only

            TIcode = "TI"                                                  # Title - can be TT, TI, T1, T2, or missing
            if(!("TI" %in% cnames)) t$TI <- rep(as.character(NA), nrow(t)) # If there's no TI column, make one
            if("TT" %in% cnames) { t$TI <- ifelse(is.na(t$TI), t$TT, t$TI) }
            if("T1" %in% cnames) { t$TI <- ifelse(is.na(t$TI), t$T1, t$TI) }
            if("T2" %in% cnames) { t$TI <- ifelse(is.na(t$TI), t$T2, t$TI) }

            AUcode = "AU"                                                  # Author can be AU, A1, or missing
            if(!("AU" %in% cnames)) t$AU <- rep(as.character(NA), nrow(t))
            if("A1" %in% cnames) { t$AU <- ifelse(is.na(t$AU), t$A1, t$AU) }

            Jcode = "JO"                                                   # Journal can be JO, JA, J1, JF, or missing
            if(!("JO" %in% cnames)) t$JO <- rep(as.character(NA), nrow(t))
            if("JA" %in% cnames) { t$JO <- ifelse(is.na(t$JO), t$JA, t$JO) }
            if("J1" %in% cnames) { t$JO <- ifelse(is.na(t$JO), t$J1, t$JO) }
            if("JF" %in% cnames) { t$JO <- ifelse(is.na(t$JO), t$JF, t$JO) }  # Full name is last choice (harder to match)
            if("T2" %in% cnames) { t$JO <- ifelse(is.na(t$JO), t$T2, t$JO) }  # JSTOR puts journal here

            Ycode = "PY"                                                   # Year - can be PY, Y1, Y2, or missing
            if(!("PY" %in% cnames)) t$PY <- rep(as.character(NA), nrow(t))
            if("Y1" %in% cnames) { t$PY <- ifelse(is.na(t$PY), t$Y1, t$PY) }
            if("Y2" %in% cnames) { t$PY <- ifelse(is.na(t$PY), t$Y2, t$PY) }

            Vcode  = "VL"                                                  # Volume

            Ncode = "IS"                                                   # Issue number
            if(!("IS" %in% cnames)) t$IS <- rep(as.character(NA), nrow(t))
            if("M1" %in% cnames) { t$IS <- ifelse(is.na(t$IS), t$M1, t$IS) }

            SPcode <- "SP"                                                 # Page; only need starting page for matching
            EPcode <- "EP"                                                 #   but need ending page to create citations

            SNcode = "SN"                                                  # ISSN or book code
            if(!("SN" %in% cnames)) t$SN <- rep(as.character(NA), nrow(t))          # Add "-" to ISSNs missing it
            p <- which(naf((str_to_upper(t$TY)=="JOUR") & (nchar(t$SN)==8)))        # which() makes a shorter vector of row
            t$SN[p] <- paste0(str_sub(t$SN[p], 1, 4), "-", str_sub(t$SN[p], 5, 8))  #   numbers to speed this up.

            ABcode = "AB"                                                  # Abstract
            if(!("AB" %in% cnames)) t$AB <- rep(as.character(NA), nrow(t))
            if("N2" %in% cnames) { t$AB <- ifelse(is.na(t$AB), t$N2, t$AB) }

            PMcode = "XX"                                                  # PMID
            if(!("XX" %in% cnames)) t$XX <- rep(as.character(NA), nrow(t))
            if("C5" %in% cnames) { t$XX <- ifelse(is.na(t$XX), t$C5, t$XX) }
            if("U2" %in% cnames && any(str_sub(t$U2, 1, 6)=="PMID: ")) {   # EBSCO databases use this
               t$XX <- ifelse(is.na(t$XX), str_sub(t$U2, 7, -2), t$XX)
            }

            PCcode = "LK"                                                  # PMCID

            DOcode <- "DO"                                                 # DOI
            if(!("DO" %in% cnames)) t$DO <- rep(as.character(NA), nrow(t))
            if("DOI" %in% cnames) { t$DO <- ifelse(is.na(t$DO), t$DOI, t$DO) }
            if("UR" %in% cnames) {                                         # EBSCO (a record can have multiple URs)
               p <- str_locate(t$UR, coll("http://dx.doi.org/"))               # start-end matrix for "http://dx.doi.org/"
               t$UR <- ifelse(is.na(p[,2]), p[,2], str_sub(t$UR, p[,2]+1, -1)) # NA or everything after "http://dx.doi.org/"
               p <- str_locate(t$UR, coll("@'`#$%"))                           # start-end matrix for "@'`#$%"
               t$UR <- ifelse(is.na(p[,1]), t$UR, str_sub(t$UR, 1, p[,1]-1))   # t$UR or part of string before "@'`#$%"
               t$DO <- ifelse(is.na(t$DO), t$UR, t$DO)                         # if we don't already have a DOI, now we do
            }
            if("L3" %in% cnames)  { t$DO <- ifelse(is.na(t$DO), t$L3, t$DO) }
         },
         "MEDLINE or .nbib" = {
#         f= file.choose()
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\pubmed_MEDLINE_result.txt"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\PubMed-citations.nbib"
# r <- tibble(stri_read_lines(f))
            t = cites2table(r, splitAt=5, SIDmark="PMID")
            cnames = names(t)
            TYcode = "PT"
            TIcode = "TI"
            AUcode = "FAU"
            if(!("FAU" %in% cnames)) t$FAU <- rep(as.character(NA), nrow(t))    # If author-full is missing, use
            if("AU" %in% cnames) { t$FAU <- ifelse(is.na(t$FAU), t$AU, t$FAU) }  #   author-initial
            Jcode  = "JT"
            Ycode  = "DP"
            Vcode  = "VI"
            Ncode  = "IP"
            SPcode = "SP"
            EPcode = "EP"
               p <- str_locate(t$PG, coll("-"))                            # Split page numbers into SP and EP
               t$SP <- ifelse(is.na(p[,1]), t$PG, str_sub(t$PG, 1, p[,1]-1))
               t$EP <- ifelse(is.na(p[,2]), as.character(NA), str_sub(t$PG, p[,2]+1, -1))
            SNcode = "IS"
               p <- str_locate(t$IS, coll(" (linking)"))
               t$IS = ifelse(is.na(p[,1]), str_sub(t$IS, 1, 9), str_sub(t$IS, p[,1]-10, p[,1]-1))
            ABcode = "AB"
            PMcode = "PMID"
            PCcode = "PMC"
            DOcode = "LID"
               if(!("LID" %in% cnames)) t$LID <- rep(as.character(NA), nrow(t))    # Make sure there's a LID column
               if("AID" %in% cnames) ifelse(is.na(t$LID), t$AID, paste0(t$LID, "@'`#$%", t$AID)) # paste LID and AID together
               p <- str_locate(t$LID, coll(" [doi]"))
               t$LID <- ifelse(is.na(p[,1]), as.character(NA), str_sub(t$LID, 1, p[,1]-1))  # chop off " [doi]" and following
               p <- str_locate(t$LID, coll("@'`#$%"))
               t$LID <- ifelse(is.na(p[,1]), t$LID, str_sub(t$LID, p[,2]+1, -1))            # chop off any part before doi
         },
         "EndNote Desktop (.ciw)" = {
# f= file.choose()
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\WebOfK Tom cites.ciw"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\WebOfK - Endnote Desktop - Full Record and Cited References.ciw"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\WofK Cochrane CR test.ciw"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\WofK Cochrane CR-94 test.ciw"
# r <- tibble(stri_read_lines(f))
            t = cites2table(r, splitAt=2, SIDmark="PT")
            cnames = names(t)
            TYcode = "PT"
            TIcode = "TI"
            AUcode = "AU"
            Jcode  = "SO"
            Ycode  = "PY"
            Vcode  = "VL"
            Ncode  = "IS"
            SPcode  = "BP"
            EPcode  = "EP"
            SNcode  = "SN"
            if(!("SN" %in% cnames)) t$SN <- rep(as.character(NA), nrow(t))    # If ISSN(SN) is missing, use
            if("EI" %in% cnames) { t$SN <- ifelse(is.na(t$SN), t$EI, t$SN) }  #   ISSN(EI)
            ABcode = "AB"
            PMcode = "PM"
            PCcode = "pmcid"
            DOcode = "DI"
         },
         "Cochrane Central Export" = {
# f <- file.choose()
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\180512 - Cochrane Central 14 hits.txt"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\170331 - Cochrane Central Results.txt"
# r <- tibble(stri_read_lines(f))
            t = cites2table(r, splitAt=4, SIDmark="ID:")
            cnames = names(t)
            if(!("PT:" %in% cnames)) {
               t[["PT:"]] = rep("Cochrane Review", nrow(t))                   # If PT is missing, it's a Cochrane Review cite file
            }
            t[["PM:"]] <- str_sub(t[["PM:"]], 8, -1)                          # Delete "PUBMED " at beginning of column
            p <- str_locate(t[["PG:"]], coll("-"))                            # Split page numbers into SP and EP
            t$SP <- ifelse(is.na(p[,1]), p[,1], str_sub(t[["PG:"]], 1, p[,1]-1))
            t$EP <- ifelse(is.na(p[,1]), p[,1], str_sub(t[["PG:"]], p[,2]+1, -1))
            TYcode = "PT:"
            TIcode = "TI:"
            AUcode = "AU:"
            Jcode  = "SO:"
            Ycode  = "YR:"
            Vcode  = "VL:"
            Ncode  = "NO:"
            SPcode  = "SP"  # No colon; created new columns above
            EPcode  = "EP"
            SNcode  = "SN"  # No ISSN field in Cochrane Export
            ABcode = "AB:"
            PMcode = "PM:"
            PCcode = "pmcid"
            DOcode = "DOI:"
         }
      )  # end of switch()
setProgress(.6)
      # clear some memory
      r = NULL
      # This makes the tibble we'll save in the database
      #   If a field value is missing from t, it creates a column of blanks; also, it makes sure a field isn't
      #      longer than its database field definition.
      tNames <- names(t)
      nrows <- nrow(t)
      r2 = tibble(
         type =     str_sub(ifelse(rep(TYcode %in% tNames, nrows), t[[TYcode]], rep("",nrows)), 1, 65535),
         title =    str_sub(ifelse(rep(TIcode %in% tNames, nrows), t[[TIcode]], rep("",nrows)), 1, 65535),
         author =   str_sub(ifelse(rep(AUcode %in% tNames, nrows), t[[AUcode]], rep("",nrows)), 1, 65535),
         journal =  str_sub(ifelse(rep(Jcode  %in% tNames, nrows), t[[Jcode]],  rep("",nrows)), 1, 65535),
         Y =        str_sub(ifelse(rep(Ycode  %in% tNames, nrows), t[[Ycode]],  rep("",nrows)), 1, 4),
         V =        str_sub(ifelse(rep(Vcode  %in% tNames, nrows), t[[Vcode]],  rep("",nrows)), 1, 10),
         N =        str_sub(ifelse(rep(Ncode  %in% tNames, nrows), t[[Ncode]],  rep("",nrows)), 1, 10),
         startP =   str_sub(ifelse(rep(SPcode %in% tNames, nrows), t[[SPcode]], rep("",nrows)), 1, 10),
         endP =     str_sub(ifelse(rep(EPcode %in% tNames, nrows), t[[EPcode]], rep("",nrows)), 1, 10),
         issn =     str_sub(ifelse(rep(SNcode %in% tNames, nrows), t[[SNcode]], rep("",nrows)), 1, 20),
         abstract = str_sub(ifelse(rep(ABcode %in% tNames, nrows), t[[ABcode]], rep("",nrows)), 1, 65535),
         pmid =     str_sub(ifelse(rep(PMcode %in% tNames, nrows), t[[PMcode]], rep("",nrows)), 1, 15),
         pmcid =    str_sub(ifelse(rep(PCcode %in% tNames, nrows), t[[PCcode]], rep("",nrows)), 1, 15),
         doi =      str_sub(ifelse(rep(DOcode %in% tNames, nrows), t[[DOcode]], rep("",nrows)), 1, 200)
      )
      # Note, NAs are changed to "" by citeTable() in sql-core.R
      #    and values are trimmed in cite2table() above
      for(c in names(r2)) {                                              # "@'`#$%" was used in cite2table() to separate
         if(c == "author") {                                             #    lines with the same code within a cite.
            r2[[c]] = str_replace_all(r2[[c]], coll("@'`#$%"), "; ")     #    Here we replace it with "; " for author
         } else {
            # if(c == "doi") {                                             # In doi, we only want the first one
            #    tf.vec = naf(str_detect(r2[[c]], coll("@'`#$%")))
            #    r2[[c]][tf.vec] = str_sub(r2[[c]][tf.vec], 1, str_locate(r2[[c]][tf.vec], coll("@'`#$%"))[,1]-1)
            # } else {
               r2[[c]] = str_replace_all(r2[[c]], coll("@'`#$%"), " ")   #    Otherwise just a space.
         }
      }
setProgress(.8)
      n = nrow(r2)
      nabs = sum(naf(r2$abstract!=""))
      npmid = sum(naf(r2$pmid!=""))
      ndoi = sum(naf(r2$doi!=""))
      S$SRCH2$citeCount[2] <<- n
      S$SRCH2$absCount[2]  <<- paste0(format(nabs,  big.mark=","), " (", format(nabs/n*100, digits=1, nsmall=1), "%)")
      S$SRCH2$pmidCount[2] <<- paste0(format(npmid, big.mark=","), " (", format(npmid/n*100, digits=1, nsmall=1), "%)")
      S$SRCH2$doiCount[2]  <<- paste0(format(ndoi,  big.mark=","), " (", format(ndoi/n*100, digits=1, nsmall=1), "%)")
      if(S$SRCH$id==0) {                            # if id = 0 we need to save the table to get an id for cite table
         S$SRCH2 <<- recSave(S$SRCH2, db=S$db)
         S$SRCH$id <<- S$SRCH2$searchID[1]
      }
setProgress(.9)
      citeTable(r2, S$db, S$SRCH$id)                # save the data in the cite table for this search
setProgress(1)
      if(all(r2$abstract=="")) {
         msg = paste0(msg, "<li>This file doesn't contain any article abstracts. We highly recommend that you return ",
                      "to this database and download the data again, but this time ask for all available fields (we ",
                      "use some odd ones to detect duplicate citations), especially the abstract. Without an abstract ",
                      "for most of your citations, your review process will be immensely more difficult.</li>")
      }
      return(msg)
   }) # end of progress
}

# PubMed search
# Save terms in S$SRCH2$terms[2]; trigger rv$PMsearch
observe({                                                        # observe rather than observeEvent because of
   if(rv$PMsearch>0) {                                           #    embedded invalidateLater()s
      switch(S$PM$cascade,
         "a" = {
            S$PM$progress <<- shiny::Progress$new()              # Create a Progress object
            S$PM$progress$set(message = "Searching PubMed...", value = 0) # Set message
            S$SRCH2$CFactual[2] <<- "MEDLINE or .nbib"
            S$SRCH2$fileName[2] <<- "Live PubMed Search"
            S$SRCH2$fileType[2] <<- "text"
            S$SRCH2$fileTime[2] <<- sTime()
            S$PM$progress$set(.2)
            max = paste0('&RetMax=', S$uploadMaxCites)
            terms <- stripHTML(S$SRCH2$terms[2])                 # Remove <p></p> and any other HTML from terms
            if(terms=="") {                                      # Make sure we have somethi8ng to search for
               S$modal_title <<- "PubMed Error"
               S$modal_text <<- HTML0("<p>Please enter something to search for in Terms.</p>")
               isolate(rv$modal_warning <- rv$modal_warning + 1)
               return()
            }
            if(is.na(S$SRCH2$beginDate[2])) { S$SRCH2$beginDate[2] <<- "1000-01-01" }       # default begin date
            if(is.na(S$SRCH2$endDate[2])) { S$SRCH2$endDate[2] <<- str_sub(now(), 1, 10) }  # default end date
            if(str_detect(terms, coll("[PDAT]")) || str_detect(terms, coll("[uid]"))) {     # coll means NOT REGEX
               terms = paste0('&term=', terms)                   # Don't add PDAT if terms already has it
            } else {                                             #    or if this is a PMID ([uid]) search
               ifelse(terms=="", "", paste0(" AND ", terms))     # Add AND (dates AND terms) unless terms is blank
               terms = paste0('&term=("', S$SRCH2$beginDate[2], '"[PDAT]:"', S$SRCH2$endDate[2] , '"[PDAT])', terms)
               terms = str_replace_all(terms, "-", "/")          # Change date delimiter
            }
            key = ifelse(PubMed.Key=="", "", paste0('&api_key=', PubMed.Key))  # PubMed.Key is in credentials; but
            Esearch <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?usehistory=y" # it's not required
            S$PM$progress$set(.4)
            url <- paste0(Esearch, key, max, str_replace_all(terms, " ", "+")) # fix url; also replace " " with "+"
            xml <- RCurl::getURL(url)                            # get xml
            S$PM$lastTime <<- now()                              # Note time of search execution
            raw <- xml2::read_xml(xml)                           # read xml
            error <- xml2::xml_text(xml2::xml_find_first(raw, "/eSearchResult/ERROR"))
            S$PM$progress$set(.6)
            if(!is.na(error)) {                                  # Not NA means there was an error!
               S$modal_title <<- "PubMed Error"
               S$modal_text <<- HTML0("<p>PubMed returned the following error: <b>", error, "</b></p>")
               S$modal_size <<- "l"
               isolate(rv$modal_warning <- rv$modal_warning + 1)
               return()
            }
            S$SRCH2$citeCount[2] <<- as.numeric(xml2::xml_text(xml_find_first(raw, "/eSearchResult/Count")))
            if(S$SRCH2$citeCount[2]==0) {                        # Check for at least 1 hit
               S$modal_title <<- "PubMed Error"
               S$modal_text <<- HTML0("<p>Nothing found.</p>")
               isolate(rv$modal_warning <- rv$modal_warning + 1)
               return()
            }
            if(S$SRCH2$citeCount[2]>S$uploadMaxCites) {          # Check for too many hits
               S$modal_title <<- "PubMed Error"
               S$modal_text <<- HTML0("<p>Your search has over ", format(S$uploadMaxCites, big.mark=","), " citations, ",
                                 "which is too many to process. You need to modify your terms or dates and search ",
                                 "PubMed again. If you really want to review over ", format(S$uploadMaxCites, big.mark=","),
                                 " citations, create multiple searches, splitting them up by publication date ",
                                 "into smaller chunks.</p>")
               isolate(rv$modal_warning <- rv$modal_warning + 1)
               return()
            }
            S$PM$progress$set(.8)
            S$PM$Chunk <<- 0                                                        # Start at zero
            S$PM$Chunks <<- S$SRCH2$citeCount[2] %/% S$PM$Chunksize                 #    number of complete chunks
            S$PM$Chunks <<- ifelse((S$SRCH2$citeCount[2] %% S$PM$Chunksize)>0, S$PM$Chunks+1, S$PM$Chunks) # add 1 for partial
            S$SRCH2$query[2] <<- xml2::xml_text(xml_find_first(raw, "/eSearchResult/QueryTranslation")) # save returned Query
            QKey <- xml2::xml_text(xml_find_first(raw, "/eSearchResult/QueryKey"))  # Prep URL for fetch from
            WebEnv <- xml2::xml_text(xml_find_first(raw, "/eSearchResult/WebEnv"))  #   PubMed history
            webenv = paste0('&WebEnv=', WebEnv, '&query_key=', QKey)
            EFetch <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&rettype=MEDLINE&retmode=text"
            S$PM$url <<- paste0(EFetch, key, webenv)             # Save URL for next part of cascade
            S$PM$cascade <<- "b"                                 #    which is part "b"
            S$PM$progress$close()                                # Close "Searching PubMed..." progress bar
            S$PM$progress <<- shiny::Progress$new()              # Create "Retreiving PubMed Data..." Progress object
            S$PM$progress$set(message = "Retreiving PubMed Data...", value = 0) # Set message
            invalidateLater(100)                                 # Move on to part B
            return()
         },
         "b" = {
            S$PM$progress$set((S$PM$Chunk/S$PM$Chunks)-.03)      # not more than .97
            pauseFor <- PubMed.Delay - (seconds(now()-S$PM$lastTime)*1000)
            if(pauseFor > 0) {                                   # Pausing so other code gets a shot at the processor
               invalidateLater(max(c(100,pauseFor)))             #    and to honor PubMed delay
               return()
            } else {
               S$PM$url2 <- paste0(S$PM$url, '&retstart=', (S$PM$Chunksize*S$PM$Chunk)+1, '&retMax=', S$PM$Chunksize)
               S$PM$Medline <<- paste0(S$PM$Medline, RCurl::getURL(S$PM$url2))   # Use URL constructed above to get cite data
               S$PM$lastTime <<- now()                           # Note end time of search execution
               S$PM$Chunk <<- S$PM$Chunk+1
               if(S$PM$Chunk==S$PM$Chunks) {                     # Started at zero, so == means end
                  S$PM$cascade <<- "c"                           # Done; move on to part "c"
               }
               invalidateLater(100)                              # Loop through part "b"
               return()
            }
         },
         "c" = {
               S$SRCH2$fileSize[2] <<- nchar(S$PM$Medline)
               S$PM$progress$close()                             # Close progress bar (saveCites will open its own)
               saveCites(stri_split_lines1(S$PM$Medline))        # Splits the single string into lines
               isolate(rv$limn <- rv$limn + 1)                   # Need to use limn to save Name and Comments
         },
         stop("S$PM$cascade is invalid.")
      )
      S$PM$cascade <<- "a"                                       # Ready for next time
      S$PM$url <<- ""
      S$PM$Medline <<- ""                                        # Especially this one
   }
})

### observer for omclick
observeEvent(input$js.omclick, {
   if(debugON) {
      cat(paste0("Click on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]                                 # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]                                 #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "sub" = {
         S$hideMenus <<- FALSE
         if(n=="2") {                               # New Search
            if(S$P$Modify) { S$hideMenus <<- TRUE } # Don't hide menus if a NOT PERMITTED error message is coming up
            S$SRCH$justArrived <<- TRUE             # When starting a new search and to view/edit an old search
            S$SRCH$id <<-0
            rv$limn = rv$limn + 1                   # Needed to hide menus
         }
         if(n=="3") {
            S$modal_title <<- "Upcoming"
            S$modal_text <<- HTML("Feature not yet available.")
            rv$modal_warning <- rv$modal_warning + 1
         }
         rv$menuActive = n                          # rv$menuActive takes us to top
      },
      "viewSearch" = {                              # This button is on the list of searches when the user can't modify
         S$hideMenus <<- TRUE
         S$SRCH$justArrived <<- TRUE                # Gets the data to view
         S$SRCH$id <<- n
         rv$menuActive = 2
         rv$limn = rv$limn + 1                      # Needed to hide menus
      },
      "editSearch" = {                              # This button is on the list of searches for powerful users
         if(S$P$Modify) {
            S$hideMenus <<- TRUE
            S$SRCH$justArrived <<- TRUE             # Gets the data to edit
            S$SRCH$id <<- n
            rv$menuActive = 2
            rv$limn = rv$limn + 1                   # Needed to hide menus
         }
      },
      "deleteAsk" = {                            # This button is on the list of searches for powerful users before a search is complete
         if(S$P$Modify) {
            S$modal_title <<- "No Undo!!!"
            S$modal_text <<- HTML0("<p>Once you delete a search it's gone.</p>")
            S$modal_footer <<- tagList(modalButton("Cancel"), bs4("btn", id="deleteSearch", n=n, q="on", "Delete"))
            rv$modal_warning <- rv$modal_warning + 1
         }
      },
      "deleteSearch" = {                            # This button is on the list of searches for powerful users before a search is complete
         removeModal()
         if(S$P$Modify) {
            S$SRCH2 <<- searchGet(SELECT="**", WHERE=tibble(c("searchID", "=", n)))
            S$SRCH2$deleted[2] <<- 1
            S$SRCH2 <<- recSave(S$SRCH2, db=S$db)
            rv$limn = rv$limn + 1                   # Re-display search list
         }
      },
      "updateSearch" = {                            # This button is on the list of searches for powerful users when Search is completed
         if(S$P$Modify) {
            S$SRCH2 <<- searchGet(SELECT="**", WHERE=tibble(c("searchID", "=", n)))
            S$SRCH2$status[2] <<- 2
            S$SRCH2 <<- recSave(S$SRCH2, db=S$db)
            S$SRCHu <<- searchGet()
            S$SRCHu$searchName[2] <<- paste0("Update of... ", S$SRCH2$searchName[2])
            S$SRCHu$database[2]   <<- S$SRCH2$database[2]
            S$SRCHu$otherDB[2]    <<- S$SRCH2$otherDB[2]
            S$SRCHu$beginDate[2]  <<- as.character.POSIXt(ymd(S$SRCH2$endDate[2])-1)  # Day before original's end date
            S$SRCHu$endDate[2]    <<- str_sub(sTime(), 1, 10)
            S$SRCHu$terms[2]      <<- S$SRCH2$terms[2]
            S$SRCHu$query[2]      <<- S$SRCH2$query[2]
            S$SRCHu$CFchosen[2]   <<- S$SRCH2$CFchosen[2]
            S$SRCHu$citeCount[2]  <<- 0
            S$SRCHu$fileName[2]   <<- ""
            S$SRCHu$comment[2]    <<- ""
            S$SRCHu$createDate[2] <<- sTime()
            S$SRCH2 <<- recSave(S$SRCHu, db=S$db)
            S$hideMenus <<- TRUE                       # Code from viewSearch above
            S$SRCH$justArrived <<- TRUE
            S$SRCH$id <<- S$SRCH2$searchID[1]
            rv$menuActive = 2
            rv$limn = rv$limn + 1
         }
      },
      "processCheck" = {                            # This button is part of new/edit Search
         if(S$P$Modify) {
            S$modal_title <<- "No Undo!!!"
S$modal_text <<- HTML0("<p>Once you process a search successfully you can't change it, delete it, or delete its ",
"citations. The citations found by this Search will be checked against the other citations in this search as well ",
"as the citations in your project for duplicates. The unduplicated citations will be added to your project, ready ",
"for Review. This may take a few minutes and <b>cannot be undone!</b> Are you sure you want to proceed?</p>")
            S$modal_size <<- "l"
            S$modal_footer <<- tagList(modalButton("Cancel"), bs4("btn", uid="OK2process_1", q="on", "Process"))
            rv$modal_warning <- rv$modal_warning + 1
         }
      },
      "OK2process" = {
         removeModal()
         if(S$P$Modify) {
            S$SRCH$saveFlag <<- TRUE
            S$SRCH$processFlag <<- TRUE
            return(js$getEdit("terms"))
         }
      },
      "searchpubmed" = {
         if(S$P$Modify) {
            S$PM$search <<- TRUE                       # Flag to tell input$js.editorText to run rv$PMsearch observer
            return(js$getEdit("terms"))                #   after getting terms out of Quill editor
         }
      },
      "save" = {
         if(S$P$Modify) {
            # S$hideMenus <<- FALSE                    # With save, this happens later
            S$SRCH$saveFlag <<- TRUE
            return(js$getEdit("terms"))
         }
      },
      "cancel" = {
         S$hideMenus <<- FALSE                      # Back to regular programming
         rv$menuActive = 1
         rv$limn = rv$limn + 1
      },
      "details" = {
         switch(input$citeFormat,
            "Live" = {
S$modal_text <<- HTML0("<p><b>PubMed Live Format</b></p>",
"<p>This is a live search of the US National Library of Medicine's PubMed database ",
"using the Date Range and Terms you specify. Sponsored by the US National Institues of Health, ",
"PubMed is a freely available resource. The Open-Meta app communicates with PubMed using ",
"the library's <i>Entrez E-utilties</i> computer-to-computer interface.</p> ",
"<p>Note that PubMed will convert your Date Range and Terms into a Query that will be ",
"somewhat different from what you've entered. The PubMed-revised Query will be displayed after you search. ",
"To edit the query, copy and paste it into the Terms field and search again.</p>",
"<p>After searching you will also see the <i>number</i> of citations found by your search, ",
"but not actual <i>examples</i> of the citations. For that, enter the Query in ",
"<a href='https://www.ncbi.nlm.nih.gov/pubmed/', target='_blank'>PubMed</a> itself.</p>",
"<p>The earliest publication date currently in PubMed is in the 1780s. Since this could change, our default start date ",
"is the beginning of the year 1000. You can, of course, change this to whatever works best for you. However, if you start ",
"at the year zero and end before today, PubMed may return false positives. The ending publication date defaults to the ",
"day the search was created.</p>",
"<p>This link provides complete information on PubMed's ",
"<a href='https://www.ncbi.nlm.nih.gov/books/NBK3827/#pubmedhelp.Search_Field_Descriptions_and', target='_blank'>",
"Search Field Descriptions and Tags</a>, which can be helpful for interpreting the returned Query.</p>")
            },
            "PMID" = {
S$modal_text <<- HTML0("<p><b>PubMed PMID Format</b></p>",
"<p><b>PMID</b> stands for <i>PubMed ID</i>. This format is just a text list of ",
"identification numbers, with each number on its own line.</p><p>The beginning of a ",
"file in this format looks like this (with different numbers):<pre>",
"26068298
26068297
26429571
26481332
26219612</pre>",
"<p>You can easily create a file like this by hand, which can be useful for entering extra ",
"citations you've found outside a standard search if those citations are in PubMed. ",
"PubMed itself will also let you download a citation file in this format. First click ",
"<i>Send to</i>, set the Destination to <i>File</i>, and the Format to <i>PMID List</i> ",
"as shown here:</p>",
"<img src='http://assets.open-meta.org/images/pubmed-pmid.png' class='img-center'>")
            },
            "MEDLINE or .nbib" = {
S$modal_text <<- HTML0("<p><b>MEDLINE or .nbib Format</b></p>",
"<p>The MEDLINE database is part of PubMed and is also available from Ovid and other ",
"database vendors. The National Library of Medicine has created a controlled vocabulary of ",
"medical subject headings, known as <b>MeSH</b>, that can be used on PubMed and MEDLINE to ",
"find relevant citations.</p><p>The beginning of a MEDLINE-formatted (.nbib) file looks like this:</p>",
"<pre>PMID- 26957379
OWN - NLM
STAT- MEDLINE
DCOM- 20161230
LR  - 20161231</pre>",
"<p>Both PubMed and Ovid will let you download a citation file in this format. On PubMed, click ",
"<i>Send to</i> and set the Destination to <i>Citation manager</i> as shown here:</p>",
"<img src='http://assets.open-meta.org/images/pubmed-medline.png' class='img-center'>")
            },
            "Cochrane Central Export" = {
S$modal_text <<- HTML0("<p><b>Cochrane Central Export Format</b></p>",
"<p>The Cochrane Central Register of Controlled Trials (<a href='http://cochranelibrary-wiley.com/cochranelibrary/search', target='_blank'>CENTRAL</a>) ",
"contains a record for every trial ever examined in Cochrane's own systematic reviews. It's updated monthly with ",
"new trial records from MEDLINE, EMBASE, and additional sources. It also supports the US National Library of ",
"Medicine's <b>MeSH</b> subject vocabulary.</p><p>The beginning of a CENTRAL file looks like this:</p>",
"<pre>Record #1 of 14
ID: CD000227
AU: Avenell Alison
AU: Mak Jenson CS
AU: O'Connell Dianne
</pre>",
"<p>The big advantage of CENTRAL is that it provides all the randomized controlled trials ever found by Cochrane researchers, ",
"who are the gold-standard experts in systematic reviewing. It also provides one-click access to everything it finds for you. ",
"After refining your search, simply click the <i>Export all</i> link shown here:</p>",
"<img src='http://assets.open-meta.org/images/cochrane-central-1.png', class='img-center'",
"<p><br>When exporting from CENTRAL, make sure you pick the <b>Citation And Abstract</b> <i>File type</i> as ",
"shown here, rather than <b>Citation Only</b>. For <i>Export type</i>, match your own computer's operating system.</p>",
"<img src='http://assets.open-meta.org/images/cochrane-central-2.png', class='img-center'")
            },
            "EndNote Desktop (.ciw)" = {
S$modal_text <<- HTML0("<p><b>EndNote Desktop (.ciw) Format</b></p>",
"<p>The <i>Web of Science</i> database and the <i>EndNote</i> citation manager are both products of the same company, ",
"Clarivate Analytics. Like <i>Cochrane CENTRAL</i>, <i>Web of Science</i> is a little weird because it won't save ",
"citations in the most popular formats. But, like CENTRAL, it has a unique feature that can be very helpful for ",
"systematic reviews: It can give you all the articles cited by an older systematic review on your topic.</p>",
"<p>The beginning of an Endnote Desktop (.ciw) file looks like this:</p>",
"<pre>FN Clarivate Analytics Web of Science
VR 1.0
PT J
AU Abu-Mouch, Saif
   Fireman, Zvi</pre>",
"<p>There are four steps to capturing the citations in a prior systematic review. First, search for the review you want ",
"in the <i>Web of Science Core Collection</i> (Web of Science can also give you MEDLINE citations, among others, but those don\'t ",
"include cited references). Once you find it, click on its title. Here's what that part looks like:</p>",
"<img src='http://assets.open-meta.org/images/web-of-science-1.png', class='img-center'",
"<p><br>This will open a page that shows two numbers in a large font. The upper number leads to newer articles that have cited ",
"this one. The lower number leads to the older articles this systematic review cited. You want to click on that second ",
"number. Here's what that part looks like:</p>",
"<img src='http://assets.open-meta.org/images/web-of-science-2.png', class='img-center'",
"<p><br>Next, some download controls will appear at the top of a page listing the first of those citations. ",
"Without checking any boxes next to article titles, select <i>Save to EndNote desktop</i> from the dropdown. ",
"Here's what that looks like:</p>",
"<img src='http://assets.open-meta.org/images/web-of-science-3.png', class='img-center'",
"<p><br>Finally, a box like the next graphic will appear. It allows you to download up to 500 citations at once. If there are ",
"more, as in this example, you'll have to repeat this part to capture the additional references. In addition to entering ",
"the record numbers, make sure you specify that you want each record to include the <b>Abstract</b>.</p>",
"<img src='http://assets.open-meta.org/images/web-of-science-4.png', class='img-center'",
"<p><br>Note that you don't actually want to <i>Send to EndNote</i>, you want to save the citation file on your computer so ",
"that you can upload it to the Open-Meta app. If your system insists on loading the files into EndNote, try this on a system ",
"that doesn't have EndNote installed.")
            },
            "RIS" = {
S$modal_text <<- HTML0("<p><b>RIS Format</b></p>",
"<p>The <a href='https://en.wikipedia.org/wiki/RIS_(file_format)', target='_blank'>RIS citation format</a> is supported by ",
"most bibliographic databases and citation managers.</p>",
"<p>The beginning of a RIS file looks like this:</p>",
"<pre>TY  - JOUR
AU  - Weishaar, Tom
AU  - Vergili, Joyce Marcley
T1  - Vitamin D Status Is a Biological Determinant of Health Disparities
JO  - Journal of the Academy of Nutrition & Dietetics</pre>")
            },
            "BibTeX" = {
S$modal_text <<- HTML0("<p><b>BibTeX Format</b></p>",
"<p>The <a href='https://en.wikipedia.org/wiki/BibTeX', target='_blank'>BibTeX citation format</a> is supported by ",
"most bibliographic databases and citation managers.</p>",
"<p>The beginning of a BibTeX file looks like this:</p>",
"<pre>@article{EJ101442620130701,
Abstract = {Foodborne illnesses remain....}, </pre>")
            },
            message(paste0("In input$js.omclick observer for details, no handler for ", input$citeFormat, "."))
         )
         S$modal_title <<- "Format Details"
         S$modal_size <<- "l"
         rv$modal_warning <- rv$modal_warning + 1
      },
      "pmid-doi-info" = {
         S$modal_title <<- "Counts explained"
S$modal_text <<- HTML0("<p>In addition to the number of citations found in your search, we show  counts and ",
                       "percentages for abstracts, PMIDs (PubMed IDs), and DOIs (Document Object Identifiers).<ul>",
                       "<li><b>Abstracts.</b> During your initial review to determine which articles meet the inclusion ",
                       "criteria for your project, you will examine article titles and abstracts. If your citation file ",
                       "includes abstracts, the Open-Meta app will make your review very easy. Without abstracts, on the ",
                       "other hand, you will have to go elsewhere to look them up, which will make your review extremely ",
                       "difficult. If your abstract count is zero, you should return to the database you used and ",
                       "download the citations again, this time making sure the download includes abstracts. Over ",
                       "90% of your citations should have abstracts; it's difficult to hit 100% because some ",
                       "items, like letters to the editor, don't have them.</li>",
                       "<li><b>PMIDs.</b>The Open-Meta app uses PubMed IDs to locate duplicate citations ",
                       "and to find full-text versions of your citations during data extraction. Only citations listed ",
                       "in the US Library of Medicine's <a href='https://www.ncbi.nlm.nih.gov/pubmed/'' target='_blank'>",
                       "PubMed database</a> have PubMed IDs, however, so if your project isn't about ",
                       "health, few of your citations will have PMIDs. (Also, RIS and BibTeX citation files don't have ",
                       "consistent codes for PMIDs; if you think your file has them but they don't show up here, let ",
                       "us know so we can fix things.)</li>",
                       "<li><b>DOIs.</b> The Open-Meta app uses Document Object Identifiers to locate duplicate ",
                       "citations and to find full-text versions of your citations during data extraction. Older citations ",
                       "are less likely to have DOIs than newer citations.</li>",
                       "</ul></p>")
         S$modal_size <<- "l"
         S$modal_footer <<- tagList(modalButton("Cancel"))
         rv$modal_warning <- rv$modal_warning + 1
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# observeEvent(input$searchDates, {
#    print("-----")
#    print(paste0("beginDate class: ", class(input$searchDates[1]), ", value: ", input$searchDates[1]))
#    print(paste0("endDate class: ", class(input$searchDates[2]), ", value: ", input$searchDates[2]))
# })

# Add records in cite table to main Hits file
#    Find and mark duplicates
processSearch = function() {
   if(S$P$Modify) {
      S$SRCH2$status[2] <<- 1
      S$SRCH2 <<- recSave(S$SRCH2, db=S$db)
   }
}



