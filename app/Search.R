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

S$PM = list()         # PubMed Search
S$PM$lastTime = now()
S$PM$search <- FALSE
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
            if(S$P$Modify && S$SRCH2$status==0) {
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
         S$SRCH2$terms[2] <<- t
         if(S$PM$search) {                  # We're about to search PubMed
            S$PM$search <<- FALSE
            S$SRCH2$beginDate[2]  <<- stripHTML(as.character(input$searchDates[1]))  # Need these for the search
            S$SRCH2$endDate[2]    <<- stripHTML(as.character(input$searchDates[2]))
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
         S$SRCH2 <<- recSave(S$SRCH2, db=S$db)
         if(S$SRCH$processFlag) {
            S$SRCH$processFlag <<- FALSE
            processSearch()               # This is elsewhere to simplify code for this observer
         }
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
                  if(S$P$Modify) {
                     buttons = list(edit=list(id="editSearch", label="Edit", q="g", class=""),
                                 delete=list(id="deleteAsk", label="Delete", q="r", class=""))
                  } else {
                     buttons = list(edit=list(id="viewSearch", label="View", q="b", class=""))
                  }
               )
               # if you need to further modify Rx, you can do it here.
               if(S$P$Modify && nrow(Rx)>0) {
                  Rx[Rx$status>0,7] <- bs4("btn", id="viewSearch", q="b", "View")
                  Rx[Rx$status==1,8] <- bs4("btn", id="updateSearch", q="r", "Update")
                  Rx[Rx$status>1,8] <- ""
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
            if(S$P$Modify && S$SRCH2$status==0) {             # Edit (or New)
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
                              bs4("quill", id="terms", S$SRCH2$terms[2]),
                              bs4("d", bs4("btn", class="mb-3", id="searchpubmed", q=c("p", "s", "b"), "Search PubMed")),
                              if(S$SRCH2$query[2]!="") {
                                 HTML("PubMed translated your Publication Dates and Terms into this Query",
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
                        HTML('<div class="form-group w-50">',
                        '<label class for="citeTotal">Number of citations in this search</label>',
                        '<input id="citeTotal" type="text" class="form-control w-50" value="" readonly="readonly"></div>'),
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
                        bs4("c10", tagList(
                           h4(S$SRCH$pageTitle),
                           # Database and File Format
                           HTML("<p>Database</p>",
                                "<input type='text', value='",
                                ifelse(S$SRCH2$database[2]!="All Other", S$SRCH2$database[2], S$SRCH2$otherDB[2]),
                                "', class='form-control figure w-50', readonly='readonly'><br>",
                                "<p>File Format</p><input type='text', value='",
                                 S$SRCH2$CFchosen[2],
                                "', class='form-control figure w-50', readonly='readonly'><br>"),
                           # Search Name
                           HTML("<p>Search Name</p>",
                                "<input type='text', value='", S$SRCH2$searchName[2],
                                "'class='form-control w-100', readonly='readonly'><br>"),
                           # Pub date range
                           HTML("<p>Publication date range of this search</p>",
                                "<input type='text', value='",
                                S$SRCH2$beginDate[2],
                                "'class='d-inline form-control w-25', readonly='readonly'",
                                ">&nbsp;to&nbsp;<input type='text', value='",
                                S$SRCH2$endDate[2], "'class='d-inline form-control w-25', readonly='readonly'>"),
                           # PubMed Terms & Query
                           if(S$SRCH2$CFchosen[2]=="Live") {
                              tagList(
                                 HTML("<br><hr><p>Terms</p><p><i>", S$SRCH2$terms[2], "</i></p>"),
                                 if(S$SRCH2$query[2]!="") {
                                    HTML("<hr><p>PubMed translated your Publication Dates and Terms into this Query</p>", "<p><i>",
                                         S$SRCH2$query[2], "</i></p><hr>")
                                 }
                              )
                           } else {
                           # Not PubMed Query & File Name
                              tagList(
                                 HTML("<br><hr><p>Query</p>"),
                                 HTML("<p><i>", S$SRCH2$query[2], "</i></p><hr>",
                                 "<p>Citation file</p>",
                                 "<input type='text', value='",
                                 S$SRCH2$fileName[2],
                                 "'class='form-control w-100', readonly='readonly'><br>")
                              )
                           },
                           #citeCount
                           HTML("<p>Number of citations in this search</p>",
                           "<input type='text' class='form-control w-50' value='",
                           format(S$SRCH2$citeCount[2], big.mark=","),
                           "' readonly='readonly'>"),
                           # Cancel Button
                           HTML('<div class="text-right mt-3">'),
                           bs4("btn", id="cancel", n=1, q="b", "Cancel"),
                           HTML('</div>')
                     )))
                  )
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
      hc = format(S$SRCH2$citeCount[2], big.mark=",")  # Add commas
   } else {
      hc = ""                                          # Blank if result is 0
   }
   updateTextInput(session, inputId="citeTotal", value=hc)
   updateTextInput(session, inputId="citeFile", value=S$SRCH2$fileName[2])
})


# This observer runs when a file upload has completed.
# It captures the file and its metadata and does an initial scan to determine
#    whether the file is in a valid format and how many cites in has.
#    Ends with either a failure message or an update to "Number of citations in this search".

# need to save fileOK somewhere for completeness test...
# need to save text vector at end if it's ok (or BibTeX)

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
   S$SRCH2$CFactual[2] <<- "BAD"                               #    likewise
   S$SRCH2$citeCount[2] <<- 0                                  #    likewise
   # First check file's extension (But does this work on Macs???)
   fnParts = str_split(S$SRCH2$fileName[2], "[.]")
   fileExt = fnParts[[1]][[length(fnParts[[1]])]]
   if(!(str_to_lower(fileExt) %in% c("txt", "ciw", "ris", "bib", "nbib"))) {
      msg = paste0(msg, "<li>This file's extension is <b>.", fileExt, "</b>. That's not a file type Open-Meta can import.</li>")
   } else {
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
   r <- separate(r, 1, into = c("cd", "value"), sep=splitAt)   # split line, cd=chars(1:splitAt), value=chars(splitAt+)
   r$cd = str_replace(r$cd, fixed("-"), "")                    # get rid of dash in code (for .nbib)
   r$cd = str_trim(r$cd, side="right")                         # Make blanks blank and trim others
   r$value = str_trim(r$value, side="both")                    # Trim values
   r[[1,1]] = ifelse(r[[1,1]]=="", "zyzzy", r[[1,1]])          # Make sure 1st cell has a code for while loop
# dplyr has a fill(variable) function, but it requires NAs, not blanks. Moreover, this while
#    loop is very cool for filling in blank cds with the cd immediately above.
# Props: https://stackoverflow.com/questions/38470355/r-fill-empty-cell-with-value-of-last-non-empty-cell
   while(length(i <- which(r$cd == "")) > 0) { r$cd[i] <- r$cd[i-1] }
   r <- r %>%
     mutate(SID = cumsum(cd == SIDmark)) %>%                   # create SID numbering
     group_by(SID, cd) %>%                                     # group by SID
     summarize(value = paste(value, collapse = "@'`#$%")) %>%  # collapse duplicate cds within SID; collapse must be a literal!
     ungroup() %>%                                             # remove grouping
     spread(cd, value)                                         # 1 row per SID, 1 col per cd
   r <- r[!r$SID==0,]                                          # Delete any rows with SID==0
   return(r)
}

saveCites = function(r) {                                      # incoming r is a character vector
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
         r <- suppressWarnings(ReadBib(input$citeFile$datapath))
         if(length(r)==0) {                                               # Any valid Bib entries in file?
            S$SRCH2$citeCount[2] <<- 0
            return("<li>This file has no valid BibTeX entries.</li>")
         }
         t <- suppressWarnings(as.data.frame(r))
         if(nrow(t)==0) {                                                 # Did any survive df conversion?
            S$SRCH2$citeCount[2] <<- 0
            return("<li>This file has no valid BibTeX entries.</li>")
         }
         if(S$SRCH2$citeCount[2] > nrow(t)) {                             # Adjust citeCount for any losses.
            msg <- paste0("<li>", S$SRCH2$citeCount[2] - nrow(t), " of the entries in this file weren't valid and were dropped.</li>")
            S$SRCH2$citeCount[2] <<- nrow(t)
         }
         for(c in 1:ncol(t)) {                                            # for each column of dataframe, remove all...
            while(length(i <- which(str_sub(t[[c]],1,1) == "{")) > 0) { t[i,c] <- str_sub(t[i,c],2,-1) }    # leading and
            while(length(i <- which(str_sub(t[[c]],-1,-1) == "}")) > 0) { t[i,c] <- str_sub(t[i,c],1,-2) }  # trailing brackets
         }
         t$author = str_replace_all(t$author, " and ", "; ")              # change " and " in author field to "; "
         t$year = str_sub(t$year, 1, 4)                                   # Trim year to first four characters
         t = as.tibble(t)                                                 # tibble it
         TYcode = "bibtype"
         TIcode = "title"
         AUcode = "author"
         Jcode  = "journaltitle"
         Ycode  = "year"
         Vcode  = "volume"
         Ncode  = "number"
         Pcode  = "pages"
         ABcode = "abstract"
         PMcode = "pmid"
         PCcode = "pmcid"
         DOcode = "doi"
      },
      "PMID" = {
# f = file.choose()
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\pubmed_PMID_result.txt"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\Citations\\pmids.txt"
   t <- tibble(PMID = r[[1]])
         TYcode = "PT"
         TIcode = "TI"
         AUcode = "AU"
         Jcode  = "SO"
         Ycode  = "YR"
         Vcode  = "VL"
         Ncode  = "NO"
         Pcode  = "PG"
         ABcode = "AB"
         PMcode = "PMID"
         PCcode = "pmcid"
         DOcode = "DO"
      },
      "RIS" = {
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\Ovid.ris"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\ProQuestDocuments-2018-05-12.ris"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\Ebsco-Academic Premier.ris"
# r <- tibble(stri_read_lines(f))
         t = cites2table(r, splitAt=6, SIDmark="TY")
         cnames = names(t)                      # Deal with coding issues:
         TYcode = "TY"
         if("TT" %in% cnames) {                 # Title can be TT, TI, T1, or missing
            TIcode = "TT"
         } else {
            if ("TI" %in% cnames) {
               TIcode = "TI"
            } else {
               TIcode = "T1"
         }}
         if("AU" %in% cnames) {                 # Author can be AU, A1, or missing
            AUcode = "AU"
         } else {
            AUcode = "A1"
         }
         if("JF" %in% cnames) {                 # Journal can be JF, JO, JA, J1, or missing
            Jcode = "JF"
         } else {
            if("JO" %in% cnames) {
               Jcode = "JO"
            } else {
               if("JA" %in% cnames) {
                  Jcode = "JA"
               } else {
                  Jcode = "J1"
         }}}
         if("PY" %in% cnames) {                 # Year is first 4 characters of PY or Y1 or Y2
            Ycode = "PY"
         } else {
            if("Y1" %in% cnames) {
               Ycode = "Y1"
            } else {
               Ycode = "Y2"
         }}
         Vcode  = "VL"
         if("IS" %in% cnames) {
            Ncode = "IS"
         } else {
            Ncode = "M1"
         }
         if(all(c("SP", "EP") %in% cnames)) {                         # if we have both kinds of page numbers,
            t$P <- ifelse(is.na(t$EP), t$SP, paste0(t$SP, "-", t$EP)) # glue them together in P
            Pcode <- "P"
         } else {                                                     # otherwise, take the one we can get
            if("SP" %in% cnames) {
               Pcode <- "SP"
            } else {
               Pcode <- "EP"
         }}
         if("AB" %in% cnames) {                        # Abstract can be AB, N2, or missing
            ABcode = "AB"
         } else {
            ABcode = "N2"
         }
         PMcode = "XX"
         PCcode = "pmcid"
         if("DOI" %in% cnames) {                        # DOI can be DO or L3 or maybe DOI
            DOcode = "DOI"
         } else {
            if("DO" %in% cnames) {                        # DOI can be DO or L3
               DOcode = "DO"
            } else {
               DOcode = "L3"
            }
      }},
      "MEDLINE or .nbib" = {
#         f= file.choose()
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\pubmed_MEDLINE_result.txt"
# f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\PubMed-citations.nbib"
# r <- tibble(stri_read_lines(f))
         t = cites2table(r, splitAt=5, SIDmark="PMID")
         t$LID <- str_replace(t$LID, fixed(" [doi]"), "")
         TYcode = "PT"
         TIcode = "TI"
         AUcode = "FAU"
         Jcode  = "JT"
         Ycode  = "DP"
         Vcode  = "VI"
         Ncode  = "IP"
         Pcode  = "PG"
         ABcode = "AB"
         PMcode = "PMID"
         PCcode = "PMCID"
         DOcode = "LID"
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
         if(all(c("SP", "EP") %in% cnames)) {                         # if we have both kinds of page numbers,
            t$P <- ifelse(is.na(t$EP), t$SP, paste0(t$SP, "-", t$EP)) # glue them together in P
            Pcode <- "P"
         } else {
            Pcode <-"SP"
         }
         TYcode = "DT"
         TIcode = "TI"
         AUcode = "AF"
         Jcode  = "SO"
         Ycode  = "PY"
         Vcode  = "VL"
         Ncode  = "IS"
         Pcode  = "P"
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
         t[["PM:"]] <- str_sub(t[["PM:"]], 8, -1)                          # Delete "PUBMED " at beginning of column
         if(!("PT:" %in% names(t))) {
            t[["PT:"]] = rep("Cochrane Review", nrow(t))                   # If PT is missing, it's a Cochrane Review cite file
         }
         TYcode = "PT:"
         TIcode = "TI:"
         AUcode = "AU:"
         Jcode  = "SO:"
         Ycode  = "YR:"
         Vcode  = "VL:"
         Ncode  = "NO:"
         Pcode  = "PG:"
         ABcode = "AB:"
         PMcode = "PM:"
         PCcode = "pmcid"
         DOcode = "DOI:"
      }
   )  # end of switch()
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
      V =        str_sub(ifelse(rep(Vcode  %in% tNames, nrows), t[[Vcode]],  rep("",nrows)), 1, 50),
      N =        str_sub(ifelse(rep(Ncode  %in% tNames, nrows), t[[Ncode]],  rep("",nrows)), 1, 50),
      P =        str_sub(ifelse(rep(Pcode  %in% tNames, nrows), t[[Pcode]],  rep("",nrows)), 1, 50),
      abstract = str_sub(ifelse(rep(ABcode %in% tNames, nrows), t[[ABcode]], rep("",nrows)), 1, 65535),
      pmid =     str_sub(ifelse(rep(PMcode %in% tNames, nrows), t[[PMcode]], rep("",nrows)), 1, 15),
      pmcid =    str_sub(ifelse(rep(PCcode %in% tNames, nrows), t[[PCcode]], rep("",nrows)), 1, 15),
      doi =      str_sub(ifelse(rep(DOcode %in% tNames, nrows), t[[DOcode]], rep("",nrows)), 1, 200)
   )
   # Note, NAs are changed to "" by citeTable() in sql-core.R
   #    and values are trimmed in cite2table() above
   for(c in names(r2)) {                                              # "@'`#$%" was used in cite2table() to separate
      if(c == "author") {                                             #    lines with the same code within a cite.
         r2[[c]] = str_replace_all(r2[[c]], fixed("@'`#$%"), "; ")    #    Here we replace it with "; " for author
      } else {
         if(c == "doi") {                                             # In doi, we only want the first one
            tf.vec = naf(str_detect(r2[[c]], fixed("@'`#$%")))
            r2[[c]][tf.vec] = str_sub(r2[[c]][tf.vec], 1, str_locate(r2[[c]][tf.vec], fixed("@'`#$%"))[,1]-1)
         } else {
            r2[[c]] = str_replace_all(r2[[c]], fixed("@'`#$%"), " ")  #    Otherwise just a space.
         }}
   }
   S$SRCH2$citeCount[2] <<- nrow(r2)
   if(S$SRCH$id==0) {                            # if id = 0 we need to save the table to get an id for cite table
      S$SRCH2 <<- recSave(S$SRCH2, db=S$db)
      S$SRCH$id <<- S$SRCH2$searchID[1]
   }
   citeTable(r2, S$db, S$SRCH$id)                # save the data in the cite table for this search
   return(msg)
}



# S$PF$trigger=0
# This is an invalidateLater() cascade for processing a completed Search
# rv$processFile = rv$processFile + 1      # start file processing
# observe({
#    rv$processFile    # trigger for this observer
#    switch(S$PF$trigger,
#       "1" = {
#       },
#       "2" = {
#          switch(S$SRCH2$CFactual[2],
#             "RIS" = {
#
#             },
#             "NBIB" = {
#
#             },
#             "BIB" = {
#
#             },
#             "CIW" = {
#
#             },
#             "CC" = {
#
#             },
#             "PMID" = {
#
#             }
#          )
#
#          S$PF$trigger <<-3
#          invalidateLater(500, session)
#       },
#       "3" = {
#          S$PF$trigger <<-4
#          invalidateLater(500, session)
#       },
#       "4" = {
#
#          S$PF$trigger <<-5
#          invalidateLater(500, session)
#       },
#       "5" = {
#
#          S$PF$trigger <<-6
#          invalidateLater(500, session)
#       },
#       "6" = {
#
#          S$PF$trigger <<-1
#       }
#    )
# })

# PubMed search
# Save terms in S$SRCH2$terms[2]; trigger rv$PMsearch
observe({                                                              # observe rather than observeEvent because of
   if(rv$PMsearch>0) {                                                 #    embedded invalidateLater()
      start.time <- Sys.time()
      on.exit(print(Sys.time() - start.time))
      pauseFor <- PubMed.Delay - (seconds(now()-S$PM$lastTime)*1000)   # PubMed.Delay comes from the credentials file...
      if(pauseFor > 0) {                                               #    If required delay minus time elapsed is more than 0
         invalidateLater(pauseFor)                                     #    pause that long
         return()                                                      #    done for now
      } else {                                                         # Otherwise, do the search
         S$PM$lastTime <<- now()                                       # Note time of search execution
         max = paste0('&RetMax=', S$uploadMaxCites)
         terms <- stripHTML(S$SRCH2$terms[2])                          # Remove <p></p> and any other HTML from terms
         if(terms=="") {
            S$modal_title <<- "PubMed Error"
            S$modal_text <<- HTML0("<p>Please enter something to search for in Terms.</p>")
            isolate(rv$modal_warning <- rv$modal_warning + 1)
            return()
         }
         if(is.na(S$SRCH2$beginDate[2])) { S$SRCH2$beginDate[2] <<- "1000-01-01" }       # default begin date
         if(is.na(S$SRCH2$endDate[2])) { S$SRCH2$endDate[2] <<- str_sub(now(), 1, 10) }  # default end date
         if(str_detect(terms, coll("[PDAT]"))) {                       # coll means NOT REGEX
            terms = paste0('&term=', terms)                            # Don't add PDAT if terms already has it
         } else {
            if(terms!="") { terms <- paste0(" AND ", terms) }          # Add AND (dates AND terms) unless terms is blank
            terms = paste0('&term=("', S$SRCH2$beginDate[2], '"[PDAT]:"', S$SRCH2$endDate[2] , '"[PDAT])', terms)
         }
         Esearch <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?"
         url <- paste0(Esearch, PubMed.Key, max,                       # PubMed.Key is in credentials; will work with a blank
                       str_replace_all(terms, " ", "+"))               # fix url; also replace " " with "+"
         xml <- RCurl::getURL(url)                                     # get xml
         raw <- xml2::read_xml(xml)                                    # read xml
         error <- xml2::xml_text(xml2::xml_find_first(raw, "/eSearchResult/ERROR"))
         if(is.na(error)) {                                            # An NA error message means no error!
            S$SRCH2$citeCount[2] <<- as.numeric(xml2::xml_text(xml_find_first(raw, "/eSearchResult/Count")))
            S$SRCH2$query[2] <<- xml2::xml_text(xml_find_first(raw, "/eSearchResult/QueryTranslation"))
            pmids = xml_text(xml2::xml_find_all(raw, "/eSearchResult/IdList/Id"))   # This is a character vector
            if(S$SRCH2$citeCount[2]==0) {                              # Check for at least 1 hit
               S$modal_title <<- "PubMed Error"
               S$modal_text <<- HTML0("<p>Nothing found.</p>")
               isolate(rv$modal_warning <- rv$modal_warning + 1)
               return()
            }
            if(S$SRCH2$citeCount[2]>S$uploadMaxCites) {                # Check for too many hits
               S$modal_title <<- "PubMed Error"
               S$modal_text <<- HTML0("<p>Your search has over ", format(S$uploadMaxCites, big.mark=","), " citations, ",
                                 "which is too many to process. You need to modify your terms or dates and search ",
                                 "PubMed again. If you really want to review over ", format(S$uploadMaxCites, big.mark=","),
                                 " citations, create multiple searches, splitting them up by publication date ",
                                 "into smaller chunks.</p>")
               isolate(rv$modal_warning <- rv$modal_warning + 1)
               return()
            }
            S$SRCH2$CFactual[2] <<- "PMID"
            S$SRCH2$fileName[2] <<- "Live PubMed Search"
            S$SRCH2$fileSize[2] <<- 0
            S$SRCH2$fileType[2] <<- "xml"
            S$SRCH2$fileTime[2] <<- sTime()
            saveCites(pmids)
            isolate(rv$limn <- rv$limn + 1)                           # Need to use limn to save Name and Comments
#            isolate(rv$render <- rv$render + 1)
         } else {
            S$modal_title <<- "PubMed Error"
            S$modal_text <<- HTML0("<p>PubMed returned the following error: <b>", error, "</b></p>")
            S$modal_size <<- "l"
            isolate(rv$modal_warning <- rv$modal_warning + 1)
         }
      }
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
            S$modal_title <<- "Upcoming Feature"
            S$modal_text <<- HTML("<p>The ability to update a search is coming soon.</p>")
            S$modal_size <<- "l"
            rv$modal_warning <- rv$modal_warning + 1
         }
      },
      "check" = {
         S$modal_title <<- "Upcoming Feature"
         S$modal_text <<- HTML("<p>The ability to check whether this Search's details are complete and ready to process is coming soon.</p>")
         S$modal_size <<- "l"
         rv$modal_warning <- rv$modal_warning + 1
         # check whether search is ready to process; need this code more than once, write a function
         # if(input$database=="All Other" && input$otherDB=="") {
         #    msg = paste0(msg, "<li>Database name can't be blank.</li>")
         # }
         # if(is.na(input$searchDates[2])) {
         #    msg = paste0(msg, "<li>End date can't be blank.</li>")
         # }
      },
      "processCheck" = {                            # This button is part of new/edit Search
         if(S$P$Modify) {
            S$modal_title <<- "No Undo!!!"
S$modal_text <<- HTML0("<p>Once you process a search successfully you can't change it, delete it, or delete its ",
"citations. The citations found by this Search will be checked against the other citations in your project for ",
"duplicates. If necessary, the app will obtain addtional bibliographic details from PubMed. Finally, it will add ",
"the citations to your project, ready for Review. <b>This may take a few minutes and cannot be undone!</b> Are ",
"you sure you want to proceed?</p>")
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

}
