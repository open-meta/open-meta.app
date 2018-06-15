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
S$SRCH$id = 0      # Initialize for new search; edit will update
rv$processFile = 0 # Trigger to begin processFile observer
rv$hitCounter = 0  # Trigger to update "Number of citations in this search" readonly field
rv$render = 0      # Trigger to render page after dealing with input initializatons
S$PF$trigger = 1   # Which section we're processing now (used with invalidateLater())

S$PM = list()     # PubMed Search
S$PM$minDelay = 400        # in milliseconds get this from settings eventually
S$PM$lastTime = now()
S$PM$search <- FALSE
rv$PMsearch <- 0

S$SRCH$saveFlag <- FALSE
S$SRCH$processFlag <- FALSE

# Only needed on this page:
searchGet = function(db=S$db, SELECT="", WHERE=tibble(c("searchID", ">", "0"))) {
   return(recGet(db, "search", SELECT, WHERE))
}

rv$menuActive = 1    # Start out on first sub-menu

# Need this to make choices stick without changing other inputs
# Not sure how to keep this from running twice when changing the database as that also
#    changes citeFormat. But need both triggers here or things don't work right.
observeEvent(c(rv$menuActive, rv$limn, input$database, input$citeFormat), {
   if(rv$menuActive==2) {                           # Bypass all this if we're not editing a Search
      if(S$SRCH$justArrived) {                      # first time through
         S$SRCH$justArrived <<- FALSE
         if(S$SRCH$id==0) {                         # Get a new record to save this search in
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
         } else {                                   # Get the record to edit
            S$SRCH$pageTitle <<- "Edit your search details"
            S$SRCH2 <<- searchGet(SELECT="**", WHERE=tibble(c("searchID", "=", S$SRCH$id)))
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
   # $fileRaw   filled in by citeFile???
   if(S$SRCH$saveFlag) {                          # Can't save a search if any of these are bad.
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
      if(S$SRCH$processFlag) {                    # A search can be saved if these are bad, but not processed.
         if(stripHTML(S$SRCH2$query[2])=="") {
            msg = paste0(msg, "<li>Your Query can't be blank.</li>")
         }
         if(S$SRCH2$CFchosen[2]=="Live") {        # PubMed
            if(S$SRCH2$citeCount[2]==0) {         # PubMed, no cites
               msg = paste0(msg, "<li>There are no citations to process. You need to modify your terms or dates ",
                                 "and search PubMed again.</li>")
            }
            if(S$SRCH2$citeCount[2]>3000) {       # PubMed, too many cites
               msg = paste0(msg, "<li>Your search has over 3,000 citations, which is too many to process. You need ",
                                 "to modify your terms or dates and search PubMed again. If you really want to review ",
                                 "over 3,000 citations, create multiple searches, splitting them up by publication date ",
                                 "into smaller chunks.</li>")
            }
         } else {                                 # Not PubMed
            if(S$SRCH2$citeCount[2]>3000) {       # Other, too many cites
               msg = paste0(msg, "<li>Your search has over 3,000 citations, which is too many to process. You need ",
                                 "to modify your terms or dates and upload a smaller file. If you really want to review ",
                                 "over 3,000 citations, split the citations up by publication date into smaller chunks ",
                                 "and create multiple searches.</li>")
            }
            if(S$SRCH2$citeCount[2]==0) {         # Other, no cites
               if(S$SRCH2$CFactual[2]=="") {      # Other, no file so no cites
                  msg = paste0(msg, "<li>There are no citations to process. You need to upload a citation file.</li>")
               } else {                           # Other, file but no cites
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
      if(nchar(msg)) {                            # No render if there's a modal; put this on top of existing render
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
            processSearch()                       # This is elsewhere to simplify code for this observer
         }
         rv$menuActive = 1
         S$hideMenus <<- FALSE            # Back to regular programming
         rv$limn = rv$limn + 1            # Need to re-limn here to get menus back, but skip for modal dialog
      }
   } else {                               # Not a save or process, just a render
      rv$render = rv$render + 1           # If not a save or process, just proceed to next step of rendering the page
   }
})

if(S$P$Msg=="") {
   output$uiMeat <- renderUI({rv$render; isolate({
      if(rv$limn && S$P$Msg=="") {
            switch(as.character(rv$menuActive),
               "1" = {
                  restOfPage = tagList(
                     DTOutput("showSearches")
                  )
                  output$showSearches  <- renderDT({                    # Get project's searches
                     Rx = omRx(                                         # Now get their user info
                        db = S$db,
                        table = "search",
                        SELECT = c("searchID", "searchName", "beginDate", "endDate", "status"),
                        WHERE = tibble(c("deleted", "=", 0)),
                        buttons = list(edit=list(id="editSearch", label="Edit Search", q="y", class=""),
                                       process=list(id="procSearch", label="Process", q="r", class=""))
                     )
                     # if you need to further modify Rx, you can do it here.
                     Rx <- Rx[,-1]                                      # Remove ID column
                     Rx$status = ifelse(Rx$status==0, "Incomplete",
                                    ifelse(Rx$status==1, "Process-Ready",
                                       ifelse(Rx$status==2, "Processed", "Updated")))
                     omDT(Rx,                                           #    ...moving buttons to 6 and 7
                        cnames = c("Search Name", "Begin Date", "End Date", "Status", "Edit", "Process"),
                        colesc = c(1:4),                                # columns to escape (minus means don't escape)
                        noText = "No searches found for this project"      # What to say when there are no results
                     )
                  },
                  # renderDT() parameters go here:
                  server = FALSE
                  )
               },
               "2" = {
                  if(S$P$Modify) {                                  # S$SRCH2 has info for inputs in all cases
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
                                    if(S$SRCH2$query[2]!="") HTML("PubMed translated your Terms into this Query", "<p><i>", S$SRCH2$query[2], "</i></p>")
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
               },
               "3" = {
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
      }
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
# need to save text vector at end if it's ok (or BibTex)

observeEvent(input$citeFile, {
   S$SRCH2$fileName[2] <<- stripHTML(input$citeFile$name)
   S$SRCH2$fileSize[2] <<- input$citeFile$size
   S$SRCH2$fileType[2] <<- input$citeFile$type
#   S$SRCH2$fileRaw[2]  <<- read_file(input$citeFile$datapath) # We'll do this later when we're sure the file is good
   S$SRCH2$fileTime[2] <<- sTime()
   msg <- ""                   # Assume no error
   fileOK <- TRUE              #   likewise
   mismatch = TRUE             # Assume an error
   S$SRCH2$CFactual[2] <<- "BAD" #    likewise
   S$SRCH2$citeCount[2] <<- 0    #    likewise
   # First check file's extension (But does this work on Macs???)
   fnParts = str_split(S$SRCH2$fileName[2], "[.]")
   fileExt = fnParts[[1]][[length(fnParts[[1]])]]
   if(!(str_to_lower(fileExt) %in% c("txt", "ciw", "ris", "bib", "nbib"))) {
      msg = paste0(msg, "<li>This file's extension is <b>.", fileExt, "</b>. That's not a file type Open-Meta can import.</li>")
   } else {
      # Has this file already been uploaded in another search?
      f = recGet(S$db, "search", "fileName", tibble(c("deleted", "=", 0)))
      if(S$SRCH2$fileName[2] %in% f$fileName) {
         msg <- "<li>This file name is in an earlier search. Unless all your citation files have the same name (not good), it is almost certainly not the file you want here.</li>"
      }

      # Read file as a string vector, one line of file per cell, then delete blank lines
      # Can still choke on odd files with nulls, like gifs.
      rf <- stri_read_lines(input$citeFile$datapath)           # "experimental" but fixes encoding issues
      r <- rf[str_trim(rf)!=""]

      # This section figures out the file format
      if(length(r)>0 && all(!is.na(suppressWarnings(as.numeric(r))))) {  # PMID is all numeric but could be only 1 line!
         S$SRCH2$CFactual[2] <<- "PMID"
         S$SRCH2$citeCount[2] <<- length(r)
         if(input$citeFormat=="PMID") mismatch = FALSE
      } else {                                                 # Otherwise needs to have at least 2 lines
         if(length(r)<6) {                                     #    to test for CIW, but none of the remaining
            S$SRCH2$CFactual[2] <<- "BLANK"                    #    formats are less than 5 lines, probably more...
            mismatch = FALSE                                   # TRUE sends the wrong message
         } else {
            r <- str_sub(r,1,6)                                # Cut the strings back to first 6 characters
            TF.vec = r=="PMID- "                               #    only for the remaining tests.
            if(any(TF.vec)) {
               S$SRCH2$CFactual[2] <<- "MEDLINE or .nbib"      # The sum tells us how many "PMID- " cells
               S$SRCH2$citeCount[2] <<- sum(TF.vec)            #    there are, which is the number of hits
               if(input$citeFormat=="MEDLINE or .nbib") mismatch = FALSE
            } else {
               TF.vec = r=="TY  - "
               if(any(TF.vec)) {
                  S$SRCH2$CFactual[2] <<- "RIS"
                  S$SRCH2$citeCount[2] <<- sum(TF.vec)           # Likewise
                  if(input$citeFormat=="RIS") mismatch = FALSE
               } else {
                  TF.vec = r=="Record"
                  if(any(TF.vec)) {
                     S$SRCH2$CFactual[2] <<- "Cochrane Central Export"
                     S$SRCH2$citeCount[2] <<- sum(TF.vec)        # Likewise
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
                           if(input$citeFormat=="BibTex") mismatch = FALSE
      }}}}}}}
   }
   if(S$SRCH2$CFactual[2]=="BAD") {
      msg = paste0(msg, "<li>This file is in an unknown format. It begins like this:</li></ul><pre>",
                  paste0(escHTML(str_sub(rf[1:6], 1, 95)), collapse="<br>"), "</pre><ul>")
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
      loadFile()
      if(mismatch) {
         S$modal_title <<- "Warning!"
         S$modal_text <<- HTML("You have the File Format set to ", stripHTML(input$citeFormat), " but the actual format ",
                               "of this file is ", S$SRCH2$CFactual[2], ". We'll assume you uploaded the correct file, ",
                               "but are you sure? You can click Browse again to upload a different file if necessary.")
         S$modal_size <<- "l"
         rv$modal_warning <- rv$modal_warning + 1
      }
   }
   rv$hitCounter = rv$hitCounter + 1    # update hits after uploading file
})

processSearch = function() {
   return()
   switch(S$SRCH2$CFactual[2],
      "BibTeX" = {
         # Needs library(RefManageR)!!!
         r <- as.tibble(ReadBib(input$citeFile$datapath))
      },
      "PMID" = {
         r <- tibble(PMID=stri_read_lines(input$citeFile$datapath))
      },
      "RIS" = {
         r <- tibble(stri_read_lines(input$citeFile$datapath))
#         f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\Ovid.ris"
#         f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\ProQuestDocuments-2018-05-12.ris"
#         f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\Ebsco-Academic Premier.ris"
#         r <- tibble(stri_read_lines(f))
         r <- separate(r, 1, into = c("variable", "value"), sep=6)
         r$variable = str_sub(r$variable, 1, 2)
         r <- mutate(r, SID = cumsum(variable == "TY"))
         r <- group_by(r, SID, variable)
         r <- summarize(r, value = paste(value, collapse = "; ")) %>%
              ungroup() %>%                                                      # remove grouping
              spread(variable, value)
         # cnames = names(r)
         # if(all(c("SP", "EP") %in% cnames)) {          # if we have both kinds of page numbers,
         #    r$P <- ifelse(is.na(r$EP), r$SP, paste0(r$SP, "-", r$EP)) # glue them together in P
         #    cnames = names(r)
         # } else {                                      # otherwise, take the one we can get
         #    cnames[cnames=="SP"] <- "P"
         #    cnames[cnames=="EP"] <- "P"
         # }
         # if("PY" %in% cnames) {                        # if we have both kinds of years, prefer PY
         #    cnames[cnames=="PY"] <- "Y"
         # } else {
         #    cnames[cnames=="Y1"] <- "Y"
         # }
         # cnames[cnames=="TY"] <- "Type"
         # cnames[cnames=="T1"] <- "Title"
         # cnames[cnames=="JF"] <- "Journal"
         # cnames[cnames=="VL"] <- "V"
         # cnames[cnames=="IS"] <- "N"
         # cnames[cnames=="XX"] <- "PMID"
         # cnames[cnames=="N2"] <- "Abstract"
         # cnames[cnames=="AB"] <- "Abstract"
         # cnames[cnames=="KW"] <- "Keywords"
         # cnames[cnames=="A1"] <- "Authors"
         # cnames[cnames=="AU"] <- "Authors"
         # cnames[cnames=="L3"] <- "DOI"
         # r <- r[nchar(cnames)!=2]               # NOTE that none of the new names can have 2 characters!!!
         # names(r) <- cnames[nchar(cnames)!=2]          # Rename columns with non-2-char cnames
         # r = r[!r$SID==0,]                             # If there is a row with SID==0, delete it
      },
      "MEDLINE or .nbib" = {
         r <- tibble(stri_read_lines(input$citeFile$datapath))
#         f= file.choose()
#         f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\pubmed_MEDLINE_result.txt"
#         f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\PubMed-citations.nbib"
#         r <- tibble(stri_read_lines(f))
         r <- separate(r, 1, into = c("variable", "value"), sep=4)
         r$variable = str_sub(r$variable, 1, 4)
         r$value    = str_sub(r$value, 3, -1)
         r <- mutate(r, SID = cumsum(variable == "PMID"))
         while(length(i <- which(r$variable == "    ")) > 0){ r$variable[i] <- r$variable[i-1] }
         r <- group_by(r, SID, variable)
         r <- summarize(r, value = paste(value, collapse = "; ")) %>%
              ungroup() %>%                                                      # remove grouping
              spread(variable, value)
         # cnames = names(r)
         # cnames[cnames=="PT  "] <- "TypeX"
         # cnames[cnames=="TI  "] <- "Title"
         # cnames[cnames=="JT  "] <- "Journal"
         # cnames[cnames=="DP  "] <- "Y"        # Need to trim
         # cnames[cnames=="VI  "] <- "V"
         # cnames[cnames=="IP  "] <- "N"
         # cnames[cnames=="PG  "] <- "P"
         # cnames[cnames=="PMID"] <- "PMIDX"
         # cnames[cnames=="AB  "] <- "Abstract"
         # cnames[cnames=="MH  "] <- "Keywords"
         # cnames[cnames=="FAU "] <- "Authors"
         # cnames[cnames=="LID "] <- "DOI"             # Will trim below
         # r <- r[nchar(cnames)!=2]                    # NOTE that none of the new names can have 2 characters!!!
         # cnames <- cnames[nchar(cnames)!=2]          # Drop names with 2 characters
         # r <- r[nchar(cnames)!=4]                    # NOTE that none of the new names can have 4 characters!!!
         # cnames <- cnames[nchar(cnames)!=4]          # Drop names with 4 characters
         # cnames[cnames=="TypeX"] <- "Type"           # Fix 4-char names
         # cnames[cnames=="PMIDX"] <- "PMID"
         # names(r) <- cnames
         # r = r[!r$SID==0,]                           # If there is a row with SID==0, delete it
         # r = r[,]
         # r$Y = str_sub(r$Y, 1, 4)                    # Trim year, next 2 lines trim DOI
         # cutAt = str_locate(r$DOI, fixed(" [doi]"))  # returns a matrix with begin and end of string in cols 1 & 2
         # r$DOI <- ifelse(is.na(cutAt[,1]), "", str_sub(r$DOI, 1, cutAt[,1]-1))
      },
      "EndNote Desktop (.ciw)" = {
         r <- tibble(stri_read_lines(input$citeFile$datapath))
#         f= file.choose()
#         f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\WebOfK Tom cites.ciw"
#         f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\WebOfK - Endnote Desktop - Full Record and Cited References.ciw"
#         r <- tibble(stri_read_lines(f))
         r <- separate(r, 1, into = c("variable", "value"), sep=3)
         r$variable = str_sub(r$variable, 1, 2)
         r <- mutate(r, SID = cumsum(variable == "PT"))
         while(length(i <- which(r$variable == "  ")) > 0){ r$variable[i] <- r$variable[i-1] }
         r <- group_by(r, SID, variable)
         r <- summarize(r, value = paste(value, collapse = "; ")) %>%
              ungroup() %>%                                                      # remove grouping
              spread(variable, value)
         # cnames = names(r)
         # if(all(c("BP", "EP") %in% cnames)) {          # if we have both kinds of page numbers,
         #    r$P <- ifelse(is.na(r$BP), r$BP, paste0(r$BP, "-", r$EP)) # glue them together in P
         #    cnames = names(r)
         # } else {                                      # otherwise, take the one we can get
         #    cnames[cnames=="BP"] <- "P"
         #   cnames[cnames=="EP"] <- "P"
         # }
         # cnames[cnames=="DT"] <- "Type"
         # cnames[cnames=="TI"] <- "Title"
         # cnames[cnames=="SO"] <- "Journal"
         # cnames[cnames=="PY"] <- "Y"
         # cnames[cnames=="VL"] <- "V"
         # cnames[cnames=="IS"] <- "N"
         # cnames[cnames=="PM"] <- "PMID"
         # cnames[cnames=="CR"] <- "CitedRefs"
         # cnames[cnames=="AB"] <- "Abstract"
         # cnames[cnames=="ID"] <- "Keywords"
         # cnames[cnames=="AF"] <- "Authors"
         # cnames[cnames=="DI"] <- "DOI"
         # r <- r[nchar(cnames)!=2]               # NOTE that none of the new names can have 2 characters!!!
         # names(r) <- cnames[nchar(cnames)!=2]          # Rename columns with non-2-char cnames
         # r = r[!r$SID==0,]                             # If there is a row with SID==0, delete it
      },
      "Cochrane Central Export" = {
         r <- tibble(stri_read_lines(input$citeFile$datapath))
#         f <- file.choose()
#         f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\180512 - Cochrane Central 14 hits.txt"
#         f = "C:\\Users\\Tom\\Documents\\SugarSync-Tom\\Open-Meta.org\\GitHub - Open-Meta\\sandbox\\app\\Citations\\170331 - Cochrane Central Results.txt"
#         r <- tibble(stri_read_lines(f))
         r <- separate(r, 1, into = c("variable", "value"), sep=4)
         r$variable = str_sub(r$variable, 1, 2)
         r <- mutate(r, SID = cumsum(variable == "ID"))
         r <- group_by(r, SID, variable)
         r <- summarize(r, value = paste(value, collapse = "; ")) %>%
              ungroup() %>%                                                      # remove grouping
              spread(variable, value)
         # cnames = names(r)
         # cnames[cnames=="PT"] <- "Type"
         # cnames[cnames=="TI"] <- "Title"
         # cnames[cnames=="SO"] <- "Journal"
         # cnames[cnames=="YR"] <- "Y"
         # cnames[cnames=="VL"] <- "V"
         # cnames[cnames=="NO"] <- "N"
         # cnames[cnames=="PG"] <- "P"
         # cnames[cnames=="PM"] <- "PMID"
         # cnames[cnames=="AB"] <- "Abstract"
         # cnames[cnames=="KY"] <- "Keywords"
         # cnames[cnames=="AU"] <- "Authors"
         # cnames[cnames=="DO"] <- "DOI"
         # r <- r[nchar(cnames)!=2]               # NOTE that none of the new names can have 2 characters!!!
         # names(r) <- cnames[nchar(cnames)!=2]          # Rename columns with non-2-char cnames
         # r = r[!r$SID==0,]                             # If there is a row with SID==0, delete it
         # r$PMID = str_sub(r$PMID, 8, -1)
      }
   )
   S$SRCH2$fileRaw[2]  <<- toJSON(r)                                     # Save whole tibble in DB for now.
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
observe({
   if(rv$PMsearch>0) {
# print("=========Inside PMsearch observer")
# print(S$SRCH2$beginDate[2])
# print(S$SRCH2$endDate[2])
      pauseFor <- S$PM$minDelay - (seconds(now()-S$PM$lastTime)*1000)  # If required delay minus time elapsed is
      if(pauseFor > 0) {                                               #    more than 0
         invalidateLater(pauseFor)                                     #    pause that long
      } else {                                                         # Otherwise, do the search
         S$PM$lastTime <<- now()                                       # Note time of search execution
         max = 1
         terms <- stripHTML(S$SRCH2$terms[2])                          # Remove <p></p> and any other HTML from terms
         if(is.na(S$SRCH2$beginDate[2])) { S$SRCH2$beginDate[2] <<- "1000-01-01" }       # default begin date
         if(is.na(S$SRCH2$endDate[2])) { S$SRCH2$endDate[2] <<- str_sub(now(), 1, 10) }  # default end date
         if(str_detect(terms, coll("[PDAT]"))) {                       # coll means NOT REGEX
            terms = paste0('&term=', terms)                            # Don't add PDAT if terms already has it
         } else {
            if(terms!="") { terms <- paste0(" AND ", terms) }          # Add AND (dates AND terms) unless terms is blank
            terms = paste0('&term=("', S$SRCH2$beginDate[2], '"[PDAT]:"', S$SRCH2$endDate[2] , '"[PDAT])', terms)
         }
         Esearch <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?tool=rct.app&email=jtw2117@columbia.edu"
         url <- paste0(Esearch, '&RetMax=', max, str_replace_all(terms, " ", "+"))    # fix url; also replace " " with "+"
# print(url)
         xml <- RCurl::getURL(url)                                     # get xml
         raw <- xml2::read_xml(xml)                                    # read xml
         error <- xml2::xml_text(xml2::xml_find_first(raw, "/eSearchResult/ERROR"))
         if(is.na(error)) {                                            # An NA error message means no error!
            S$SRCH2$citeCount[2] <<- xml2::xml_text(xml_find_first(raw, "/eSearchResult/Count"))
            S$SRCH2$query[2] <<- xml2::xml_text(xml_find_first(raw, "/eSearchResult/QueryTranslation"))
            pmids = xml_text(xml2::xml_find_all(raw, "/eSearchResult/IdList/Id"))
            if(length(pmids)==0) {                                     # Check for at least 1 hit
               S$modal_title <<- "PubMed Error"
               S$modal_text <<- HTML0("<p>Nothing found.</p>")
               isolate(rv$modal_warning <- rv$modal_warning + 1)
               return()
            }
            r = tibble(SID=1:length(pmids), PMID=pmids)
            S$SRCH2$fileRaw[2]  <<- toJSON(r)
            S$SRCH2$fileName[2] <<- "Live PubMed Search"
            S$SRCH2$fileSize[2] <<- 0
            S$SRCH2$fileType[2] <<- "xml"
            S$SRCH2$fileTime[2] <<- sTime()
            isolate(rv$render <- rv$render + 1)
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
            if(S$P$Modify) { S$hideMenus <<- TRUE } # Don't hide menus if an error message is coming up
            S$SRCH$justArrived <<- TRUE                  # When starting a new search and when starting to edit an old search
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
      "editSearch" = {
         if(S$P$Modify) { S$hideMenus <<- TRUE }    # Don't hide them if an error message is coming up
         S$SRCH$justArrived <<- TRUE                     # When starting a new search and when starting to edit an old search
         S$SRCH$id <<- n
         rv$menuActive = 2
         rv$limn = rv$limn + 1                      # Needed to hide menus
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
      "processCheck" = {
         S$modal_title <<- "No Undo!!!"
S$modal_text <<- HTML0("<p>Once you process a search successfully you can't change it, delete it, or delete its ",
"citations. The citations found by this Search will be checked against the other citations in your project for ",
"duplicates. If necessary, the app will obtain addtional bibliographic details from PubMed. Finally, it will add ",
"the citations to your project, ready for Review. <b>This may take a few minutes and cannot be undone!</b> Are ",
"you sure you want to proceed?</p>")
         S$modal_size <<- "l"
         S$modal_footer <<- tagList(modalButton("Cancel"), bs4("btn", uid="OK2process_1", q="on", "OK"))
         rv$modal_warning <- rv$modal_warning + 1
      },
      "OK2process" = {
         removeModal()
         S$SRCH$saveFlag <<- TRUE
         S$SRCH$processFlag <<- TRUE
         return(js$getEdit("terms"))
      },
      "searchpubmed" = {
         S$PM$search <<- TRUE                       # Flag to tell input$js.editorText to run rv$PMsearch observer
         return(js$getEdit("terms"))                #   after getting terms out of Quill editor
      },
      "save" = {
         # S$hideMenus <<- FALSE                    # With save, this happens later
         S$SRCH$saveFlag <<- TRUE
         return(js$getEdit("terms"))
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

observeEvent(input$searchDates, {
   print("-----")
   print(paste0("beginDate class: ", class(input$searchDates[1]), ", value: ", input$searchDates[1]))
   print(paste0("endDate class: ", class(input$searchDates[2]), ", value: ", input$searchDates[2]))
})
