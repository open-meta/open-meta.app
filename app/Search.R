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
databases = c("PubMed", "Cochrane Central", "Web of Knowledge", "All Other")
citeFormats = list(c("Live", "PMID", "MEDLINE or .nbib"), "Cochrane Central Export", "EndNote Desktop (.ciw)", c("RIS", "BibTeX", "PMID"))
S$SRCH$id = 0     # Initialize for new search; edit will update
rv$processFile    # Trigger to begin processFile observer
rv$hitCounter = 0 # Trigger to update "Number of citations in this search" readonly field
rv$render = 0     # Trigger to render page after dealing with input initializatons
S$PF$trigger = 1  # Which section we're processing now (used with invalidateLater())

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
      if(S$justArrived) {                             # first time through
         S$justArrived <<- FALSE
         if(S$SRCH$id==0) {                         # Get a new record to save this search in
            S$pageTitle <<- "Enter your search details"
            S$SRCH2 <<- searchGet()                 # These become the initial values displayed on a new search
            S$SRCH2$searchName[2] <<- ""
            S$SRCH2$database[2]   <<- "PubMed"
            S$SRCH2$otherDB[2]    <<- ""
            S$SRCH2$beginDate[2]  <<- as.Date(NA)
            S$SRCH2$endDate[2]    <<- sTime()
            S$SRCH2$terms[2]      <<- ""
            S$SRCH2$query[2]      <<- ""
            S$SRCH2$CFchosen[2]   <<- "Live"
            S$SRCH2$citeLevel[2]  <<- 1
            S$SRCH2$fileName[2]   <<- ""
            S$SRCH2$comment[2]    <<- ""
            S$SRCH2$createDate[2] <<- sTime()
         } else {                                   # Get the record to edit
            S$pageTitle <<- "Edit your search details"
            S$SRCH2 <<- searchGet(SELECT="**", WHERE=tibble(c("searchID", "=", S$SRCH$id)))
         }
      } else {
         S$saveSearch <<- FALSE
         return(js$getEdit("terms"))             # Goto next observeEvent() to get current inputs before rendering
      }
   }
   rv$render = rv$render + 1                     # Not editing, just render
})

# fills S$SRCH2 fields with current inputs; used by Render (S$saveSearch=FALSE) and by Save (S$saveSearch=TRUE)
# To call this use:
   # S$saveSearch <<- TRUE/FALSE
   # js$getEdit("terms")
observeEvent(input$js.editorText, {
   id = input$js.editorText[1]         # The quill id
   t = input$js.editorText[2]          # The edited text
   switch(id,
      "terms" = {
         S$SRCH2$terms[2] <<- t
         return(js$getEdit("query"))
      },
      "query" = {
         S$SRCH2$query[2] <<- t
         return(js$getEdit("comment"))
      },
      "comment" = {
         S$SRCH2$comment[2] <<- t
#         S$SRCH2$comment[2] <<- input$citeFile$datapath     # you can stick stuff in the comments for debugging
      },
      message(paste0("In input$js.editorText observer, no handler for ", id, "."))
   )
   S$SRCH2$searchName[2] <<- esc(str_sub(input$searchName, 1, 254))    # VARCHAR(254) in SQL
   # $status filled in by "Check Search"
   S$SRCH2$database[2]   <<- input$database
   if(!is.null(input$otherDB)) {
      S$SRCH2$otherDB[2] <<- esc(str_sub(input$otherDB, 1, 60))        # VARCHAR(60) in SQL
print(S$SRCH2$otherDB[2])
   }
   S$SRCH2$beginDate[2]  <<- input$searchDates[1]
   S$SRCH2$endDate[2]    <<- input$searchDates[2]
   # S$SRCH2$terms[2] handled above
   # S$SRCH2$query[2] handled above
   S$SRCH2$CFchosen[2]   <<- input$citeFormat
   # $CFactual filled in by citeFile
   if(!is.null(input$citeLevel)) {
      S$SRCH2$citeLevel[2]  <<- which(c("Level 1 only", "Level 2 only", "Both") %in% input$citeLevel)
   }
   # $citesL1  filled in by citeFile
   # $citesL2  filled in by citeFile
   # $fileName filled in by citeFile
   # $fileSize filled in by citeFile
   # $fileType filled in by citeFile
   # $fileTime filled in by citeFile
   # $fileRaw  filled in by citeFile???
   # S$SRCH2$comment[2] handled above
   S$SRCH2$beginDate[2]  <<- as.character(input$searchDates[1])
   S$SRCH2$endDate[2]    <<- as.character(input$searchDates[2])
   if(S$saveSearch) {
      msg=""
      if(input$searchName=="") {
         msg = paste0(msg, "<li>Your Search Name can't be blank.</li>")
      }
      if(nchar(msg)) {                    # No render; put this on top of existing render
         S$modal_title <<- "Whoops!"
         S$modal_text <<- HTML("<p>Can't save:<ul>", msg, "</ul></p>")
         rv$modal_warning <- rv$modal_warning + 1
      } else {
         S$SRCH2 <<- recSave(S$SRCH2, db=S$db)
         S$hideMenus <<- FALSE            # Back to regular programming
         rv$menuActive = 1
      }
   } else {                               # Not a save, just a render
      rv$render = rv$render + 1           # whether msg or not, next step is to render the page
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
                     citeLevelRadios = {
                        citeSelected = c("Level 1 only", "Level 2 only", "Both")[S$SRCH2$citeLevel[2]]
                        if(!is.null(input$database) && input$database == "Web of Knowledge") {
                        radioButtons("citeLevel", label="Citations to include",
                           choices=list("Level 1 only", "Level 2 only", "Both"),
                           selected=citeSelected, inline=TRUE)
                        } else {
                           ""
                        }
                     }
                     restOfPage =tagList(
#                         bs4("r", align="hc",
#                            bs4("c10", tagList(
#                               bs4("d", class="card bg-warning text-dark mx-auto my-4", bs4("d", class="card-body",
#                                  bs4("d", class="card-title", h4(class='text-dark', "New Searches")),
#                                  bs4("d", class="card-text", HTML(
# "<p>The information you enter here will help you remember what you did when you write up your project.",
# "But the important thing is that you can download references from citation databases and load them into Open-Meta ",
# "(like you might otherwise load them into EndNote or another reference manager) to create the list of articles you will review.",
# "<ul>",
# "<li>First select a database - the inputs on the page change depending on the database you select.</li>",
# "<li>PubMed-Live actually runs the terms you enter on PubMed (because it's open to the public) and tells you how many hits it finds.</li>",
# "<li>If you pick <b><i>Web of Knowledge</i></b>, radio buttons will appear with the label <b>Citations to Include</b>.",
#    "<i>Level 2</i> means the references cited by the Level 1 references. Level 2 is useful if your search is a very short",
#    "list of other systematic reviews on your topic, but avoid it otherwise.</li>",
# "<li>These databases all allow you to download references in several formats; choose the one that Open-Meta supports.</li>",
# "<li>If you select 'Other', you need to hand-enter the database name and select the format the downloaded references use.</li>",
# "<li>If you want to use a database that doesn't support any Open-Meta formats, let us know and we\'ll see what we can do.</li>",
# "</ul></p>",
# "<p><b>Not only that...</b></p>")
#                               )))
#                         ))),
                        bs4("r", align="hc",
                           bs4("c10", tagList(
                              h4(S$pageTitle),
                              databaseFields,
                              citeLevelRadios,
                              ttextInput("searchName", "Search Name", value=S$SRCH2$searchName[2], groupClass="w-75"),
                              dateRangeInput("searchDates", label="Date range of search",
                                       start=S$SRCH2$beginDate[2], end=S$SRCH2$endDate[2]),
                              if(S$SRCH2$CFchosen[2]=="Live") {
                                 tagList(
                                    HTML("Terms"),
                                    bs4("quill", id="terms", S$SRCH2$terms[2]),
                                    bs4("d", bs4("btn", class="mb-3", id="searchpubmed", q=c("p", "s", "b"), "Search PubMed")),
                                    if(S$SRCH2$query[2]!="") HTML("PubMed translated your Terms into this Query", "<p>", S$SRCH2$query[2], "</p>")

                                    # Make this a readonly input

                                 )
                              } else {
                                 tagList(
                                    HTML("Query"),
                                    bs4("quill", id="query", S$SRCH2$query[2]),
                                    fileInput("citeFile",
                                       paste0("Choose the Reference File you downloaded from ", S$SRCH2$database[2]),
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
                              bs4("btn", id="check", n=1, q=ifelse(S$SRCH2$status[2]==0, "r", "g"), "Check Search"),
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
#      if(nchar(input$searchName)>254) {
      if(!is.null(input$searchName) && nchar(input$searchName)>254) {
         msg = "The Search Name is limited to 254 characters."
      }
#      if(nchar(input$otherDB)>60) {
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
   if(!is.null(isolate(input$database)) && !is.null(S$SRCH2$citesL1[2]) && S$SRCH2$citesL1[2]>0) {                          # For CIW, report both Level 1 and Level 2
      if(isolate(input$database)=="Web of Knowledge") {
         hc = paste0("Level 1: ", S$SRCH2$citesL1[2], "; Level 2: ", S$SRCH2$citesL2[2])
      } else {
         hc = format(S$SRCH2$citesL1[2], big.mark=",") # Otherwise, just the level 1 number, with commas
      }
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
   S$SRCH2$fileName[2] <<- input$citeFile$name
   S$SRCH2$fileSize[2] <<- input$citeFile$size
   S$SRCH2$fileType[2] <<- input$citeFile$type
#   S$SRCH2$fileRaw[2]  <<- read_file(input$citeFile$datapath) # We'll do this later when we're sure the file is good
   S$SRCH2$fileTime[2] <<- sTime()
   msg <- ""                   # Assume no error
   fileOK <- TRUE              #   likewise
   mismatch = TRUE             # Assume an error
   S$SRCH2$CFactual[2] <<- "BAD" #    likewise
   S$SRCH2$citesL1[2] <<- 0    #    likewise
   S$SRCH2$citesL2[2] <<- 0    #    likewise
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
         S$SRCH2$citesL1[2] <<- length(r)
         if(input$citeFormat=="PMID") mismatch = FALSE
      } else {                                                 # Otherwise needs to have at least 2 lines
         if(length(r)<6) {                                     #    to test for CIW, but none of the remaining
            S$SRCH2$CFactual[2] <<- "BLANK"                               #    formats are less than 5 lines, probably more...
            mismatch = FALSE                                   # TRUE sends the wrong message
         } else {
            r <- str_sub(r,1,6)                                # Cut the strings back to first 6 characters
            TF.vec = r=="PMID- "                               #    only for the remaining tests.
            if(any(TF.vec)) {
               S$SRCH2$CFactual[2] <<- "MEDLINE or .nbib"                 # The sum tells us how many "PMID- " cells
               S$SRCH2$citesL1[2] <<- sum(TF.vec)              #    there are, which is the number of hits
               if(input$citeFormat=="MEDLINE or .nbib") mismatch = FALSE
            } else {
               TF.vec = r=="TY  - "
               if(any(TF.vec)) {
                  S$SRCH2$CFactual[2] <<- "RIS"
                  S$SRCH2$citesL1[2] <<- sum(TF.vec)           # Likewise
                  if(input$citeFormat=="RIS") mismatch = FALSE
               } else {
                  TF.vec = r=="Record"
                  if(any(TF.vec)) {
                     S$SRCH2$CFactual[2] <<- "Cochrane Central Export"
                     S$SRCH2$citesL1[2] <<- sum(TF.vec)        # Likewise
                     if(input$citeFormat=="Cochrane Central Export") mismatch = FALSE
                 } else {
                     TF.vec = str_sub(r, 1, 3)=="PT "
                     if(r[2]=="VR 1.0") {                      # This test is a little different (not any()), as
                        S$SRCH2$CFactual[2] <<- "EndNote Desktop (.ciw)"  #   .ciw files have a version number in row 2
                        S$SRCH2$citesL1[2] <<- sum(TF.vec)
                        r <- str_sub(r,1,2)                                          # take r down to 2 chars
                        while(length(i <- which(r == "  ")) > 0) { r[i] <- r[i-1] }  # fill in blanks with one above
                        S$SRCH2$citesL2[2] <<- sum(r=="CR")                          # how many CRs?
                        if(input$citeFormat=="EndNote Desktop (.ciw)") mismatch = FALSE
                     } else {
                        TF.vec = str_sub(r, 1, 1)=="@"         # All we have to go on for BibTeX is lines that start
                        if(any(TF.vec)) {                      #   with @
                           S$SRCH2$CFactual[2] <<- "BibTeX"
                           S$SRCH2$citesL1[2] <<- sum(TF.vec)
                           if(input$citeFormat=="BibTex") mismatch = FALSE
      }}}}}}}
   }
   if(S$SRCH2$CFactual[2]=="BAD") {
      msg = paste0(msg, "<li>This file is in an unknown format. It begins like this:</li></ul><pre>",
                  paste0(esc(str_sub(rf[1:6], 1, 95)), collapse="<br>"), "</pre><ul>")
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
         S$modal_text <<- HTML("You have the File Format set to ", input$citeFormat, " but the actual format ",
                               "of this file is ", S$SRCH2$CFactual[2], ". We'll assume you uploaded the correct file, ",
                               "but are you sure? You can click Browse again to upload a different file if necessary.")
         S$modal_size <<- "l"
         rv$modal_warning <- rv$modal_warning + 1
      }
   }
   rv$hitCounter = rv$hitCounter + 1    # update hits after uploading file
})

loadFile = function() {
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
            S$justArrived <<- TRUE                  # When starting a new search and when starting to edit an old search
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
      "editSearch" = {
         if(S$P$Modify) { S$hideMenus <<- TRUE }    # Don't hide them if an error message is coming up
         S$justArrived <<- TRUE                     # When starting a new search and when starting to edit an old search
         S$SRCH$id <<- n
         rv$menuActive = 2
         rv$limn = rv$limn + 1                      # Needed to hide menus
      },
      "details" = {
         switch(input$citeFormat,
            "Live" = {
               S$modal_text <<- HTML("<p>More about the Live format</p>")
            },
            "PMID" = {
               S$modal_text <<- HTML("<p>More about the PMID format</p>")
            },
            "MEDLINE or .nbib" = {
               S$modal_text <<- HTML("<p>More about the MEDLINE .nbib format</p>")
            },
            "Cochrane Central Export" = {
               S$modal_text <<- HTML("<p>More about the Cochrane format</p>")
            },
            "EndNote Desktop (.ciw)" = {
               S$modal_text <<- HTML("<p>More about the EndNote format</p>")
            },
            "RIS" = {
               S$modal_text <<- HTML("<p>More about the .ris and BibTex formats</p>")
            },
            "BibTeX" = {
               S$modal_text <<- HTML("<p>More about the .ris and BibTex formats</p>")
            },
            message(paste0("In input$js.omclick observer for details, no handler for ", input$citeFormat, "."))
         )
         S$modal_title <<- "Format Details"
         S$modal_size <<- "l"
         rv$modal_warning <- rv$modal_warning + 1
      },
      "procSearch" = {
         S$modal_title <<- "Upcoming Feature"
         S$modal_text <<- HTML("<p>This will check for duplicates and, if necessary, obtain addtional bibliographic ",
                               "details from PubMed. Then it will put the citations in the database, ready for Review.</p>")
         S$modal_size <<- "l"
         rv$modal_warning <- rv$modal_warning + 1
      },
      "searchpubmed" = {
         S$modal_title <<- "Upcoming Feature"
         S$modal_text <<- HTML("<p>Ability to search PubMed coming soon.</p>")
         S$modal_size <<- "l"
         rv$modal_warning <- rv$modal_warning + 1
      },
      "save" = {
         S$saveSearch <<- TRUE
         return(js$getEdit("terms"))
      },
      "cancel" = {
         S$hideMenus <<- FALSE                      # Back to regular programming
         rv$menuActive = 1
         rv$limn = rv$limn + 1
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)


