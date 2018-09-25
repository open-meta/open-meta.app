### open-meta.app Extract.R
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

# Pagination globals
S$PGN$itemsPerPage <- 30
#S$PGN$itemsPerPage <- 3       # For testing pagination
S$PGN$activePage <- 1
S$PGN$nPages <- 0

rv$menuActive = 1    # Start out on first sub-menu
rv$subMenu = 1

if(S$P$Msg=="") {
   output$uiMeat <- renderUI({rv$limn; isolate({
      widgets = ""
      switch(as.character(rv$menuActive),
         "1" = {
            subMenu <- ""
            restOfPage = "Dashboard Here"
         },
         "2" = {
            subMenu <- as.character(bs4("mp", id="custom", n=1:5, active=rv$subMenu, text=c("Participants", "Interventions", "Comparisons", "Outcomes", "Time Spans")))

            restOfPage = tagList(
               bs4("r", bs4("c1"), bs4("c10",
                  uiOutput("addOutcome"),
                  uiOutput("showOutcomes"),
                  uiOutput("yboxOutcomes")))
            )
            output$addOutcome <- renderUI(
               if(S$P$Modify) {                                                          # Only show Add button if
                  tagList(                                                               #   user has permission to Add
                     bs4("btn", id="addInput", q="g", "Add an Outcome"),
                     bs4("hr")
                  )
               } else {
                  ""
               }
            )
            output$showOutcomes  <- renderUI({
               S$IN$TABLE <<- "outcome"
               S$IN$NAME <<- "Outcome"
               R <- getDataChunk()
               if(R[1,1]==0) {
                  noResultsMsg <- "No Outcomes have been added to this project yet."
                  tagList(
                     bs4("r",
                        bs4("c12", HTML0("<h5>", noResultsMsg, "</h5>")),
                        bs4("c12", bs4("hr0", class="pb-4"))
                     )
                  )
               } else {
                  # Set up buttons
                  if(S$P$Modify) {                                         # Button vector construction from here...
                     btnid = "editOutcome"
                     btnq = "g"
                     btnlabel = "Edit"
                  } else {
                     btnid = "viewOutcome"
                     btnq = "b"
                     btnlabel = "View"
                  }
                  pattern = rep("XxX", nrow(R))
                  btn = bs4("btn", id=btnid, n="XxX", q=btnq, btnlabel)    # ... to next line
                  R[,"btn"] = str_replace_all(btn, pattern, as.character(S$IN$CHUNKS[[S$PGN$activePage]]))   # str_replace is vectorized
                  tagList(
                     bs4("pgn", np=S$PGN$nPages, ap=S$PGN$activePage),
# This does the entire table with one vectorized paste0(). R is a tibble and its columns are vectors.
#    The "collapse" at the end creates one long string. R[3] is the value column and R[4] is the button column.
# In this particular example, there's one row with a col-11 containing all the data, using <br> to start new
#    lines, and col-1 for the button. Note that cites$btn isn't stored in MySQL, but is added to "cites" above.
HTML(paste0(
'<div class="row justify-content-center">
   <div class="col-11">
      ', R$value,'
   </div>
   <div class="col-1">
      ', R$btn, '
   </div>
   ', bs4('c12', bs4('hr')), '
</div>', collapse = '')),     # End of paste0()
                     bs4("pgn", np=S$PGN$nPages, ap=S$PGN$activePage)
                  )
               }
            })  # end of render

            output$yboxOutcomes = renderUI(tagList(
               bs4("r", class="mt-3", bs4("c12", bs4("cd", q="y", bs4("cdb", bs4("cdt", HTML0(
"<p>On this page, enter all the Outcomes that meet the criteria of your project. You should edit <i>All Outcomes</i>
if your project is interested only in specific Outcomes, or leave it if you'll accept any outcome.</p>
<p>Your Principal Investigator may have customized the Outcome form in the Members & Settings menu to collect
additional information on each Outcome.</p>
"))))))
         ))
         },
         "3" = {
            subMenu <- ""
            restOfPage = "Trials Here"
         },
         "10" = {                                     # edit an item
            S$IN$FORM <<- imGetFORM(S$IN$TABLE)           # S$IN$TABLE is set when preparing multi-item view
            if(S$IN$recID>0) {                            # If this is an edit, get the FORM's current values
               R <- recGet(S$db, S$IN$TABLE, c("name", "value"), tibble(c(paste0(S$IN$TABLE,"NUM"), "=", imID2NUM())))
               for(i in 1:nrow(S$IN$FORM)) {              # Insert values from R into form$value
                  S$IN$FORM$value[i] <<- R$value[R$name==S$IN$FORM$name[i]]    # In FORM, "name" is the short label
               }                                                               # In R, it's the "name" of the "value"
            }
            if(S$P$Modify) {
               SaveBtn = HTML0(bs4("btn", id="save", n=1, q="b", "Save Details"))
            } else {                                      # If user can't modify inputs, force View (disabled inputs)
               S$IN$FORM$disable <<- TRUE                 #    and skip the Save button
            }
            restOfPage = tagList(
               imForm2HTML(S$IN$FORM),
               bs4("d", class="text-right mt-3",
                  bs4("btn", id="cancel", n=1, q="b", "Cancel"),
                  SaveBtn
               )
            )
         }
      )

      pageMenu = {
         if(S$hideMenus) {
            ""
         } else {
            tagList(
               bs4("md", id="sub", n=1:4, active=rv$menuActive, text=c("Dashboard", "PICO Setup", "Trials", "Citation List")),
               HTML0(subMenu),
               bs4("dx", style="height:1.5rem")
            )
         }
      }
      return(tagList(
         bs4("r", align="hc",
            bs4("c10", tagList(
               pageMenu,
               widgets,
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
         S$hideMenus <<- FALSE
         S$PGN$activePage <- 1                    # When changing submenu, set scroller back to 1
         rv$menuActive = n
         rv$limn = rv$limn+1
      },
      "pgn" = {
         S$PGN$activePage <<- as.numeric(n)
         rv$limn = rv$limn+1
      },
      "addInput" = {
         S$IN$recID <<- 0
         rv$menuActive = 10
         S$hideMenus <<- TRUE
         rv$limn = rv$limn+1
      },
      "editOutcome" = {
         S$IN$recID <<- n
         rv$menuActive = 10
         S$hideMenus <<- TRUE
         rv$limn = rv$limn+1
      },
      "viewOutcome" = {
         S$IN$recID <<- n
         rv$menuActive = 10
         S$hideMenus <<- FALSE
         rv$limn = rv$limn+1
      },
      "save" = {
         rv$imGetFORMData <<- rv$imGetFORMData + 1
         S$hideMenus <<- FALSE
         rv$menuActive = 2
      },
      "cancel" = {
         S$hideMenus <<- FALSE
         rv$menuActive = 2
         rv$limn = rv$limn+1
      },
      # "view" = {
      #    S$modal_title <<- "Under Construction."
      #    S$modal_text <<- HTML("<p>Sorry, this feature isn't available yet.</p>")
      #    rv$modal_warning <- rv$modal_warning + 1
      # },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)


# These two globals need to be set up before calling this function
#    S$IN$TABLE <<- "outcome"
#    S$IN$NAME <<- "Outcome"
getDataChunk <- function() {
   tableID <- paste0(S$IN$TABLE,"ID")
# First get all of tableIDs for the rows that have the name to display in the scroller
   R <- recGet(S$db, S$IN$TABLE, tableID, tibble(c("name", "=", S$IN$NAME)))
# If there aren't any, just set firstOne flag
   if(R[1,1]==0) {
      S$IN$flag$firstOne <<- TRUE
   } else {
# Set up Chunking and Pagination variables
      S$IN$flag$firstOne <<- FALSE
      S$IN$IDs <<- as.integer(R$outcomeID)
      S$IN$CHUNKS <<- chunker(S$IN$IDs, S$PGN$itemsPerPage)
      S$PGN$nPages <<- length(S$IN$CHUNKS)
      if(S$PGN$activePage > S$PGN$nPages) {                              # This can happen during filtering
         S$PGN$activePage <<- 1
      }
# Now get the data (value field) for this chunk
      R = recGet(S$db, S$IN$TABLE, "value", tibble(c(tableID, " IN ",    # Get data for this chunk
            paste0("(", paste0(S$IN$CHUNKS[[S$PGN$activePage]], collapse=","), ")"))))
   }
   return(R)
}




