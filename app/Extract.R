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

S$PGN$itemsPerPage <- 30       # Are these the only pagination things we need???
S$PGN$activePage <- 1

rv$menuActive = 1    # Start out on first sub-menu

if(S$P$Msg=="") {
   output$uiMeat <- renderUI({rv$limn; isolate({
      widgets = ""
      switch(as.character(rv$menuActive),
         "1" = {
            restOfPage = "Dashboard Here"

         },
         "2" = {
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
               noResultsMsg <- "No Outcomes have been added to this project yet."
               S$IN$TABLE <<- "outcome"
               WHERE = tibble(c("name", "=", "Outcome"))
               R <- recGet(S$db, S$IN$TABLE, paste0(S$IN$TABLE,"ID"), WHERE)             # Get all record IDs
               if(R[1,1]==0) {
                  S$IN$flag$firstOne <<- TRUE
                  tagList(
                     bs4("r",
                    #    bs4("c12", bs4("hr0", class="pb-4")),
                        bs4("c12", HTML0("<h5>", noResultsMsg, "</h5>")),
                        bs4("c12", bs4("hr0", class="pb-4"))
                     )
                  )
               } else {
                  S$IN$flag$firstOne <<- FALSE
                  IDs <- as.integer(R$outcomeID)
                  chunkedIDs <- chunker(IDs, S$PGN$itemsPerPage)
                  pageCount <- length(chunkedIDs)
                  if(S$PGN$activePage>pageCount) {                          # This can happen during filtering
                     S$PGN$activePage <<- 1
                  }
                  SELECT = c("value")
                  R = recGet(S$db, S$IN$TABLE, SELECT, tibble(c("outcomeID", " IN ",
                        paste0("(", paste0(chunkedIDs[[S$PGN$activePage]], collapse=","), ")"))))
                  if(S$P$Modify) {
                     btnid = "editOutcome"
                     btnq = "g"
                     btnlabel = "Edit"
                  } else {
                     btnid = "viewOutcome"
                     btnq = "b"
                     btnlabel = "View"
                  }
                  pattern = rep("XxX", nrow(R))
                  btn = bs4("btn", id=btnid, n="XxX", q=btnq, btnlabel)
                  R[,"btn"] = str_replace_all(btn, pattern, as.character(chunkedIDs[[S$PGN$activePage]]))   # str_replace is vectorized
                  tagList(
                     bs4("r",
                        bs4("c12", bs4("hr0", class="pb-4")),
                        bs4("pgn", np=pageCount, ap=S$PGN$activePage),
                        bs4("c12", bs4("hr0", class="pb-4"))
                     ),
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
                     bs4("r",
#                        bs4("c12", bs4("hr0", class="pb-4")),       # last row of table provides this <hr>
                        bs4("pgn", np=pageCount, ap=S$PGN$activePage),
                        bs4("c12", bs4("hr0", class="pb-4"))
                     )
                  )
               }
            })  # end of render
            output$yboxOutcomes = renderUI(tagList(
               bs4("r", class="mt-3", bs4("c12", bs4("cd", q="y", bs4("cdb", bs4("cdt", HTML0(
"<p>If you are a member of this project with appropriate permissions, you can </p>
"))))))
         ))
         },
         "3" = {
            restOfPage = "Trials-Arms-Groups Here"
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

      # Javascript notes:
      # $('#tinid')[0].value gets what's in a text input
      # $('#cbxid')[0].checked gets status of a checkbox

      # better yet, use input[[dynamicName]] for input$staticName

      pageMenu = {
         if(S$hideMenus) {
            ""
         } else {
            bs4("md", id="sub", n=1:3, active=rv$menuActive, text=c("Dashboard", "Outcomes", "Trials-Arms-Groups"))
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
