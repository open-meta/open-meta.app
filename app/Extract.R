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

rv$menu1Active <- 1
rv$menu2Active <- 1

# pickR globals
S$PKR$itemsPerPage <- 30
#S$PKR$itemsPerPage <- 3       # For testing pagination

S$PKR$P$activePage <- 1
S$PKR$I$activePage <- 1
S$PKR$C$activePage <- 1
S$PKR$O$activePage <- 1
S$PKR$TS$activePage <- 1

S$picoName = ""
S$display = "add"

print(paste0("At page entry, rv$limn is ", rv$limn))

output$uiMeat <- renderUI({c(rv$limn); isolate({
   if(rv$limn && S$P$Msg=="") {
      switch(as.character(rv$menu1Active),
         "1" = { return(
            tagList(
               bs4("r", align="hc",
                  bs4("c10",
                  bs4("r", id="pageMenu"),
                  bs4("r", id="Dashboard")
               ))
            ))
         },
         "2" = { return(
            tagList(
               bs4("r", align="hc",
                  bs4("c10",
                  bs4("r", id="pageMenu"),
                  bs4("r", id="TrialSetup")
               ))
            ))
         },
         "3" = { return(
            tagList(
               bs4("r", align="hc",
                  bs4("c10",
                  bs4("r", id="pageMenu"),
                  bs4("r", id="PICOSetup")
               ))
            ))
         },
         "4" = { return(
            tagList(
               bs4("r", align="hc",
                  bs4("c10",
                  bs4("r", id="pageMenu"),
                  bs4("r", id="Extraction")
               ))
            ))
         }
      )
   }
})})

output$pageMenu <- renderUI({c(rv$limn); isolate({
   menu1 <- c("Dashboard", "Trial Setup", "PICO(T) Setup", "Extraction List")
   menu2 <- c("Participants", "Interventions", "Comparisons", "Outcomes", "Time Spans")
   subMenu <- ifelse(rv$menu1Active!=3, "",
                  as.character(bs4("mp", id="menu2", n=1:length(menu2), active=rv$menu2Active, text=menu2)))
   return(
      if(S$hideMenus) {
         ""
      } else {
         tagList(
            bs4("c12",
               bs4("md", id="menu1", n=1:length(menu1), active=rv$menu1Active, text=menu1),
               HTML0(subMenu),
               bs4("dx", style="height:1.5rem")
            )
         )
      }
   )
})})

output$Dashboard <- renderUI({c(rv$limn); isolate({
   return(
      tagList(
         bs4("c12", "Dashboard Here")
      )
   )
})})

output$TrialSetup <- renderUI({c(rv$limn); isolate({
   return(
      tagList(
         bs4("c12", "Trial Setup Here")
      )
   )
})})

output$PICOSetup <- renderUI({c(rv$limn); isolate({
   if(S$display=="add") {
      return(
         tagList(
            bs4("c12", id="picoAdd"),
            bs4("c12", id="picoPickR"),
            bs4("c12", id="picoYbox")
         )
      )
   }
   if(S$display=="edit") {
      return(
         tagList(
            bs4("c12", id="editPico"),
            bs4("c12", id="picoYbox")
         )
      )
   }
})})

output$picoAdd <- renderUI({c(rv$limn); isolate({
   switch(as.character(rv$menu2Active),
      "1" = {
         btnLabel = "Add a participant group"
      },
      "2" = {
         btnLabel = "Add an intervention"
      },
      "3" = {
         btnLabel = "Add a comparison"
      },
      "4" = {
         btnLabel = "Add an outcome"
      },
      "5" = {
         btnLabel = "Add a time span"
      }
   )
   return(
      if(!S$P$Modify) {                   # Only show Add button if user has permission to Add
         ""
      } else {
         tagList(
            bs4("btn", id="addPico", q="g", btnLabel),
            bs4("hr")
         )
      }
   )
})})

# !!!Note that rv$limnForms must be rv[[paste0("limn",ID)]]!!!
output$picoPickR <- renderUI({c(rv$limn, rv$limnP, rv$limnI, rv$limnC, rv$limnO, rv$limnTS); isolate({
   switch(as.character(rv$menu2Active),
      "1" = {
         S$picoName <<- c("Participant", "PrjForm-Participant Grp")    # Name in pico table, form name in settings table
      },
      "2" = {
         S$picoName <<- c("Intervention", "PrjForm-Intervention")
      },
      "3" = {
         S$picoName <<- c("Comparison", "PrjForm-Comparison")
      },
      "4" = {
         S$picoName <<- c("Outcome", "PrjForm-Outcome")
      },
      "5" = {
         S$picoName <<- c("TimeSpan", "PrjForm-Time Span")
      }
   )
   ID = "Pico"
   activePage = ifelse(is.null(S$PKR[[ID]]$activePage), 1, S$PKR[[ID]]$activePage)
   TABLE = "pico"                                                          # The table the pickR data will come from
   SELECT = c("name","value")                                              # These are the table fields needed to build the pickR
   WHERE = tibble(c("name", "=", S$picoName[1]))
   HeadlineF = THRUb                                                       # THRUb returns "", as we have no headline
   if(S$P$Modify) {                                                        # View or Edit depends on permissions
     ButtonData <- list(edit=list(id=paste0("edit", ID), q="g", class="mr-2", label="Edit"),
                      delete=list(id=paste0("delete", ID), q="r", label="Delete"))
   } else {
     ButtonData <- list(view=list(id=paste0("view",ID), q="b", label="View"))
   }
   ButtonF = stdButtons
   FixDataF = picoFix
   FormatF = prf_1X1
   NOtext = "Nothing here yet."
   itemsPerPage = S$PKR$itemsPerPage                                       # Modifiable pickR-by-pickR
   scroll = FALSE                                                          # Modifiable pickR-by-pickR
   return(pickR(ID, activePage, S$db, TABLE, SELECT, WHERE, HeadlineF, ButtonData, ButtonF, FixDataF, FormatF, NOtext, itemsPerPage, scroll))
})})
# Also needs first two items in omclick observer

picoFix <- function(x) {
   x <- x[,-2]            # delete "name" column, leave "values"
   x[1:2,"Action"] = ""   # delete buttons from first two rows (No Response and DNMPC)
   return(x)
}

output$picoYbox <- renderUI({c(rv$limn); isolate({
   switch(as.character(rv$menu2Active),
      "1" = {
         yBox = HTML0(
"<p>If you plan to do secondary analysis comparing different participant groups, you'll need to
create a category for each group here. For example, if your project allows studies with either children or
adults, you could change <i>Eligible participant group</i> to <i>Participants younger than 18</i>
and add a Participant group for <i>Participants 18 and older</i>. You might also need a category
for <i>Ages combined or unspecificed</i>, depending on whether such groups meet the criteria of your project.</p>
<p>Your Principal Investigator may have customized the interventions group form in the Members & Settings menu to collect
additional information on each participant group.</p>"

         )
      },
      "2" = {
         yBox = HTML0(
"<p>If your project is about comparing different interventions, create a category for each eligible intervention
here. For example, if you intend to compare different dosages of a drug, create a category for each dose size.</p>
<p>Your Principal Investigator may have customized the interventions form in the Members & Settings menu to collect
additional information on each intervention.</p>"

         )
      },
      "3" = {
         yBox = HTML0(
"<p>It is unusual to investigate different comparisons, but you may be interested in whether control groups with
no intervention, a placebo intervention, or a standard-of-care intervention make a difference in your project's
research domain. If so, enter a category for each type of control group you are interested in here.</p>
<p>Your Principal Investigator may have customized the comparisons form in the Members & Settings menu to collect
additional information on each comparison.</p>"
         )
      },
      "4" = {
         yBox = HTML0(
"<p>Enter all the outcomes that meet the criteria of your project. Typically you would change <i>Eligible outcome</i>
to a specific eligible outcome, such as a score on specific test. Then you'd add all the other specific outcomes your
project is interested in.</p>
<p>Your Principal Investigator may have customized the outcome form in the Members & Settings menu to collect
additional information on each outcome.</p>"
         )
      },
      "5" = {
         yBox = HTML0(
"<p>The meaning of a time span differs depending on whether your project's intervention happens one time, like
surgery, or continuously, like taking a drug or supplement. For one time interventions, the time span would
refer to the time between the intervention and outcome measurement. For continuous interventions, it refers
to the time between the intervention <i>start</i> and outcome measurement. In this case, your project might consider
an outcome measurement taken after the intervention has stopped as either eligible or ineligible.</p>
<p>Your Principal Investigator may have customized the time span form in the Members & Settings menu to collect
additional information on each time span.</p>"
         )
      }
   )
   return(tagList(
      bs4("r", class="mt-3", bs4("c12", bs4("cd", q="y", bs4("cdb", bs4("cdt", yBox)))))
   ))
})})

output$editPico <- renderUI({c(rv$limn); isolate({
   S$IN$FORM <<- imGetFORM(S$picoName[2])        # S$picoName is set in picoPickR
   if(S$recID>0) {                               # If this is an edit, get the FORM's current values; S$recID set in addPico, editPico
      R <- recGet(S$db, "pico", c("name", "value"), tibble(c("picoNUM", "=", imID2NUM(S$recID, "pico"))))
      for(i in 1:nrow(S$IN$FORM)) {              # Insert values from R into form$value
         S$IN$FORM$value[i] <<- R$value[R$name==S$IN$FORM$name[i]]    # In FORM, "name" is the short label
      }                                                               # In R, it's the "name" of the "value"
   }
   if(S$P$Modify) {
      SaveBtn = HTML0(bs4("btn", id="save", n=1, q="b", "Save"))
#      DeleteBtn = HTML0(bs4("btn", id="delete", n=1, q="r", "Delete"))
   } else {                                      # If user can't modify inputs, force View (disabled inputs)
      S$IN$FORM$disable <<- TRUE                 #    and skip the Save button
      SaveBtn = ""
#      DeleteBtn = ""
   }
   restOfPage = tagList(
      imForm2HTML(S$IN$FORM),
      bs4("r", align="he",
          bs4("c3", bs4("btn", id="cancel", n=1, q="b", "Cancel"), SaveBtn)
          # bs4("c3", bs4("btn", id="cancel", n=1, q="b", "Cancel"), SaveBtn),
          # bs4("c1", DeleteBtn)
      )
   )
})})




output$Extraction <- renderUI({c(rv$limn); isolate({
   return(
      tagList(
         bs4("c12", "Extraction List Here")
      )
   )
})})





# output$showOutcomes  <- renderUI({
#    R <- getPICOname()
#    if(R[1,1]==0) {
#       noResultsMsg <- paste0("No ", S$IN$picoName[2], " have been added to this project yet.")
#       tagList(
#          bs4("r",
#             bs4("c12", HTML0("<h5>", noResultsMsg, "</h5>")),
#             bs4("c12", bs4("hr0", class="pb-4"))
#          )
#       )
#    } else {
#       # Set up buttons
#       if(S$P$Modify) {                                         # Button vector construction from here...
#          btnid = "editPico"
#          btnq = "g"
#          btnlabel = "Edit"
#       } else {
#          btnid = "viewPico"
#          btnq = "b"
#          btnlabel = "View"
#       }
#       pattern = rep("XxX", nrow(R))
#       btn = bs4("btn", id=btnid, n="XxX", q=btnq, btnlabel)    # ... to next line
#       R[,"btn"] = str_replace_all(btn, pattern, as.character(S$IN$CHUNKS[[S$PGN$activePage]]))   # str_replace is vectorized
#       tagList(
#          bs4("pgn", np=S$PGN$nPages, ap=S$PGN$activePage),
# # This does the entire table with one vectorized paste0(). R is a tibble and its columns are vectors.
# #    The "collapse" at the end creates one long string. R[3] is the value column and R[4] is the button column.
# # In this particular example, there's one row with a col-11 containing all the data, using <br> to start new
# #    lines, and col-1 for the button. Note that cites$btn isn't stored in MySQL, but is added to "cites" above.
# HTML(paste0(
# '<div class="row justify-content-center">
#    <div class="col-11">
#       ', R$value,'
#    </div>
#    <div class="col-1">
#       ', R$btn, '
#    </div>
#    ', bs4('c12', bs4('hr')), '
# </div>', collapse = '')),     # End of paste0()
#          bs4("pgn", np=S$PGN$nPages, ap=S$PGN$activePage)
#       )
#    }
# })  # end of render showOutcomes

#          },
#          "4" = {
#             subMenu <- ""
#             restOfPage = "Extraction List Here"
#          },
#          "10" = {                                     # edit an item
            # S$IN$FORM <<- imGetFORM(S$IN$picoName[1])                   # Sets S$IN$FORM; S$IN$picoName[1] is set in submenu switch()
            # if(S$recID>0) {                            # If this is an edit, get the FORM's current values; S$recID set in addPico, editPico
            #    R <- recGet(S$db, "piconame", c("name", "value"), tibble(c(paste0("pico","NUM"), "=", imID2NUM())))
            #    for(i in 1:nrow(S$IN$FORM)) {              # Insert values from R into form$value
            #       S$IN$FORM$value[i] <<- R$value[R$name==S$IN$FORM$name[i]]    # In FORM, "name" is the short label
            #    }                                                               # In R, it's the "name" of the "value"
            # }
            # if(S$P$Modify) {
            #    SaveBtn = HTML0(bs4("btn", id="save", n=1, q="b", "Save"))
            #    DeleteBtn = HTML0(bs4("btn", id="delete", n=1, q="r", "Delete"))
            # } else {                                      # If user can't modify inputs, force View (disabled inputs)
            #    S$IN$FORM$disable <<- TRUE                 #    and skip the Save button
            #    SaveBtn = ""
            #    DeleteBtn = ""
            # }
            # restOfPage = tagList(
            #    imForm2HTML(S$IN$FORM),
            #    bs4("r", bs4("c8"),
            #        bs4("c3", bs4("btn", id="cancel", n=1, q="b", "Cancel"), SaveBtn),
            #        bs4("c1", DeleteBtn)
            #    )
#                # bs4("d", class="text-right mt-3",
#                #    bs4("btn", id="cancel", n=1, q="b", "Cancel"),
#                #    SaveBtn
#                # )
#             )
#          }
#       )
#
#       pageMenu = {
#          if(S$hideMenus) {
#             ""
#          } else {
#             tagList(
#                bs4("md", id="sub", n=1:4, active=rv$menuActive, text=c("Dashboard", "Trial Setup", "PICO(T) Setup", "Extraction List")),
#                HTML0(subMenu),
#                bs4("dx", style="height:1.5rem")
#             )
#          }
#       }
#       return(tagList(
#          bs4("r", align="hc",
#             bs4("c10", tagList(
#                pageMenu,
#                restOfPage
#             ))
#          )
#       ))
#    })})
# }

### observer for omclick
observeEvent(input$js.omclick, {
   if(debugON) {
      cat(paste0("Click on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "menu1" = {
         S$hideMenus <<- FALSE
#         S$PGN$activePage <- 1                    # When changing submenu, set scroller back to 1
         rv$menu1Active = n
         rv$limn = rv$limn+1
      },
      "menu2" = {
         S$hideMenus <<- FALSE
         S$display <<- "add"
#         S$PGN$activePage <- 1                    # When changing submenu, set scroller back to 1
         rv$menu2Active = n
         rv$limn = rv$limn+1
      },
      "pgn" = {                    # For pgn, [2] or n is the form name, [3] is the recID
         S$PKR[[n]]$activePage <<- as.numeric(uid[[1]][3])
         limnID = paste0("limn", n)         # The pickR render should respond to this rv$limn...;
         rv[[limnID]] = rv[[limnID]] + 1    # rv$limn... also needs to be pre-defined at the top of the script
      },
      # "P" = {                  # Edit Button, "P" is the ID of one of the pickRs on this page; this loads the selected form
      #    S$IN$FORMname <<- "inputForm-participant"
      #    r <- recGet(S$db, "settings", c("name","value"), tibble(c("name", "=", S$IN$FORMname)))
      #    f <- as.tibble(fromJSON(r$value))           # Yes, unJSONize as it's a tibble
      #    # for(i in 1:nrow(f)) {
      #    #    d <- recGet(S$db, "pico", c("name","value"), tibble(c("picoID", "=", n)))
      #    #    f$value[i] f$id[i]
      #    # }
      #
      #    S$IN$FORM$value <<- r$value
      #    S$IN$FORM <<-
      #    S$IN$flag$showAddInputButton <<- TRUE
      #    S$display <<- "edit"
      #    S$hideMenus <<- TRUE
      #    rv$limn <- rv$limn +1     # Need to limn at this level to hideMenus
      # },
      "addPico" = {
         S$recID <<- 0
         S$display <<- "edit"
         S$hideMenus <<- TRUE
         rv$limn = rv$limn+1
      },
      "editPico" = {
         S$recID <<- n
         S$display <<- "edit"
         S$hideMenus <<- TRUE
         rv$limn = rv$limn+1
      },
      "viewPico" = {
         S$recID <<- n
         S$display <<- "edit"
         S$hideMenus <<- FALSE
         rv$limn = rv$limn+1
      },
      "save" = {
         S$IN$recID <<- S$recID
         empty=0
         for(i in which(S$IN$FORM$placeholder=="Required...")) {
            empty <- empty + (input[[S$IN$FORM$id[i]]]=="")
         }
         if(empty!=0) {
            if(empty==1) {
               S$modal_text <<- HTML0("<p>You need to enter something in the required field.</p>")
            } else {
               S$modal_text <<- HTML0("<p>You need to enter something in ", empty, " required fields.</p>")
            }
            S$modal_title <<- "Whoops."
            S$modal_size <<- "m"
            rv$modal_warning <- rv$modal_warning + 1
         } else {
            rv$imGetFORMData <<- rv$imGetFORMData + 1
            S$display <<- "add"
            S$hideMenus <<- FALSE
            rv$menuActive = 2
         }
      },
      "cancel" = {
         S$display <<- "add"
         S$hideMenus <<- FALSE
         rv$menuActive = 2
         rv$limn = rv$limn+1
      },
      "delete" = {
#   Needs S$IN$flag$firstOne
#         S$IN$TABLE
#         S$recID
         NUM <- imID2NUM(S$recID, "pico")                                # We'll need this for each row, so save in variable
         for(i in 1:nrow(S$IN$FORM)) {                                   # Save FORM values
            R <-  recGet(S$db, "pico", SELECT="**",
                     WHERE=tibble(c("picoNUM", "=", NUM), c("name", "=", S$IN$FORM$name[i])))
            R$outcomeNUM[2] = NUM                                        # While edited recs will already have NUM
            R$name[2] = S$IN$FORM$name[i]                                #    and Name, must do this for new rows!
            R$value[2] = S$IN$FORM$value[i]
            R <- recSave(R, S$db)
         }
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


# # These two globals need to be set up before calling this function
# #    S$IN$picoName[1] <<- "outcome"
# getPICOname <- function() {
# # First get all of piconameIDs for the rows that have the name to display in the scroller
#    R <- recGet(S$db, "piconame", "piconameID", tibble(c("name", "=", S$IN$picoName[1])))
# # If there aren't any, just set firstOne flag
#    if(R[1,1]==0) {
#       S$IN$flag$firstOne <<- TRUE
#    } else {
# # Set up Chunking and Pagination variables
#       S$IN$flag$firstOne <<- FALSE
#       S$IN$IDs <<- as.integer(R$piconameID)
#       S$IN$CHUNKS <<- chunker(S$IN$IDs, S$PGN$itemsPerPage)
#       S$PGN$nPages <<- length(S$IN$CHUNKS)
#       if(S$PGN$activePage > S$PGN$nPages) {                              # This can happen during filtering
#          S$PGN$activePage <<- 1
#       }
# # Now get the data (value field) for this chunk
#       R = recGet(S$db, "piconame", "value", tibble(c(piconameID, " IN ",    # Get data for this chunk
#             paste0("(", paste0(S$IN$CHUNKS[[S$PGN$activePage]], collapse=","), ")"))))
#    }
#    return(R)
# }

