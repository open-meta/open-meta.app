### open-meta.app adminForms.R
### Tom Weishaar - Sep 2018 - v0.1

# No chokidar.R because we're not in a specific project
S$P$SA   <-  S$U$sPowers > 899
S$P$Modify <- S$P$SA           # Flag for whether the user can edit on this page
S$P$Msg  <- ""                 # Error flag and message

# load the inputMeta.R code (also used by other pages)
source("inputMeta.R", local=TRUE)

# pickR globals
S$PKR$itemsPerPage <- 30
#S$PKR$itemsPerPage <- 3       # For testing pagination
S$PKR$Forms$activePage <- 1

rv$menuActive = 1              # Start out on first sub-menu
rv$limnForms = 1

S$db = "om$prime"
S$display = "open"
S$IN$flag$showAddInputButton <- TRUE
S$IN$view <- "Look and Feel"

# load the inputMeta.R code (also used by other pages)
source("inputMeta.R", local=TRUE)

output$uiHead <- renderUI({rv$limn; isolate({
   if(rv$limn) {
      if(debugON) {
         cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
      }
      if(S$U$sPowers < S$PG$spReq) {   # Although users without superpowers can't get here via the
         return(tagList(               #    menus, they CAN get here by typing in the URL.
            slimHead,
            bs4("hrt", "You haven't been granted access to this page.")
         ))
      } else {
      if(S$hideMenus) {
         return(slimHead)
      } else {
         text=c("Control Panel","Users", "Pages", "Projects", "User Projects", "Project Users", "Forms")
         links=c("?adminCP", "?adminUsers", "?adminPages", "?adminProjects", "?adminUserPrj", "?adminPrjUser", "?adminForms")
            active=which(links %in% paste0("?", S$PG$pageName))
            if(length(active)==0) {active=0}
            return(tagList(
               slimHead,
               bs4("hrm", text=text, links=links, active=active)))
         }
      }
   }
})})

output$uiMeat <- renderUI({c(rv$limn); isolate({
   if(rv$limn) {
      switch(S$display,
         "open" = {
            restofpage <-
               tagList(
                  bs4("r", align="hc",
                     bs4("c7",
                     bs4("r", id="newBTN"),
                     bs4("r", id="pickR")
                  ))
               )
         },
         "newForm" = {
            restofpage <-
               tagList(
                  bs4("r", align="hc",
                     bs4("c7",
                     bs4("r", id="newForm")
                  ))
               )
         },
         "editForm" = {
            restofpage <-
               tagList(
                  bs4("r", align="hc",
                     bs4("c10",
                     bs4("r", id="editForm")
                  ))
               )
         }
      )
      return(restofpage)
   }
})})

output$newBTN <- renderUI({rv$limn; isolate({
   tagList(
      bs4("c12",
         HTML0('<h5 class="mb-4">Create or Edit System-Wide Forms</h5>'),
         bs4("btn", id="newForm", q="g", class="mb-4", "Create a New Form")
      )
   )
})})

###########
###
### pickR FUNCTION CLASSIC EXAMPLE
###
###
output$pickR <- renderUI({c(rv$limn, rv$limnForms); isolate({              # !!!Note that rv$limnForms must be rv[[paste0("limn",ID)]]!!!
   ID = "Forms"                                                            # This allows multiple pickRs on a single page
   activePage = ifelse(is.null(S$PKR[[ID]]$activePage), 1, S$PKR[[ID]]$activePage)
   TABLE = "settings"                                                      # The table the pickR data will come from
   SELECT = "name"                                                         # These are the table fields needed to build the pickR
   WHERE = tibble(c("name", "LIKE", "%form%"))                             # This is the incoming filter
   HeadlineF = THRUb                                                       # THRUb returns "", as we have no headline
   ButtonData = list(edit=list(id=ID, q="b", class="", label="Edit Form"))
   ButtonF = stdButtons                                                    # use just the function name; no quotes, no ()
   FixDataF = THRU                                                         # the THRU function does exactly nothing
   FormatF = prf_1X1
   NOtext = "No forms found by this filter."
   itemsPerPage = S$PKR$itemsPerPage                                       # Modifiable pickR-by-pickR
   scroll = FALSE                                                          # Modifiable pickR-by-pickR
   return(pickR(ID, activePage, S$db, TABLE, SELECT, WHERE, HeadlineF, ButtonData, ButtonF, FixDataF, FormatF, NOtext, itemsPerPage, scroll))
})})
# Also needs first two items in omclick observer

output$newForm <- renderUI({rv$limn; isolate({
   r <- recGet(S$db, "settings", "value", tibble(c("name", "=", "Form-newForm")))
   tagList(
      bs4("c12",
         HTML0('<h5 class="my-4">Create a new form</h5>'),
#         bs4("tin", id="newForm", "Form name"),                           # Need this when there's no Form-newForm yet
         imForm2HTML(fromJSON(r$value)),
         bs4("r", align="he", bs4("c3",
            HTML0('<div class="text-right mt-3">'),
            bs4("btn", id="cancelAdd", n=1, q="b", "Cancel"),
            bs4("btn", id="saveAdd", n=1, q="b", "Save"),
            HTML0('</div>')
         )),
         bs4("r", bs4("c12", bs4("hr")))
      )
   )
})})

output$editForm <- renderUI({rv$limn; isolate({
   return(
      tagList(
         bs4("c12",
            bs4("hr"),
            HTML0('<h5 class="mb-4">Edit System-Wide Forms</h5>'),
            output$modifyInputs <- renderUI(imModifyInputs()),
            output$showInputs   <- renderUI(imShowInputs()),
            bs4("btn", id="cancelAdd", n=2, q="b", "Cancel")
         )
      )
   )
})})

### observer for omclick
observeEvent(input$js.omclick, {
   if(debugON) {
      cat(paste0("Click on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]                # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]                #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "pgn" = {                    # For pgn, [2] or n is the form name, [3] is the recID
         S$PKR[[n]]$activePage <<- as.numeric(uid[[1]][3])
         limnID = paste0("limn", n)         # The pickR render should respond to this rv$limn...;
         rv[[limnID]] = rv[[limnID]] + 1    # rv$limn... also needs to be pre-defined at the top of the script
      },
      "Forms" = {                  # Edit Button, "Forms" is the ID of one of the pickRs on this page; this loads the selected form
         r <-recGet(S$db, "settings", c("name","value"), tibble(c("settingsID", "=", n)))
         S$IN$FORMname <<- r$name
         S$IN$FORM <<- as.tibble(fromJSON(r$value))           # Yes, unJSONize as it's a tibble
         S$IN$flag$showAddInputButton <<- TRUE
         S$display <<- "editForm"
         S$hideMenus <<- TRUE
         rv$limn <- rv$limn +1     # Need to limn at this level to hideMenus
      },
      "newForm" = {
         S$IN$flag$showAddInputButton <<- TRUE
         S$display <<- "newForm"
         S$hideMenus <<- TRUE
         rv$limn <- rv$limn +1     # Need to limn at this level to hideMenus
      },
      "saveAdd" = {
         f = str_trim(stripHTML(input$newForm))
         r = recGet(S$db, "settings", "**", tibble(c("name", "=", f)))
         if(r[[1,1]]==0) {
            r$name[2] <- f
            r$value[2] <- toJSON(imGetBlankFORMrow("text"))
            r = recSave(r)
            S$display <<- "open"
            S$hideMenus <<- FALSE
            rv$limn <- rv$limn +1   # Need to limn at this level to un-hideMenus, also to display added Form
         } else {
            S$modal_title <<- "Whoops"
            S$modal_text <<- HTML("<p>That name is taken.</p>")
            S$modal_size <<- "s"
            rv$modal_warning <- rv$modal_warning + 1
         }
      },
      "cancelAdd" = {
         S$display <<- "open"
         S$hideMenus <<- FALSE
         rv$limn <- rv$limn +1      # Need to limn at this level to un-hideMenus
      },
      "saveEdit" = {
         S$IN$flag$showAddInputButton <<- TRUE

      },
      "addInput" = {                # This is the big green Add button
         S$IN$flag$showAddInputButton <<- FALSE
         S$hideMenus <<- TRUE
         S$IN$flag$editingForm <<- FALSE
         S$IN$FORMrow <<- imGetBlankFORMrow(S$IN$codeTypes[S$IN$inputType])
         S$IN$FORMrowform <<- imGetBlankform(S$IN$codeTypes[S$IN$inputType])
         S$IN$FORMrowform$order <<- 1:nrow(S$IN$FORMrowform)
         rv$limn = rv$limn + 1
      },
      "editMe" = {                  # This is the green Edit button
         if(S$P$SA || (S$P$Modify && !S$IN$FORM[[n, "locked"]])) {
            S$IN$flag$showAddInputButton <<- FALSE
            S$hideMenus <<- TRUE
            S$IN$flag$editingForm <<- TRUE                                       # disable selector, among other things
            S$IN$FORMrow <<- S$IN$FORM[n,]
            S$IN$FORMrowform <<- imFORMrow2form(S$IN$FORMrow)                    # expand form row into a form
            S$IN$inputType <<- which(S$IN$codeTypes %in% S$IN$FORM[[n,"type"]])  # get type of input for selector
            rv$limn = rv$limn + 1
         }
      },
      "saveInput" = {               # This button is on the output$modifyAnInput screen
         if(S$P$Modify && imFormValidates()) {
            print("Saving in saveInput")
            imSaveform2FORMrow()
            S$IN$flag$showAddInputButton <<- TRUE
            S$IN$settingsName
            S$hideMenus <<- FALSE
            saveDB <- S$db
            imSaveFORM()
            rv$limn = rv$limn + 1
         }
      },
      "cancelInput" = {             # This button is on the output$modifyAnInput screen
         S$IN$flag$showAddInputButton <<- TRUE
         S$hideMenus <<- FALSE
         rv$limn = rv$limn + 1
      },
      "deleteMe" = {
         if(S$P$Modify && !S$IN$FORM[[n,"locked"]]) {
            dbLink <- poolCheckout(shiny.pool)                              # When deleting an input, we also need to delete
            on.exit(poolReturn(dbLink), add = TRUE)                         #   its id from the ids table
            r = dbExecute(dbLink, paste0("DELETE FROM `", S$db, "`.`ids` WHERE idsID='", S$IN$FORM[n, "id"], "';"))
            S$IN$FORM <<- S$IN$FORM[-as.numeric(n),]                        # Delete the n row from FORM
            imSaveFORM()                                                    # Save the FORM
            rv$limn = rv$limn + 1
         }
      },
      "upMe" = {
         if(S$P$Modify) {
            S$IN$FORM[n,"order"] <<- S$IN$FORM[n,"order"] - 1.5
            S$IN$FORM <<- imFixOrder(S$IN$FORM)
            imSaveFORM()
            rv$limn = rv$limn + 1
         }
      },
      "downMe" = {
         if(S$P$Modify) {
            S$IN$FORM[n,"order"] <<- S$IN$FORM[n,"order"] + 1.5
            S$IN$FORM <<- imFixOrder(S$IN$FORM)
            imSaveFORM()
            rv$limn = rv$limn + 1
         }
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)
