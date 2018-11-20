### open-meta.app adminForms.R
### Tom Weishaar - Sep 2018 - v0.1

# No chokidar.R because we're not in a specific project
S$P$SA   <-  S$U$sPowers > 899
S$P$Modify <- S$P$SA           # Flag for whether the user can edit on this page
S$P$Msg  <- ""                 # Error flag and message

# load the inputMeta.R code (also used by other pages)
source("inputMeta.R", local=TRUE)

# pickR globals
# S$PKR$itemsPerPage             Now in app.R
S$PKR$Forms$activePage <- 1

rv$menuActive = 1              # Start out on first sub-menu
rv$limnForms = 1

S$db = "om$prime"
S$display = "open"             # open, newForm, editForm
S$IN$flag$showAddInputButton <- TRUE
S$IN$view <- "Look and Feel"

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
         "open" = { return(
            tagList(
               bs4("r", align="hc",
                  bs4("c7",
                  bs4("r", id="newBTN"),
                  bs4("r", id="pickR"),
                  bs4("r", id="ybox")
               ))
            ))
         },
         "newForm" = { return(
            tagList(
               bs4("r", align="hc",
                  bs4("c7",
                  bs4("r", id="newForm"),
                  bs4("r", id="ybox")
               ))
            ))
         },
         "editForm" = { return(
            tagList(
               bs4("r", align="hc",
                  bs4("c10",
                  bs4("r", id="editForm"),
                  bs4("r", bs4("c12", id="ybox"))
               ))
            ))
         }
      )
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
   FilterF <- whereFilter                                                  # Filter function; this one just uses WHERE
   HeadlineF = THRUb                                                       # THRUb returns "", as we have no headline
   ButtonData <- list(edit=list(id=paste0("editForm"), q="g", class="mr-2", label="Edit"),
                    delete=list(id=paste0("deleteForm"), q="r", label="Delete"))
   ButtonF = stdButtons                                                    # use just the function name; no quotes, no ()
   FixDataF = THRU
   FormatF = prf_1X1
   NOtext = "No forms found by this filter."
   itemsPerPage = S$PKR$itemsPerPage                                       # Modifiable pickR-by-pickR
   scroll = FALSE                                                          # Modifiable pickR-by-pickR
   return(pickR(ID, S$db, TABLE, WHERE, FilterF, HeadlineF, SELECT, ButtonData, ButtonF,
                    FixDataF, FormatF, NOtext, activePage, itemsPerPage, scroll))
})})
# Also needs first two items in omclick observer

output$newForm <- renderUI({rv$limn; isolate({
   FORM <- imGetBlankFORMrow("text")
   FORM$id <- S$newFormNameID <<- "id110101119701111141097897109101"       # "newFormName" in utf8 code
   FORM$name <- "newFormName"
   FORM$label <- "Name of the new form"
   FORM$helptext <- "Customizable forms must begin with 'PrjForm-'. All others must begin with 'Form-'"
   FORM$value <- "Form-"
   tagList(
      bs4("c12",
         HTML0('<h5 class="my-4">Create a new form</h5>'),
         imForm2HTML(FORM),
         bs4("r", align="he", bs4("c4",
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
   BackBtn <- ""
   if(S$IN$flag$showAddInputButton) {                                      # Only show the back button if not editing an input.
      BackBtn <- bs4("btn", id="cancelAdd", n=2, q="b", "Cancel (Back to List of Forms)")
   }
   return(
      tagList(
         bs4("c12",
            bs4("hr"),
            HTML0('<h5 class="mb-4">Edit System-Wide Form: ', S$IN$FORMname, '</h5>'),
            output$modifyInputs <- renderUI(imModifyInputs()),
            output$showInputs   <- renderUI(imShowInputs()),
            BackBtn
         )
      )
   )
})})

output$ybox <- renderUI({c(rv$limn); isolate({
yBox = HTML0(
"<p>If you're here, you're a system administrator. The forms you create and edit here are saved in the om$prime.settings
file (with appropriate changes to the om$prime ids file).</p>
<p>When a user starts a new project, the forms with names that begin <i>PrjForms-</i> in the om$prime.settings file are
copied to the project's settings file. Thus, all the inputs you add to forms here appear in every project and should be basic
inputs that collect information required by every project.</p>
<p>Principal Investigators can customize some of these forms (the ones that begin with <i>PrjForm-</i>) by adding additional
fields. The idea is to allow each project to collect customized information. But that's NOT what's going on on this page;
here you create and edit the parts of forms that are the same for every project.</p>
<p>PS: There are some minor problems with using a double-quotation mark in a field. It works fine until you try to edit that
input, at which point the value of the input will end at the first double-quote. Single quotes work, as does re-entering the
text every time you edit an input. Sorry; this one is too complicated to figure out.</p>
")
   return(tagList(
      bs4("r", class="mt-3", bs4("c12", bs4("cd", q="y", bs4("cdb", bs4("cdt", yBox)))))
   ))

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
      "newForm" = {
         if(S$P$Modify) {
            S$IN$flag$showAddInputButton <<- TRUE
            S$display <<- "newForm"
            S$hideMenus <<- TRUE
            rv$limn <- rv$limn +1     # Need to limn at this level to hideMenus
         }
      },
      "editForm" = {               # This doesn't use imGetForm because at this point we have the form's ID rather than its name.
         if(S$P$Modify) {
            r <-recGet("om$prime", "settings", c("name","value"), tibble(c("settingsID", "=", n)))
            S$IN$FORMname <<- r$name
            fileName <- paste0("FORMs/", S$IN$FORMname, ".csv")
            if(AppGlobal$FORMfromDisk && file.exists(fileName)) {
               print("Loading file...")
               S$IN$FORM <<- read_csv(fileName, na=character(), col_types="ccccccccccccccccccclllln")
            } else {
               print("Loading from SQL...")
               S$IN$FORM <<- as.tibble(fromJSON(r$value))           # Yes, unJSONize as it's a tibble
            }
   #         View(S$IN$FORM)
            S$IN$flag$showAddInputButton <<- TRUE
            S$IN$flag$oldInput <<- FALSE                      # Need this for the Look and Feel item-disabling if()
            S$display <<- "editForm"
            S$hideMenus <<- TRUE
            rv$limn <- rv$limn +1     # Need to limn at this level to hideMenus
         }
      },
      "deleteForm" = {
         if(S$P$Modify) {
            r <-recGet(S$db, "settings", c("name","value"), tibble(c("settingsID", "=", n)))
            S$IN$FORM <<- as.tibble(fromJSON(r$value))
            dbLink <- poolCheckout(shiny.pool)                              # When deleting a FORM, we also need to delete
            on.exit(poolReturn(dbLink), add = TRUE)                         #   its ids from the ids table
            for(i in 1:nrow(S$IN$FORM)) {
               r = dbExecute(dbLink, paste0("DELETE FROM `", S$db, "`.`ids` WHERE idsID='", S$IN$FORM[i, "id"], "';"))
            }
            r = dbExecute(dbLink, paste0("DELETE FROM `", S$db, "`.`settings` WHERE settingsID='", n, "';"))
            rv$limn = rv$limn + 1
         }
      },
      "saveAdd" = {
         if(S$P$Modify) {
            f = str_trim(stripHTML(input[[S$newFormNameID]]))
            if(str_sub(f, 1, 5)=="Form-" || str_sub(f, 1, 8)=="PrjForm-") {
               r = recGet(om$prime, "settings", "**", tibble(c("name", "=", f)))   # all forms are in om$prime but not in S$db
                  if(r[[1,1]]==0) {
                     r$name[2] <- f
                     r$value[2] <- toJSON(imGetBlankFORMrow("blank")[-1,])
                     r = recSave(r)
                     S$display <<- "open"
                     S$hideMenus <<- FALSE
                     rv$limn <- rv$limn +1   # Need to limn at this level to un-hideMenus, also to display added Form
                  } else {
                     S$modal_title <<- "Whoops"
                     S$modal_text <<- HTML0("<p>That name is taken.</p>")
                     S$modal_size <<- "s"
                     rv$modal_warning <- rv$modal_warning + 1
                  }
            } else {
               S$modal_title <<- "Whoops"
               S$modal_text <<- HTML0("<p>Form names MUST begin with either <i>PrjForm-</i> (for a project's ",
                                     "customizable forms) or <i>Form-</i> for system-wide forms.</p>")
               S$modal_size <<- "m"
               rv$modal_warning <- rv$modal_warning + 1
            }
         }
      },
      "cancelAdd" = {
         S$display <<- "open"
         S$hideMenus <<- FALSE
         rv$limn <- rv$limn +1      # Need to limn at this level to un-hideMenus
      },
      "inputAdd" = {                # This is the big green Add button
         if(S$P$Modify) {
            S$IN$flag$showAddInputButton <<- FALSE
            S$hideMenus <<- TRUE
            S$IN$flag$oldInput <<- FALSE
            S$IN$inputNUM <<- 0
            S$IN$FORMrow <<- imGetBlankFORMrow(S$IN$codeTypes[S$IN$inputType])
            S$IN$FORMrowform <<- imGetBlankform(S$IN$codeTypes[S$IN$inputType])
            S$IN$FORMrowform$order <<- 1:nrow(S$IN$FORMrowform)
            rv$limn = rv$limn + 1
         }
      },
      "inputEdit" = {               # This is the green Edit button
         if(S$P$Modify) {
            S$IN$flag$showAddInputButton <<- FALSE
            S$hideMenus <<- TRUE
            S$IN$flag$oldInput <<- TRUE                                          # disable selector, among other things
            S$IN$inputNUM <<- as.numeric(n)
            S$IN$FORMrow <<- S$IN$FORM[n,]
            S$IN$FORMrowform <<- imFORMrow2form(S$IN$FORMrow)                    # expand form row into a form
            S$IN$inputType <<- which(S$IN$codeTypes %in% S$IN$FORM[[n,"type"]])  # get type of input for selector
            rv$limn = rv$limn + 1
         }
      },
      "inputSave" = {               # This button is on the output$modifyAnInput screen
         if(S$P$Modify && imInputValidates()) {
#            S$IN$TABLE <<- "pico"
            imSaveform2FORMrow()
#            View(S$IN$FORM)
            S$IN$flag$showAddInputButton <<- TRUE
            # S$IN$settingsName
            # saveDB <- S$db
            imSaveFORM()
            rv$limn = rv$limn + 1
         }
      },
      "inputCancel" = {             # This button is on the output$modifyAnInput screen
         S$IN$flag$showAddInputButton <<- TRUE
         rv$limn = rv$limn + 1
      },
      "inputDelete" = {
         if(S$P$Modify) {
            dbLink <- poolCheckout(shiny.pool)                              # When deleting an input, we also need to delete
            on.exit(poolReturn(dbLink), add = TRUE)                         #   its id from the ids table
            r = dbExecute(dbLink, paste0("DELETE FROM `", S$db, "`.`ids` WHERE idsID='", S$IN$FORM[n, "id"], "';"))
            S$IN$FORM <<- S$IN$FORM[-as.numeric(n),]                        # Delete the n row from FORM
            imSaveFORM()                                                    # Save the FORM
            rv$limn = rv$limn + 1
         }
      },
      "inputUp" = {
         if(S$P$Modify) {
            S$IN$FORM[n,"order"] <<- S$IN$FORM[n,"order"] - 1.5
            imFixOrder()                                                    # imFixOrder also saves and updates S$IN$FORM
            rv$limn = rv$limn + 1
         }
      },
      "inputDown" = {
         if(S$P$Modify) {
            S$IN$FORM[n,"order"] <<- S$IN$FORM[n,"order"] + 1.5
            imFixOrder()                                                    # imFixOrder also saves and updates S$IN$FORM
            rv$limn = rv$limn + 1
         }
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)
