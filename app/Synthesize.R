### open-meta.app Synthesize.R
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

rv$menuActive = 1    # Start out on first sub-menu
rv$limnSetup = 0

S$editFORM <- FALSE
S$NUMs$analysisNUM <- 0

S$R <- recGet(S$db, "result",
           c("resultID", "studyNUM", "armNUM", "P", "I", "C", "O", "TS", "info", "esType", "nC", "nI", "es", "v", "ci.lo", "ci.hi"),
           tibble(d = c("deleted", "=", "0")))
S$noResults <- ifelse(S$R$resultID[1]==0, TRUE, FALSE)

if(S$P$Msg=="") {
   output$uiMeat <- renderUI({c(rv$limn); isolate({
      if(rv$limn && S$P$Msg=="") {
         if(S$editFORM && S$P$Modify) {
            return(tagList(
               bs4("r", align="hc",
                  bs4("c10",
                  bs4("r", id="editForm")
               ))
            ))
         } else {
            switch(as.character(rv$menuActive),
               "1" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="Setup")
                     ))
                  ))
               },
               "2" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="Prisma")
                     ))
                  ))
               },
               "3" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="Sequential")
                     ))
                  ))
               },
               "4" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="Forest")
                     ))
                  ))
               },
               "5" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="Bias")
                     ))
                  ))
               },
               "6" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="Subgroup")
                     ))
                  ))
               },
               "7" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="MetaRegression")
                     ))
                  ))
               }
            )
         }
      }
   })})
}

output$pageMenu <- renderUI({c(rv$menuActive, rv$limn); isolate({
   if(S$hideMenus) { return("") }
   return(
      tagList(
         bs4("c12",
         bs4("md", id="sub", n=1:7, active=rv$menuActive, text=c("Analysis Setup", "PRISMA diagram", "Sequential Analysis", "Forest Plot", "Bias Plots", "Subgroup Analysis", "Meta-Regression")),
            bs4("dx", style="height:1.5rem")
         )
      )
   )
})})

output$editForm <- renderUI({c(rv$limn); isolate({
   db <- ifelse(str_sub(S$editFORMname,1,5)=="Form-", "om$prime", S$db)
   S$IN$FORM <<- imGetFORM(S$editFORMname, db)                    # S$editFORMname is the only input here
   S$IN$FORM <<- imGetFORMvalues(S$IN$FORM)                       # Heavily uses inputMeta.R functions
   return(tagList(                                                # Save will use rv$imGetFORMData, which
      imForm2HTML(S$IN$FORM),                                     #   needs S$IN$FORM, which is set up here
      bs4("c12", class="text-right",
         bs4("btn", uid=paste0("cancelForm_", S$editFORMname), q="b", class="mr-3", "Cancel"),
         bs4("btn", uid=paste0("saveForm_", S$editFORMname), q="b", class="mr-3", "Save")
      )
   ))
})})

output$Setup <- renderUI({c(rv$limn, rv$limnSetup); isolate({
#   print(paste0("Running output$Setup; analysisNUM: ", S$NUMs$analysisNUM))
   if(S$noResults) { return(HTML0(("<h5>Nothing to display - extraction must be complete on at least one study.</h5>"))) }
   A <- recGet(S$db, "analysis",
           c("analysisID", "name", "type", "P", "I", "C", "O", "TS", "comment"),
           tibble(d = c("deleted", "=", "0")))
   if(S$P$Modify) {
      newAnyBtn <- tagList(                                          # New Any/Edit Any buttons
         bs4("c12", bs4("btn", uid="addForm_Form-Analysis", q="g", class="mr-3", "Add a New Analysis")))
      editAnyBtn <- tagList(
         bs4("c12", class="text-right", bs4("btn", uid="editForm_Form-Analysis", q="g", class="mr-3", "Edit Analysis")))
   } else {
      newAnyBtn <- editAnyBtn <- ""
   }
   if(S$NUMs$analysisNUM==0) {    # Show picker
      ID = "viewAny"                                                 # This allows multiple pickRs on a single page
      TABLE = "analysis"                                             # The table the pickR data will come from
      WHERE=tibble(d=c("deleted", "=", "0"))
      FilterF = whereFilter                                          # typically whereFilter
      HeadlineF = THRUb                                              # typically THRUb
      SELECT = "name"                           # These are the table fields needed to build the pickR
      if(S$P$Modify) {                                               # View or Edit depends on permissions
        ButtonData <- list(edit=list(id=paste0("viewAny"), q="b", class="mr-2", label="View Analysis"),
                           delete=list(id=paste0("deleteX"), q="r", class="mr-2", label="Delete Analysis"))
      } else {
        ButtonData <- list(view=list(id=paste0("viewAny"), q="b", label="View"))
      }
      ButtonF = stdButtons                                           # use just the function name; no quotes, no ()
      FixDataF = THRU
      FormatF = prf_analysis
      NOtext = "No Analyses have been created yet."
      activePage = ifelse(is.null(S$PKR[[ID]]$activePage), 1, S$PKR[[ID]]$activePage)
      itemsPerPage = S$PKR$itemsPerPage                              # Modifiable pickR-by-pickR
      scroll = FALSE                                                 # Modifiable pickR-by-pickR
      results <- pickR(ID, S$db, TABLE, WHERE, FilterF, HeadlineF, SELECT, ButtonData, ButtonF,
                    FixDataF, FormatF, NOtext, activePage, itemsPerPage, scroll)
      return(tagList(
         newAnyBtn,
         results
      ))
   }
   if(S$NUMs$analysisNUM>0) {    # Show selected analysis
      FORM <- imGetFORM("Form-Analysis", "om$prime")
      FORM <- imGetFORMvalues(FORM)
      FORM$disabled <- TRUE                                          # Disable the FORM until editing
      S$Any$FORM <<- FORM
      otherAnyBtn <- tagList(
         bs4("c12", class="pl-0 pb-2", bs4("btn", uid="viewAny_0", q="b", class="mr-3", "Select a Different Analysis")))
      return(tagList(
         otherAnyBtn,
         imForm2HTML(FORM),
         editAnyBtn
      ))
   }
})})

# Standard pickR formatting functions
prf_analysis = function(r) {                        # Standard function for one column of data and one row of buttons
   return(paste0(
'<div class="row">
   <div class="col-4">', r[[1,]], '</div>
   <div class="col-5 text-right">', r[[2,]], '</div>',
#   <div class="col-5></div>',
   bs4('c12', bs4('hr0', class="py-2")), '
</div>', collapse = ''))
}

output$Prisma <- renderUI({c(rv$menuActive, rv$limn); isolate({
   return(HTML0("<p>PRISMA diagram</p>")
   )
})})

output$Sequential <- renderUI({c(rv$menuActive, rv$limn); isolate({
   if(S$noResults) { return(HTML0(("<h5>Nothing to display - extraction must be complete on at least one study.</h5>"))) }
   return(HTML0("<p>Sequential Analysis</p>")
   )
})})

output$Forest <- renderUI({c(rv$menuActive, rv$limn); isolate({
   if(S$noResults) { return(HTML0(("<h5>Nothing to display - extraction must be complete on at least one study.</h5>"))) }
   return(HTML0("<p>Forest Plot</p>")
   )
})})

output$Bias <- renderUI({c(rv$menuActive, rv$limn); isolate({
   if(S$noResults) { return(HTML0(("<h5>Nothing to display - extraction must be complete on at least one study.</h5>"))) }
   return(HTML0("<p>Bias Plots</p>")
   )
})})

output$Subgroup <- renderUI({c(rv$menuActive, rv$limn); isolate({
   if(S$noResults) { return(HTML0(("<h5>Nothing to display - extraction must be complete on at least one study.</h5>"))) }
   return(HTML0("<p>Subgroup Analysis</p>")
   )
})})

output$MetaRegression <- renderUI({c(rv$menuActive, rv$limn); isolate({
   if(S$noResults) { return(HTML0(("<h5>Nothing to display - extraction must be complete on at least one study.</h5>"))) }
   return(HTML0("<p>Meta-regression</p>")
   )
})})

### observer for omclick
observeEvent(input$js.omclick, {
   if(A$debugON) {
      cat(paste0("Click on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "sub" = {
         rv$menuActive = n
         rv$limn = rv$limn + 1
      },
      "viewAny" = {
         S$NUMs$analysisNUM <<- n
         S$hideMenus <<- FALSE
         rv$limnSetup <- rv$limnSetup + 1
      },
      "deleteX" = {
         if(S$P$Modify) {
            r <- recGet(S$db, "analysis", c("**"), tibble(c("analysisID", "=", n)))
            r$deleted[2] <- 1
            r <- recSave(r, S$db)
            rv$limnSetup = rv$limnSetup + 1
         }
      },
      "addForm" = {
         if(S$P$Modify) {
            S$editFORM <<- TRUE
            S$editFORMname <<- n
            r <- recGet(S$db, "analysis", c("analysisID"), tibble(c("deleted", ">=", 0)))
            S$NUMs$analysisNUM <<- max(r$analysisID) + 1
            S$hideMenus <<- TRUE
            rv$limn <- rv$limn + 1
         }
      },
      "editForm" = {
         if(S$P$Modify) {
            S$editFORM <<- TRUE
            S$editFORMname <<- n
            S$hideMenus <<- TRUE
            rv$limn <- rv$limn + 1
         }
      },
      "cancelForm" = {
         S$editFORM <<- FALSE
         S$hideMenus <<- FALSE
         rv$limn <- rv$limn + 1
      },
      "saveForm" = {
         if(S$P$Modify) {
            msg=""
               n <- str_trim(stripHTML(as.character(input[["AnalysisName"]]))) == ""
               P <- is.null(input[["AnalysisP"]])
               I <- is.null(input[["AnalysisI"]])
               C <- is.null(input[["AnalysisC"]])
               O <- is.null(input[["AnalysisO"]])
               TS <- is.null(input[["AnalysisTS"]])
               if(n) {
                  msg = paste0(msg, "<li>Analysis name can't be blank</li>")
               }
               if(P) {
                  msg = paste0(msg, "<li>You must check at least one Participant Group</i></li>")
               }
               if(I) {
                  msg = paste0(msg, "<li>You must check at least one Intervention</i></li>")
               }
               if(C) {
                  msg = paste0(msg, "<li>You must check at least one Comparison</i></li>")
               }
               if(O) {
                  msg = paste0(msg, "<li>You must check at least one Outcome</i></li>")
               }
               if(TS) {
                  msg = paste0(msg, "<li>You must check at least one Time Span</i></li>")
               }
            if(msg!="") {
               S$modal_title <<- "Whoops"
               S$modal_text <<- HTML("<p>Can't save this analysis because:<ul>", msg, "</ul></p>")
               rv$modal_warning <- rv$modal_warning + 1
            } else {
               S$editFORM <<- FALSE
               S$hideMenus <<- FALSE
               rv$imGetFORMData <- rv$imGetFORMData + 1
               rv$limnSetup <- rv$limnSetup+1
            }
         }
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)


