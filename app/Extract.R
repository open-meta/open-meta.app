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
source("citationFiltering.R", local=TRUE)

rv$menu1Active <- 1
rv$menu2Active <- 1

# pickR globals
# S$PKR$itemsPerPage             Now in app.R

# S$PKR$P$activePage <- 1        These aren't being used but does pagination work for picos?
# S$PKR$I$activePage <- 1
# S$PKR$C$activePage <- 1
# S$PKR$O$activePage <- 1
# S$PKR$TS$activePage <- 1

S$PRK$Studies$activePage <- 1
rv$limnExtraction <- 0
rv$limnviewStudy <- 0
rv$limnviewArm <- 0
rv$limnviewCalculator <- 0
rv$limnviewResults <- 0
# rv$limnviewGroup <- 0

S$picoName = ""
S$picoDisplay = "add"

S$NUMs = list()
S$NUMs$extractID <- 0
S$NUMs$catalogID <- 0
S$NUMs$studyNUM <- 0
S$NUMs$armNUM <- 0
S$NUMs$armNUMnext <- 0

S$Arm <- list()
S$Arm$FORM <- tibble()
S$Arm$iVec <- 0
S$Arm$tsVec <- 0
S$Arm$FORMname <- ""
S$Arm$lastCalc <- now()
S$Arm$lastWasNULL <- FALSE

S$Names = list()
S$Names$Trial <- ""


S$extractTBL <- tbl(shiny.pool, in_schema(S$db, "extract"))
S$catalogTBL <- tbl(shiny.pool, in_schema(S$db, "catalog"))
S$picoTBL <- tbl(shiny.pool, in_schema(S$db, "pico"))

S$editFORM <- FALSE


output$uiMeat <- renderUI({c(rv$limn); isolate({
   if(rv$limn && S$P$Msg=="") {
      S$E$reviews <<- updateEXtable()                    # Add/remove extraction records to match catalog$decision
      if(S$editFORM && S$P$Modify) {
         return(tagList(
            bs4("r", align="hc",
               bs4("c10",
               bs4("r", id="editForm")
            ))
         ))
      } else {
         switch(as.character(rv$menu1Active),
            "1" = {
               return(tagList(
                  bs4("r", align="hc",
                     bs4("c10",
                     bs4("r", id="pageMenu"),
                     bs4("r", id="Dashboard")
                  ))
               ))
            },
            "2" = {
               return(tagList(
                  bs4("r", align="hc",
                     bs4("c10",
                     bs4("r", id="pageMenu"),
                     bs4("r", id="PICOSetup")
                  ))
               ))
            },
            "3" = {
               return(tagList(
                  bs4("r", align="hc",
                     bs4("c10",
                     bs4("r", id="pageMenu"),
                     bs4("r", id="Extraction")
                  ))
               ))
            }
         )
      }
   }
})})

output$pageMenu <- renderUI({c(rv$limn); isolate({
   menu1 <- c("Dashboard", "PICO(T) Setup", "Extraction List")
   menu2 <- c("Participants", "Interventions", "Comparisons", "Outcomes", "Time Spans")
   subMenu <- ifelse(rv$menu1Active!=2, "",
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

output$editForm <- renderUI({c(rv$limn); isolate({
   S$IN$FORM <<- imGetFORM(S$Arm$FORMname)                        # S$Arm$FORMname is the only input here
   S$IN$FORM <<- imGetFORMvalues(S$IN$FORM)                       # Heavily uses inputMeta.R functions
   return(tagList(                                                # Save will use rv$imGetFORMData, which
      imForm2HTML(S$IN$FORM),                                     #   needs S$IN$FORM, which is set up here
      bs4("c12", class="text-right",
         bs4("btn", uid=paste0("cancelForm_", S$Arm$FORMname), q="b", class="mr-3", "Cancel"),
         bs4("btn", uid=paste0("saveForm_", S$Arm$FORMname), q="b", class="mr-3", "Save")
      )
   ))
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
         bs4("c12", id="TSpickR"),
         bs4("c12", id="TSYbox")
      )
   )
})})

output$TSpickR <- renderUI({c(rv$limn); isolate({
   # This is probably going to require another table with catalogID and Trial Name fields. The extract table doesn't work
   #   because it only has catalogIDs that have passed Stage 1 review, while the new table should support any catalogID.
   # Maybe there should be another submenu here that does "author analysis". I'm thinking of somehow using dplyr to find,
   #   for both all reports and for S1Pass+ reports, which authors are on more than report and which reports have the same
   #   3+, 2, or 1 author. Maybe the results of this also appear in the dashboard?
   # Perhaps the pickR here should display title and abstract?
HTML0(
"<p>Trial Setup will be here.</p>")
})})

output$TSYbox <- renderUI({c(rv$limn); isolate({
   yBox <- HTML0("
<p>So far in your project you have searched for and reviewed published <i>reports</i> of research studies. Typically there is
a one-to-one correspondence between a study and report, but not always. Very large studies, which Open-Meta refers to as
<i>trials</i>, may be described in several separate reports.</p>
<p>To meet the assumptions of meta-analysis, these various reports have to be gathered together into one trial. If one of
the individual published reports you've reviewed indicates that it is part of a larger trial, enter the name of that trial
here. Open-Meta will search through the titles and abstracts of all your publications looking for other reports that
include that name and allow you to gather these reports together here under the trial's name.
   ")
   return(tagList(
      bs4("r", class="mt-3", bs4("c12", bs4("cd", q="y", bs4("cdb", bs4("cdt", yBox)))))
   ))

})})


output$PICOSetup <- renderUI({c(rv$limn); isolate({
   if(S$picoDisplay=="add") {
      return(
         tagList(
            bs4("c12", id="picoAdd"),
            bs4("c12", id="picoPickR"),
            bs4("c12", id="picoYbox")
         )
      )
   }
   if(S$picoDisplay=="edit") {
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
output$picoPickR <- renderUI({c(rv$limn, rv$limnPico); isolate({
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
   FilterF <- whereFilter
   HeadlineF = THRUb                                                       # THRUb returns "", as we have no headline
   if(S$P$Modify) {                                                        # View or Edit depends on permissions
     ButtonData <- list(edit=list(id="editPico", q="g", class="mr-2", label="Edit"),
                      delete=list(id="deletePico", q="r", label="Delete"))
   } else {
     ButtonData <- list(view=list(id="viewPico", q="b", label="View"))
   }
   ButtonF = stdButtons
   FixDataF = picoFix
   FormatF = prf_1X1
   NOtext = "Nothing here yet."
   itemsPerPage = S$PKR$itemsPerPage                                       # Modifiable pickR-by-pickR
   scroll = FALSE                                                          # Modifiable pickR-by-pickR
   return(pickR(ID, S$db, TABLE, WHERE, FilterF, HeadlineF, SELECT, ButtonData, ButtonF,
                    FixDataF, FormatF, NOtext, activePage, itemsPerPage, scroll))
})})

picoFix <- function(x) {
   x <- x[,-2]            # delete "name" column, leave "values"
   x[1,"Action"] = str_replace(x[1,"Action"], "btn-danger", "btn-danger disabled")  # disable deletion of button 1!
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
<p>Your Principal Investigator may have customized the participant group form in the Members & Settings menu to collect
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
   S$Pico$Form <<- imGetFORM(S$picoName[2])        # S$picoName is set in picoPickR
   if(S$recID>0) {                               # If this is an edit, get the FORM's current values; S$recID set in addPico, editPico
      R <- recGet(S$db, "pico", c("name", "value"), tibble(c("picoNUM", "=", imID2NUM(S$recID, "pico"))))
      for(i in 1:nrow(S$Pico$Form)) {              # Insert values from R into form$value
         S$Pico$Form$value[i] <<- R$value[R$name==S$Pico$Form$name[i]]    # In FORM, "name" is the short label
      }                                                               # In R, it's the "name" of the "value"
   }
   if(S$P$Modify) {
      SaveBtn = HTML0(bs4("btn", id="savePico", n=1, q="b", "Save"))
   } else {                                      # If user can't modify inputs, force View (disabled inputs)
      S$Pico$Form$disable <<- TRUE                 #    and skip the Save button
      SaveBtn = ""
   }
   restOfPage = tagList(
      imForm2HTML(S$Pico$Form),
      bs4("r", align="he",
          bs4("c3", bs4("btn", id="cancelPico", n=1, q="b", "Cancel"), SaveBtn)
      )
   )
})})

output$Extraction <- renderUI({c(rv$limn, rv$limnExtraction); isolate({
   return(
      tagList(
         bs4("c12", id="pickStudy"),
         bs4("c12", id="viewStudy"),
#         bs4("c12", id="studyYbox"),
         bs4("c12", id="viewArm"),
#         bs4("c12", id="ArmYbox"),
         bs4("c12", id="viewCalculator"),
         bs4("c12", id="CalculatorYbox"),
         bs4("c12", id="viewResults"),
         bs4("c12", id="ResultsYbox")
#         bs4("c12", id="viewGroup")
#         bs4("c12", id="GroupYbox"),
      )
   )
})})



   # Moving on...
   # I think we're then ready to make a pickR from the extract table and the review decisions
   # pickR buttons should be Extract or View
   #
   # EXTRACT STUDY
   # First you get a chance to change the STUDY name; this form also gives you the STUDY bias radio buttons
   # Under that is a pickR that allows you to create a new Arm and see exising Arms. Buttons: Edit or View
   # After edit/view, the pickR is replaced by a FORM for the Arm.
   # Each Arm has a specific participant group, comparison group, and time span (One way to think of this is that
   #    within an Arm, the control group doesn't change.)
   # Part of that is a pickR (+ add?) button for the Outcome.
   # When you edit an outcome, first you get a dropdown for the type of data you have, which will present the correct
   #    calculator to convert the data into an effect size in the next section.
   # After that is a pickR + add button for the Group.
   # The first group is the control group; there must also be at least one intervention group, but there can be multiple.
   # You enter the data for the groups, save them, do this for all outcomes, then for all arms, then for all Trials,
   #    and you're done.

#    return(
#       tagList(
#          bs4("c12", HTML("<pre>Length of review is ", nrow(review)," and it is ", review$catalogID, "</pre>"))
#       )
#    )
# })})

output$pickStudy <- renderUI({c(rv$limn, rv$limnExtraction); isolate({ # !!!Note that rv$limnForms must be rv[[paste0("limn",ID)]]!!!
#   print("Running output$pickStudy")
   if(S$NUMs$catalogID>0) {
      return("")
   } else {
   ###### pickR start
      ID = "Studies"                                                 # This allows multiple pickRs on a single page
      TABLE = "catalog"                                              # The table the pickR data will come from
      WHERE = "extract"                       # Special handling for citesFilter
      FilterF = citesFilter                                                   # typically whereFilter
      HeadlineF = citesHead                                                   # typically THRUb
      SELECT = NULL                           # These are the table fields needed to build the pickR
      if(S$P$Modify) {                                               # View or Edit depends on permissions
        ButtonData <- list(extract=list(id=paste0("viewStudy"), q="g", class="mr-2", label="Extract"))
      } else {
        ButtonData <- list(view=list(id=paste0("viewStudy"), q="b", label="View"))
      }
      ButtonF = stdButtons                                           # use just the function name; no quotes, no ()
      FixDataF = studyFix
      FormatF = prf_5.3.4r
      NOtext = "No studies found by this filter."
      activePage = ifelse(is.null(S$PKR[[ID]]$activePage), 1, S$PKR[[ID]]$activePage)
      itemsPerPage = S$PKR$itemsPerPage                              # Modifiable pickR-by-pickR
      scroll = FALSE                                                 # Modifiable pickR-by-pickR
      results <- pickR(ID, S$db, TABLE, WHERE, FilterF, HeadlineF, SELECT, ButtonData, ButtonF,
                    FixDataF, FormatF, NOtext, activePage, itemsPerPage, scroll)
      restOfPage <- tagList(
         bs4("r",
            bs4("c12",
               HTML("<span style='font-size: 1.25rem; color:#fff;'>Filter citations</span><br>"),
               imForm2HTML(S$FIL$FORM)
            ),
            # bs4("c1"),
            # bs4("c12", class="text-right",
            #    bs4("btn", uid="filter_0", q="b", class="mr-3", "Filter")
            # ),
            results
         )
      )
   }
})})

studyFix <- function(R) {
   # R has 30 catalogID and buttons
   # extractTBL <- tbl(shiny.pool, in_schema(S$db, "extract"))
   # catalogTBL <- tbl(shiny.pool, in_schema(S$db, "catalog"))
   catalog <- S$catalogTBL %>%
      filter(catalogID %in% R$catalogID) %>%
      select(catalogID, reviewBest, Y, author) %>%
      collect()
   statusText=c("Not reviewed", "Stage 1 Fail", "Stage 1 Pass", "Extraction Fail", "Extraction Pass")
   catalog$reviewBest <- statusText[catalog$reviewBest + 1]
   Rx <- S$extractTBL %>%
      filter(catalogID %in% R$catalogID & name=="Trial") %>%         # filter extract table; these should be unique
      select(catalogID, value) %>%                               # just need Trial
      merge(catalog, all.x=TRUE) %>%                                 # add reviewBest, Y, author
      merge(R, all.x=TRUE) %>%                                       # add buttons
      arrange(Y, author) %>%                                         # sort
      select(-Y, -author) %>%                                        # remove Y, author
      collect() %>%
      as.tibble()
   return(Rx)
}

prf_5.3.4r = function(r) {                        # Standard function for one column of data and one row of buttons
   return(paste0(
'<div class="row">
   <div class="col-5">', r[[1,]], '</div>
   <div class="col-3">', r[[2,]], '</div>
   <div class="col-4 text-right">', r[[3,]], '</div>',
   bs4('c12', bs4('hr0', class="py-2")), '
</div>', collapse = ''))
}

output$viewStudy <- renderUI({c(rv$limn, rv$limnExtraction, rv$limnviewStudy); isolate({
#   print("Running output$viewStudy")
   if(S$NUMs$catalogID==0) {
      return("")
   } else {
      r <- S$extractTBL %>%
            filter(catalogID == S$NUMs$catalogID & name=="Trial") %>%  # All we have now is the catalogID
            select(studyNUM, value) %>%                                # Get studyNUM too
            collect()
      S$NUMs$studyNUM <<- r$studyNUM                                   # Memorize studyNUM and study name
      S$Names$Trial <<- r$value
      FORM <- imGetFORM("PrjForm-Trial", S$db)
      FORM <- imGetFORMvalues(FORM)
      FORM$disabled <- TRUE                                            # Disable the FORM for now
      if(S$P$Modify) {                                                 #   but provide a button for editing it
         editStudyBtn <- tagList(
            bs4("c12", class="text-right",
                bs4("btn", uid="editForm_PrjForm-Trial", q="g", class="mr-3", "Edit Study-Level Data")))
      } else {
         editStudyBtn <- ""
      }
      return(tagList(
         bs4("c12", class="pl-0 pb-2", bs4("btn", uid="menu1_4", q="b", class="mr-3", "Select a Different Study")),
         imForm2HTML(FORM),
         editStudyBtn
      ))
   }
})})

output$viewArm <- renderUI({c(rv$limn, rv$limnExtraction, rv$limnviewArm); isolate({
#   print(paste0("Running output$viewArm; armNUM: ", S$NUMs$armNUM))
   if(S$NUMs$studyNUM==0) {
      return("")
   }
   r = recGet(S$db, "extract", "armNUM", tibble(                     # Look up all armNUMs for this article
                        d = c("deleted", ">=", "0"),
                        c = c("catalogID", "=", S$NUMs$catalogID)))
   S$NUMs$armNUMnext <<- max(r$armNUM) + 1                           # Save the highest + 1
   if(S$P$Modify) {
      newArmBtn <- tagList(                                          # New Arm/Edit Arm buttons
         bs4("c12", bs4("btn", uid="addForm_PrjForm-Arm", q="g", class="mr-3", "Add a New Arm")))
      editArmBtn <- tagList(
         bs4("c12", class="text-right", bs4("btn", uid="editForm_PrjForm-Arm", q="g", class="mr-3", "Edit Arm-Level Data")))
   } else {
      newArmBtn <- editArmBtn <- ""
   }
   if(S$NUMs$armNUM>0) {
      FORM <- imGetFORM("PrjForm-Arm", S$db)
      FORM <- imGetFORMvalues(FORM)
      FORM$disabled <- TRUE                                          # Disable the FORM for now
      S$Arm$FORM <<- FORM
      otherArmBtn <- tagList(
         bs4("c12", class="pl-0 pb-2", bs4("btn", uid="otherArm_0", q="b", class="mr-3", "Select a Different Arm")))
      rv$limnviewCalculator <- rv$limnviewCalculator + 1             # Display the calculator
      return(tagList(
         otherArmBtn,
         imForm2HTML(FORM),
         editArmBtn
      ))
   }
   if(S$NUMs$armNUM==0) {
      ID = "viewArm"                                                 # This allows multiple pickRs on a single page
      TABLE = "extract"                                              # The table the pickR data will come from
      WHERE=tibble(c=c("catalogID", "=", S$NUMs$catalogID),
                   s=c("studyNUM", "=", S$NUMs$studyNUM),
            #       a=c("armNUM", "=", "0"),
                   n=c("name", "=", "armName"))
      FilterF = whereFilter                                                   # typically whereFilter
      HeadlineF = THRUb                                                   # typically THRUb
      SELECT = "value"                           # These are the table fields needed to build the pickR
      if(S$P$Modify) {                                               # View or Edit depends on permissions
        ButtonData <- list(edit=list(id=paste0("viewArm"), q="b", class="mr-2", label="View Arm"),
                           delete=list(id=paste0("deleteX"), q="r", class="mr-2", label="Delete"))
      } else {
        ButtonData <- list(view=list(id=paste0("viewArm"), q="b", label="View"))
      }
      ButtonF = stdButtons                                           # use just the function name; no quotes, no ()
      FixDataF = THRU
      FormatF = prf_arm
      NOtext = "No arms have been created yet."
      activePage = ifelse(is.null(S$PKR[[ID]]$activePage), 1, S$PKR[[ID]]$activePage)
      itemsPerPage = S$PKR$itemsPerPage                              # Modifiable pickR-by-pickR
      scroll = FALSE                                                 # Modifiable pickR-by-pickR
      results <- pickR(ID, S$db, TABLE, WHERE, FilterF, HeadlineF, SELECT, ButtonData, ButtonF,
                    FixDataF, FormatF, NOtext, activePage, itemsPerPage, scroll)
      return(tagList(
         newArmBtn,
         results
      ))
   }
})})

# Standard pickR formatting functions
prf_arm = function(r) {                        # Standard function for one column of data and one row of buttons
   return(paste0(
'<div class="row">
   <div class="col-4">', r[[1,]], '</div>
   <div class="col-4 text-right">', r[[2,]], '</div>',
#   <div class="col-5></div>',
   bs4('c12', bs4('hr0', class="py-2")), '
</div>', collapse = ''))
}

output$viewCalculator <- renderUI({c(rv$limnviewCalculator, input$OutcomePICO, input$EStype, input$ES); isolate({
   if(S$NUMs$armNUM==0) {                    # If no armNUM, don't display the Calculator
      return("")
   }
   # We'll need the list of all project outcomes a couple of times here
   allOutcomes <- recGet(S$db, "pico", c("value"), tibble(c("name", "=", "Outcome")))$value
   # First, nail down the OutcomePICO we want to use
   if(is.null(input$OutcomePICO)) {                            # If this is the first time through, look
      calcOutcomes <- recGet(S$db, "extract", "outcome",       #   for any saved Calculators for this arm
            WHERE=tibble(c("studyNUM", "=", S$NUMs$studyNUM),
                         c("armNUM", "=", S$NUMs$armNUM),
                         c("name", "=", "Calculator")))$outcome
      if(calcOutcomes[1]!="") {
         OutcomePICO <- calcOutcomes[1]                        # Use first Calculator outcome, if available
      } else {
         OutcomePICO <- allOutcomes[1]                         # Otherwise, use first allOutcomes
      }
   } else {
      OutcomePICO <- input$OutcomePICO                         # But use the input value if there is one
   }
   # Second, see if we have a Saved Calculator for this Arm and OutcomePICO
   r <- recGet(S$db, "extract", "value",     # If we've saved a Calculator for this arm and outcome, get it.
               WHERE=tibble(c("studyNUM", "=", S$NUMs$studyNUM),
                            c("armNUM", "=", S$NUMs$armNUM),
                            c("outcome", "=", OutcomePICO),
                            c("name", "=", "Calculator")))
   if(is.null(input$ES)) {                   # If this is the first time through and there was a Calculator
      if(r$value!="") {                      #    on the server, use it.
         FORM <- fromJSON(r$value)
      } else {                               # No saved Calculator for this Outcome, use default
         FORM <- imGetFORM("Form-ES-d", "om$prime")
      }
   } else {                                  # Not first time; check inputs
      if(r$value!="") {                      # There is a saved form for this Arm/Outcome
         FORM <- fromJSON(r$value)
         fEStype <- FORM %>% filter(id=="EStype") %>% pull(value)
         fES     <- FORM %>% filter(id=="ES") %>% pull(value)
         if(fEStype!=input$EStype || fES!= input$ES) {          # Does saved form have right EStype/ES?
            FORM <- whichCalculator(input$EStype, input$ES)     #    No; find the right form
         }
      } else {
         FORM <- whichCalculator(input$EStype, input$ES)        # Nothing saved ; find the right form
      }
   }
   CalcName <- FORM$formname[1]
   ## Add all current outcomes to every form
   FORM[1, "options"] <- paste0(allOutcomes, collapse=";")
   FORM[1, "value"] <- OutcomePICO
   FORM <- addTimeSpans(FORM)
   FORM <- addInterventions(FORM)
   S$IN$FORM <<- FORM
   View(S$IN$FORM)
   if(S$P$Modify) {
      saveResultsBtn <- tagList(
         bs4("c12", class="text-right", bs4("btn", uid=paste0("calcNsave_", CalcName), q="g", class="mr-3", "Save Results")))
   } else {
      saveResultsBtn <- ""
   }
   return(tagList(
      imForm2HTML(FORM),
      saveResultsBtn
   ))
})})

output$viewResults <- renderUI({c(rv$limn, rv$limnExtraction, rv$limnviewArm, rv$limnviewCalculator,
                                 rv$limnviewResults); isolate({
   if(S$NUMs$armNUM==0) {
      return("")
   }
   return(HTML("Results go here."))
})})

output$studyYbox <- renderUI({c(rv$limn, rv$limnExtraction, rv$limnviewStudy); isolate({
   if(S$NUM$armNUM>0) {
      return("")
   } else {
      yBox = HTML0("
<p>Study</p>
")
      return(tagList(
         bs4("r", class="mt-3", bs4("c12", bs4("cd", q="y", bs4("cdb", bs4("cdt", yBox)))))
      ))
   }
})})

output$ArmYbox <- renderUI({c(rv$limn, rv$limnExtraction, rv$limnviewArm); isolate({
   if(S$NUM$armNUM>0) {
      return("")
   } else {
      yBox = HTML0("
<p>Arm</p>
")
      return(tagList(
         bs4("r", class="mt-3", bs4("c12", bs4("cd", q="y", bs4("cdb", bs4("cdt", yBox)))))
      ))
   }
})})

output$CalculatorYbox <- renderUI({c(rv$limn, rv$limnExtraction, rv$limnviewArm, rv$limnviewCalculator); isolate({
   if(S$NUM$armNUM==0) {
      return("")
   } else {
      yBox = HTML0("
<p>Move pbr to means.<br>
Multiple Time spans<br>
Multiple Interventions<br>
Display results<br>
Add complete article info with possible links to full text at top.</p>
")
      return(tagList(
         bs4("r", class="mt-3", bs4("c12", bs4("cd", q="y", bs4("cdb", bs4("cdt", yBox)))))
      ))
   }
})})

output$ResultsYbox <- renderUI({c(rv$limn, rv$limnExtraction, rv$limnviewArm, rv$limnviewCalculator,
                                 rv$limnviewResults); isolate({
   if(S$NUM$armNUM==0) {
      return("")
   } else {
      yBox = HTML0("
<p>Results</p>
")
      return(tagList(
         bs4("r", class="mt-3", bs4("c12", bs4("cd", q="y", bs4("cdb", bs4("cdt", yBox)))))
      ))
   }
})})

### observer for omclick
observeEvent(input$js.omclick, {
   if(debugON) {
      cat(paste0("Click on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "pgn" = {                    # For pgn, [2] or n is the form name, [3] is the recID
         S$PKR[[n]]$activePage <<- as.numeric(uid[[1]][3])
         limnID = paste0("limn", n)         # The pickR render should respond to this rv$limn...;
         rv[[limnID]] <- rv[[limnID]] + 1    # rv$limn... also needs to be pre-defined at the top of the script
      },
      "filter" = {
         rv$limnExtraction <- rv$limnExtraction + 1
#         rv$render = rv$render+1
      },
      "menu1" = {
         S$hideMenus <<- FALSE
#         S$PGN$activePage <- 1                    # When changing submenu, set scroller back to 1
         rv$menu1Active = n
         if(n==3) {                                # Extraction, start at beginning
            S$NUMs$catalogID <<- 0
            S$NUMs$studyNUM <<- 0
            S$NUMs$armNUM <<- 0
            S$hideMenus <<- FALSE
         }
         rv$limn = rv$limn+1
      },
      "menu2" = {
         S$hideMenus <<- FALSE
         S$picoDisplay <<- "add"
#         S$PGN$activePage <- 1                    # When changing submenu, set scroller back to 1
         rv$menu2Active = n
         rv$limn = rv$limn+1
      },
      "pgn" = {                    # For pgn, [2] or n is the form name, [3] is the recID
         S$PKR[[n]]$activePage <<- as.numeric(uid[[1]][3])
         limnID = paste0("limn", n)         # The pickR render should respond to this rv$limn...;
         rv[[limnID]] = rv[[limnID]] + 1    # rv$limn... also needs to be pre-defined at the top of the script
      },
      "addPico" = {
         S$recID <<- 0
         S$picoDisplay <<- "edit"
         S$hideMenus <<- TRUE
         rv$limn = rv$limn+1
      },
      "editPico" = {
         S$recID <<- n
         S$picoDisplay <<- "edit"
         S$hideMenus <<- TRUE
         rv$limn = rv$limn+1
      },
      "viewPico" = {
         S$recID <<- n
         S$picoDisplay <<- "edit"
         S$hideMenus <<- FALSE
         rv$limn = rv$limn+1
      },
      "savePico" = {
         S$IN$recID <<- S$recID
         empty=0
         for(i in which(S$Pico$Form$placeholder=="Required...")) {
            empty <- empty + (input[[S$Pico$Form$id[i]]]=="")
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
            S$IN$FORM <<- S$Pico$Form
            rv$imGetFORMData <<- rv$imGetFORMData + 1
            S$picoDisplay <<- "add"
            S$hideMenus <<- FALSE
            rv$menuActive = 2
         }
      },
      "cancelPico" = {
         S$picoDisplay <<- "add"
         S$hideMenus <<- FALSE
         rv$menuActive = 2
         rv$limn = rv$limn+1
      },
      "deletePico" = {
         NUM <- imID2NUM(n, "pico")                                         # Get the NUM for this picoID
         picoids <- S$picoTBL %>% filter(picoNUM==NUM) %>% select(picoID) %>% collect()
         for(id in picoids$picoID) {                                        # For each ID, delete the record
            r <- recGet(S$db, "pico", "**", tibble(c("picoID", "=", id)))
            r$deleted[2] <- 1
            recSave(r, S$db)
         }
         rv$limnPico = rv$limnPico + 1
      },
      "viewStudy" = {
         S$NUMs$catalogID <<- as.numeric(n)
         S$NUMs$studyNUM <<- 0
         S$NUMs$armNUM <<- 0
         S$hideMenus <<- FALSE
         rv$limn = rv$limn+1
      },
      "viewArm" = {
         r <- recGet(S$db, "extract", "armNUM", tibble(c("extractID", "=", n)))
         S$NUMs$extractID <<- n
         S$NUMs$armNUM <<- r$armNUM
         S$hideMenus <<- FALSE
         rv$limnviewArm <- rv$limnviewArm+1          # Needed to show Arm data
      },
      "otherArm" = {
         S$NUMs$armNUM <<- 0
         S$hideMenus <<- FALSE
         rv$limnviewArm <- rv$limnviewArm+1
         rv$limnviewCalculator <- rv$limnviewCalculator + 1             # Hide the calculator

      },
      "deleteX" = {
         # This should refuse to delete an arm until all its outcomes have been deleted
         #   and refuse to delete an outcome until all its intervention data has been deleted.
         warning("deleteX needs work.")
         r <- recGet(S$db, "extract", c("catalogID", "armNUM", "outcome"),
                     tibble(c("extractID", "=", n)))
         WHERE <- tibble(
            c = c("catalogID", "=", r$catalogID),
            a = c("armNUM", "=", r$armNUM))
         if(r$outcome != "") {
            WHERE$o = c("outcome", "=", r$outcome)
         }
         r <- recGet(S$db, "extract", "extractID", WHERE)
         for(id in r$extractID) {
            rX <-recGet(S$db, "extract", "**", tibble(c("extractID", "=", id)))
            rX$deleted[2] <- 1
            rx <- recSave(rX, S$db)
         }
         rv$limnviewArm = rv$limnviewArm + 1
      },
      "addForm" = {
         S$editFORM <<- TRUE
         S$Arm$FORMname <<- n
         S$NUMs$armNUM <<- S$NUMs$armNUMnext
         S$hideMenus <<- TRUE
         rv$limn <- rv$limn + 1
      },
      "editForm" = {
         S$editFORM <<- TRUE
         S$Arm$FORMname <<- n
         S$hideMenus <<- TRUE
         rv$limn <- rv$limn + 1
      },
      "saveForm" = {
         msg=""
         switch(n,
            "PrjForm-Arm" = {
               n <- str_trim(stripHTML(as.character(input[["idArmName"]]))) == ""
               i <- is.null(input[["idArmI"]])
               ts <- is.null(input[["idArmTS"]])
               if(n) {
                  msg = paste0(msg, "<li>Arm name can't be blank</li>")
               }
               if(i) {
                  msg = paste0(msg, "<li>You must check at least one Intervention</i></li>")
               }
               if(ts) {
                  msg = paste0(msg, "<li>You must check at least one Time Span</i></li>")
               }
            }
         )
         if(msg=="") {
            S$editFORM <<- FALSE
            S$hideMenus <<- FALSE
            rv$imGetFORMData <- rv$imGetFORMData + 1
            rv$limnviewArm <- rv$limnviewArm+1
         } else {
            S$modal_title <<- "Whoops"
            S$modal_text <<- HTML("<p>Can't save form because:<ul>", msg, "</ul></p>")
            rv$modal_warning <- rv$modal_warning + 1
         }
      },
      "calcNsave" = {
         r <- calcNsave()
         rv$limnviewCalculator <- rv$limnviewCalculator + 1
      },
      "cancelForm" = {
         switch(n,
            "PrjForm-Arm" = {
               S$NUMs$armNUM <<- 0
            },
            warning(paste0("in editForm, no handler for ", n))
         )
         S$editFORM <<- FALSE
         S$hideMenus <<- FALSE
         rv$limn <- rv$limn + 1
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# This function updates the extraction table, which means it adds studies that have passed stage 1 review
#   and deletes studies that did, but don't anymore.
updateEXtable <- function() {
   start.time <- Sys.time()
   on.exit({
      cat("\n### Update extraction table ###\n")
      print(Sys.time() - start.time)
      cat("\n")
   })
   dbLink <- poolCheckout(shiny.pool)        # get a dbLink from the pool
   on.exit(poolReturn(dbLink), add = TRUE)   # Have to use dbLink to use MySQL max() function
   review <- recGet(S$db, "review", c("catalogID", "decision"), tibble(c("decision", ">", "1")))
   extract <- recGet(S$db, "extract", "catalogID", tibble(c("extractID", ">", "0")))
   workIDs <- setdiff(review$catalogID, extract$catalogID)
   badIDs <- setdiff(extract$catalogID, review$catalogID)
   progress <- Progress$new(session)         # Create a Progress object
   progress$set(message = "Updating extraction table...", value = 0) # Set message
   if(length(badIDs)>0 && badIDs[1]>0) {     # Length=0 when there are no differences; ID[1]=0 when extract table is empty
      for(cID in badIDs) {                   # Delete any extract records that are no longer S1Pass+ (can happen when a user
         r <- recGet(S$db, "extract", "extractID", tibble(c("catalogID", "=", cID)))     # edits an S1Pass+ review down)
         for(eID in r$extractID) {           # Because there can be multiple extract records with one catalog ID
            r <- recGet(S$db, "extract", "**", tibble(c("extractID", "=", eID)))
            r$deleted[2] <- 1
            r <- recSave(S$db, r)
         }
      }
   }
   if(length(workIDs)>0 && workIDs[1]>0) {   # Length=0 when there are no differnces; ID[1]=0 when review table has no S1Pass+ decisions
      dc <- recGet(S$db, "extract", "catalogID", tibble(c("deleted", ">", "0")))  # Get IDs of deleted records in extract table
      for(cID in workIDs) {
         if(cID %in% dc$catalogID) {         # If catalogID is already in the extract table, undeleted those records.
            de <- recGet(S$db, "extract", "extractID", tibble(c("catalogID", "=", cID), c("deleted", ">", "0"))) # extractIDs to undelete
            for(eID in de$extractID) {       # If there are multiple records with the same catalogID, r$extractID is a vector.
               r <- recGet(S$db, "extract", "**", tibble(c("extractID", "=", eID), c("deleted", ">", "0")))
               r$deleted[2] <- 0
               r <- recSave(S$db, r)
            }
         } else {                            # Otherwise, add the catalogID to the extract table
            sNUM <- dbGetQuery(dbLink, paste0("SELECT MAX(studyNUM) FROM `", S$db, "`.`extract` WHERE `extractID`>0;"))
               # When the extract table is empty, this returns NA; which is our signal to start at 1, er 0+1.
            studyNUM <- ifelse(is.na(sNUM[[1,1]]), 0, sNUM[[1,1]])
               # In case there's a comma after the name, eg "Weishaar, Tom", change it to "Weishaar  Tom" first,
               #   then split at the first space and take the first segment of the name.
               # This assumes last name first; once there's a database where it's not, we'll need to fix the
               #   authors in the catalog, not here...
            catalog <- recGet(S$db, "catalog", c("catalogID", "author", "Y"), tibble(c("catalogID", "=", cID)))
            studyAuthor <- (str_split(str_replace(catalog$author, ",", " "), " ")[[1]][1])
            Trial <- paste0(studyAuthor,"-",catalog$Y) # Author (before first space)-Year
            allNames <- S$extractTBL %>% filter(name=="Trial") %>% pull(value)
            i <- 2      # start with "b"; the first one has no letter at all.
            original <- Trial
            while(length(allNames)>0 && Trial %in% allNames) {   # make sure Trial is unique
               Trial <- paste0(original, letters[i])
               i <- i + 1
            }
         }
         r <- recGet(S$db, "extract", SELECT="", WHERE="")           # Get a new record
         r$catalogID[2] <- cID                                       # Save catalogID
         r$name[2] <- "Trial"                                 #    Trial
         r$value[2] <- Trial                                 #    Trial
         r$studyNUM[2] <- as.integer(studyNUM + 1)                   #    studyNUM
         r <- recSave(r, S$db)
         progress$set(which(workIDs %in% cID)/length(workIDs))       # update progress
      }
   }
   progress$close()
   return(review)
}

addTimeSpans <- function(FORM) {
   S$Arm$tsVec <- unlist(str_split(S$Arm$FORM[S$Arm$FORM$id=="idArmTS","value"], ";"))
   for(i in 1:length(S$Arm$tsVec)) {
      FORM[FORM$id==paste0("TS.",i), "value"] <- S$Arm$tsVec[i]
   }
   return(FORM)
}

addInterventions <- function(FORM) {
   S$Arm$iVec <- unlist(str_split(S$Arm$FORM[S$Arm$FORM$id=="idArmI","value"], ";"))
   for(i in 1:length(S$Arm$iVec)) {
      FORM[FORM$id==paste0("IN.",i), "value"] <- S$Arm$iVec[i]
   }
   return(FORM)
}

calcNsave <- function() {
   ids <- S$IN$FORM$id
   for(i in 1:length(ids)) {                                # Get current values, store in FORM
      if(ids[i]!="") {                                      # No quill, so we can just loop through the inputs
         S$IN$FORM$value[i] <<- str_trim(stripHTML(as.character(input[[ids[i]]])))
      }
   }
   dataVec <- S$IN$FORM$value[S$IN$FORM$id!=""]             # Drop rows with blank ids
   names(dataVec) <- ids[S$IN$FORM$id!=""]                  #    dataVec is now a named vector
   # For each Time Span by Intervention, calulate results and save in result table
   msg=""
   for(ts in 1:length(S$Arm$tsVec)) {                       # vector of Time Spans in this form
      for(i in 1:length(S$Arm$iVec)) {                      # vector of Interventions in this form
         r <- recGet(S$db, "result", SELECT="**", WHERE=tibble(c("studyNUM", "=", S$NUMs$studyNUM),
                                                               c("armNUM", "=", S$NUMs$armNUM),
                                                               c("O", "=", dataVec["OutcomePICO"])))
         r$extractID[2] <- S$NUMs$extractID
         r$catalogID[2] <- S$NUMs$catalogID
         r$studyNUM[2]  <- S$NUMs$studyNUM
         r$armNUM[2]    <- S$NUMs$armNUM
         r$P[2]  <- S$Arm$FORM %>% filter(id=="idArmP") %>% pull("value")
         r$I[2]  <- dataVec[paste0("IN.",i)]
         r$C[2]  <- S$Arm$FORM %>% filter(id=="idArmC") %>% pull("value")
         r$O[2]  <- dataVec["OutcomePICO"]
         r$TS[2] <- dataVec[paste0("TS.",ts)]
         z <- calcES(r, dataVec, ts, i)                     # calcES returns z$r and z$error
         if(!z$error) {                                     # Build error message if errors
            r <- recSave(z$r, S$db)
         } else {
            msg = paste0(msg, "<li>", r$I[2], " in ", r$TS[2], "</li>")
         }
      }
   }
   # Save the calculation FORM in the extract table
   r <- recGet(S$db, "extract", SELECT="**", WHERE=tibble(c("studyNUM", "=", S$NUMs$studyNUM),
                                                      c("armNUM", "=", S$NUMs$armNUM),
                                                      c("outcome", "=", dataVec["OutcomePICO"])))
   r$studyNUM[2] <- S$NUMs$studyNUM          # Need to fill these in in case it's a new record
   r$armNUM[2] <- S$NUMs$armNUM
   r$outcome[2] <- dataVec["OutcomePICO"]
   r$name[2] <- "Calculator"
   r$value[2] <- toJSON(S$IN$FORM)
   r <- recSave(r, S$db)
   if(msg!="") {
      S$modal_text <<- HTML("<p>Missing data is preventing the calculation of:<ul>", msg, "</ul></p>")
      S$modal_title <<- "Whoops."
      S$modal_size <<- "m"
      rv$modal_warning <- rv$modal_warning + 1
   }
}

calcES <- function(r, d, ts, i) {
   switch(d["EStype"],
      "Counts" = {
         nwC <- d[paste0("nw.0.",ts)]
         nbC <- d[paste0("nb.0.",ts)]
         nwI <- d[paste0("nw.",i,".",ts)]
         nbI <- d[paste0("nb.",i,".",ts)]
         if(nwC=="" || nbC=="" || nwI=="" || nbI=="") {
            NoData = TRUE
         } else {
            NoData = FALSE
            e <- esc_2x2(as.numeric(nbI),
                         as.numeric(nwI),
                         as.numeric(nbC),
                         as.numeric(nwC),
                         es.type="or")
            r$nC[2] <- as.numeric(nwC) + as.numeric(nbC)  # will be converted back to chr when stuffed in r
            r$nI[2] <- as.numeric(nwI) + as.numeric(nbI)
            r$es[2] <- format(e$es, digits=6)
            r$v[2]  <- format(e$v, digits=6)
         }
      },
      "Proportions" = {
         nC <- d[paste0("n.0.",ts)]
         nI <- d[paste0("n.",i,".",ts)]
         sC <- d[paste0("s.0.",ts)]
         sI <- d[paste0("s.",i,".",ts)]
         if(nC=="" || nI =="" || sC=="" || sI=="") {
            NoData = TRUE
         } else {
            NoData = FALSE
            e <- esc_bin_prop(as.numeric(sI),
                              as.numeric(nI),
                              as.numeric(sC),
                              as.numeric(nC),
                              es.type = c("or"))
            r$nC[2] <- nC
            r$nI[2] <- nI
            r$es[2] <- format(e$es, digits=6)
            r$v[2]  <- format(e$v, digits=6)
         }
      },
      "Means" = {
         print(tibble(a=names(d), b=d))
         nC  <- d[paste0("n.0.",ts)]
         nI  <- d[paste0("n.",i,".",ts)]
         switch(d["ES"],
            "Means & SEs" = {
               mC  <- d[paste0("m.0.",ts)]
               mI  <- d[paste0("m.",i,".",ts)]
               seC <- d[paste0("se.0.",ts)]
               seI <- d[paste0("se.",i,".",ts)]
               if(nC=="" || mC =="" || seC=="" || nI=="" || mI=="" || seI =="") {
                  NoData = TRUE
               } else {
                  NoData = FALSE
                  e <- esc_mean_se(as.numeric(mI),
                                   as.numeric(seI),
                                   as.numeric(nI),
                                   as.numeric(mC),
                                   as.numeric(seC),
                                   as.numeric(nC),
                                   es.type = "d")
                  r$nC[2] <- nC
                  r$nI[2] <- nI
                  r$es[2] <- format(e$es, digits=6)
                  r$v[2]  <- format(e$v, digits=6)
               }
            },
            "Means & SDs" = {
               mC  <- d[paste0("m.0.",ts)]
               mI  <- d[paste0("m.",i,".",ts)]
               sdC <- d[paste0("sd.0.",ts)]
               sdI <- d[paste0("sd.",i,".",ts)]
               if(nC=="" || mC =="" || sdC=="" || nI=="" || mI=="" || sdI =="") {
                  NoData = TRUE
               } else {
                  NoData = FALSE
                  e <- esc_mean_sd(as.numeric(mI),
                                   as.numeric(sdI),
                                   as.numeric(nI),
                                   as.numeric(mC),
                                   as.numeric(sdC),
                                   as.numeric(nC),
                                   es.type = "d")
                  r$nC[2] <- nC
                  r$nI[2] <- nI
                  r$es[2] <- format(e$es, digits=6)
                  r$v[2]  <- format(e$v, digits=6)
               }
            },
            "Means & Overall SD" = {
               mC  <- d[paste0("m.0.",ts)]
               mI  <- d[paste0("m.",i,".",ts)]
               sdI <- d[paste0("sd.",i,".",ts)]
               if(nC=="" || mC =="" || nI=="" || mI=="" || sdI =="") {
                  NoData = TRUE
               } else {
                  NoData = FALSE
                  e <- esc_mean_sd(grp1m=as.numeric(mI),
                                   grp1n=as.numeric(nI),
                                   grp2m=as.numeric(mC),
                                   grp2n=as.numeric(nC),
                                   totalsd=as.numeric(sdI),
                                   es.type = "d")
                  r$nC[2] <- nC
                  r$nI[2] <- nI
                  r$es[2] <- format(e$es, digits=6)
                  r$v[2]  <- format(e$v, digits=6)
               }
            },
            "t-test t-value" = {
               tI <- d[paste0("t.",i,".",ts)]
               if(nC=="" || nI=="" || tI=="") {
                  NoData = TRUE
               } else {
                  NoData = FALSE
                  e <- esc_t(t=as.numeric(tI),
                             grp1n=as.numeric(nI),
                             grp2n=as.numeric(nC),
                             es.type = "d")
                  r$nC[2] <- nC
                  r$nI[2] <- nI
                  r$es[2] <- format(e$es, digits=6)
                  r$v[2]  <- format(e$v, digits=6)
               }
            },
            "t-test p-value" = {
               pI <- d[paste0("p.",i,".",ts)]
               if(nC=="" || nI =="" || pI =="") {
                  NoData = TRUE
               } else {
                  NoData = FALSE
                  e <- esc_t(p=as.numeric(pI),
                             grp1n=as.numeric(nI),
                             grp2n=as.numeric(nC),
                             es.type = "d")
                  r$nC[2] <- nC
                  r$nI[2] <- nI
                  r$es[2] <- format(e$es, digits=6)
                  r$v[2]  <- format(e$v, digits=6)
               }
            }
         )
      },
      "Effect sizes" = {
         nC <- d[paste0("n.0.",ts)]
         nI <- d[paste0("n.",i,".",ts)]
         es <- d[paste0("es.",i,".",ts)]
         v  <- d[paste0("v.",i,".",ts)]
         if(nC=="" || nI =="" || es=="" || v=="") {
            NoData = TRUE
         } else {
            NoData = FALSE
            r$nC[2] <- nC
            r$nI[2] <- nI
            r$es[2] <- es
            r$v[2]  <- v
            e <- list()
            e$se <- e$ci.lo <- e$ci.hi <- e$w <- ""
            e$info <- paste0("User-entered effect size ", d["ES"])
            switch(d["ES"],
               "Cohen's d" = { e$measure <- "d"},
               "Hedge's g" = { e$measure <- "g"},
               "Point-biserial r" = { e$measure <- "r"},
               "Relative Risk" = { e$measure <- "rr"},
               "Log Relative Risk" = { e$measure <- "lrr"},
               "Odds Ratio" = { e$measure <- "or"},
               "Log Odds Ratio" = { e$measure <- "lor"}
            )
         }
      }
   )
   if(!NoData) {
      r$info[2]   <- e$info
      r$esType[2] <- e$measure
      r$se[2]     <- format(e$se, digits=6)
      r$ci.lo[2]  <- format(e$ci.lo, digits=6)
      r$ci.hi[2]  <- format(e$ci.hi, digits=6)
      r$weight[2] <- format(e$w, digits=6)
   }
   print(tibble(a=names(d), b=d))
   return(list(r=r, error=NoData))
}

whichCalculator <- function(EStype, ES) {
   switch(EStype,
      "Effect sizes" = {
         switch(ES,
            "Cohen's d" = { CalcName <- "Form-ES-d" },
            "Hedge's g" = { CalcName <- "Form-ES-g" },
            "Relative Risk" = { CalcName <- "Form-ES-rr" },
            "Log Relative Risk" = { CalcName <- "Form-ES-Lrr" },
            "Odds Ratio" = { CalcName <- "Form-ES-or" },
            "Log Odds Ratio" = { CalcName <- "Form-ES-Lor" },
            CalcName <- "Form-ES-d"        # Default when EStype changes.
         )
      },
      "Means" = {
         switch(ES,
            "Means & SEs" = { CalcName <- "Form-ESC-Mean&SE" },
            "Means & SDs" = { CalcName <- "Form-ESC-Mean&SD" },
            "Means & Overall SD" = { CalcName <- "Form-ESC-Mean&OverallSD" },
            "t-test t-value" = { CalcName <- "Form-ESC-t-test-t" },
            "t-test p-value" = { CalcName <- "Form-ESC-t-test-p" },
            "Point-Biserial r" = { CalcName <- "Form-ES-pbr" },
            CalcName <- "Form-ESC-Mean&SE" # Default when EStype changes.
         )
      },
      "Proportions" = { CalcName <- "Form-ESC-Proportions" },
      "Counts" = { CalcName <- "Form-ESC-Counts" }
   )
   return(imGetFORM(CalcName, "om$prime"))
}
