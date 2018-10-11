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

rv$limnStudies <- 0
S$PRK$Studies$activePage <- 1

S$picoName = ""
S$display = "add"

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
   review <- updateEXtable()
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

picoFix <- function(x) {
   x <- x[,-2]            # delete "name" column, leave "values"
   x[1,"Action"] = ""     # delete buttons from first row (No Response)
   x[2,"Action"] = str_replace(x[2,"Action"], "btn-danger", "btn-danger disabled")
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
   S$IN$FORM <<- imGetFORM(S$picoName[2])        # S$picoName is set in picoPickR
   if(S$recID>0) {                               # If this is an edit, get the FORM's current values; S$recID set in addPico, editPico
      R <- recGet(S$db, "pico", c("name", "value"), tibble(c("picoNUM", "=", imID2NUM(S$recID, "pico"))))
      for(i in 1:nrow(S$IN$FORM)) {              # Insert values from R into form$value
         S$IN$FORM$value[i] <<- R$value[R$name==S$IN$FORM$name[i]]    # In FORM, "name" is the short label
      }                                                               # In R, it's the "name" of the "value"
   }
   if(S$P$Modify) {
      SaveBtn = HTML0(bs4("btn", id="save", n=1, q="b", "Save"))
   } else {                                      # If user can't modify inputs, force View (disabled inputs)
      S$IN$FORM$disable <<- TRUE                 #    and skip the Save button
      SaveBtn = ""
   }
   restOfPage = tagList(
      imForm2HTML(S$IN$FORM),
      bs4("r", align="he",
          bs4("c3", bs4("btn", id="cancel", n=1, q="b", "Cancel"), SaveBtn)
      )
   )
})})

output$Extraction <- renderUI({c(rv$limn, rv[["limnStudies"]]); isolate({ # !!!Note that rv$limnForms must be rv[[paste0("limn",ID)]]!!!
   if(TRUE) {
      S$E$reviews <<- updateEXtable()
   ###### pickR start
      ID = "Studies"                                                 # This allows multiple pickRs on a single page
      activePage = ifelse(is.null(S$PKR[[ID]]$activePage), 1, S$PKR[[ID]]$activePage)
      TABLE = "extract"                                              # The table the pickR data will come from
      SELECT = c("studyName", "catalogID")                           # These are the table fields needed to build the pickR
      WHERE = tibble(c("studyName", "!=", ""))                       # This is the incoming filter
      HeadlineF = THRUb                                              # THRUb returns "", as we have no headline
      if(S$P$Modify) {                                               # View or Edit depends on permissions
        ButtonData <- list(extract=list(id=paste0("extractStudy"), q="g", class="mr-2", label="Extract"))
      } else {
        ButtonData <- list(view=list(id=paste0("viewStudy"), q="b", label="View"))
      }
      ButtonF = stdButtons                                           # use just the function name; no quotes, no ()
      FixDataF = studyFix
      FormatF = prf_5.3.4r
      NOtext = "No studies found by this filter."
      itemsPerPage = S$PKR$itemsPerPage                              # Modifiable pickR-by-pickR
      scroll = FALSE                                                 # Modifiable pickR-by-pickR
      return(pickR(ID, activePage, S$db, TABLE, SELECT, WHERE, HeadlineF, ButtonData, ButtonF, FixDataF, FormatF, NOtext, itemsPerPage, scroll))
   }
})})

studyFix <- function(R) {
   statusText=c("Not reviewed", "Stage 1 Fail", "Stage 1 Pass", "Extraction Fail", "Extraction Pass")
   R$status <- ""
   for(i in 1:nrow(R)) {
      R$status[i] <- statusText[S$E$reviews$decision[R$catalogID[i]==S$E$reviews$catalogID]+1]
   }
   return(tibble(extractID=R$extractID, studyName=R$studyName,       # reorder columns, drop catalogID
                    status=R$status, action=R$Action))
}

prf_5.3.4r = function(r) {                        # Standard function for one column of data and one row of buttons
   return(paste0(
'<div class="col-12"><div class="row">
   <div class="col-5">', r[[1,]], '</div>
   <div class="col-3">', r[[2,]], '</div>
   <div class="col-4 text-right">', r[[3,]], '</div>',
   bs4('c12', bs4('hr0', class="py-2")), '
</div></div>', collapse = ''))
}


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
   # You enter the data for the groups, save them, do this for all outcomes, then for all arms, then for all Trials, and you're done.

#    return(
#       tagList(
#          bs4("c12", HTML("<pre>Length of review is ", nrow(review)," and it is ", review$catalogID, "</pre>"))
#       )
#    )
# })})


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

# This function updates the extraction table, which means it adds studies that have passed stage 1 review
#   and deletes studies that did, but don't anymore.
updateEXtable <- function() {
   start.time <- Sys.time()
   on.exit({
      cat("### Update extraction table ###\n")
      print(Sys.time() - start.time)
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
            studyName <- paste0(studyAuthor,"-",catalog$Y) # Author (before first space)-Year
            allNames <- recGet(S$db, "extract", "studyName", tibble(c("extractID", ">", "0")))$studyName
            i <- 2      # start with "b"; the first one has no letter at all.
            original <- studyName
            while(length(allNames)>0 && studyName %in% allNames) {   # make sure studyName is unique
               studyName <- paste0(original, letters[i])
               i <- i + 1
            }
         }
         r <- recGet(S$db, "extract", SELECT="", WHERE="")           # Get a new record
         r$catalogID[2] <- cID                                       # Save catalogID
         r$studyName[2] <- studyName                                 #    studyName
         r$studyNUM[2] <- as.integer(studyNUM + 1)                   #    studyNUM
         r <- recSave(r, S$db)
         progress$set(which(workIDs %in% cID)/length(workIDs))       # update progress
      }
   }
   progress$close()
   return(review)
}

