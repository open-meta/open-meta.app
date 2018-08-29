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

S$OC$NUM = 0

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
                  DTOutput("showOutcomes"),
                  uiOutput("yboxOutcomes")))
            )
            output$addOutcome = renderUI(tagList(
               bs4("r", bs4("c9",
                  HTML0("Outcome Name<br>"),
                  textInput("outcomeName", label=NULL, value="", width="100%")),
               bs4("c3", align="sc", bs4("btn", uid="new_0", q="g", class="mt-1", "Add Outcome"))),
               bs4("hr")

         ))
            output$showOutcomes  <- renderDT({
               Rx = omRx(                 # Get project's outcomes
                  db = S$db,
                  table = "outcome",
                  SELECT = c("outcomeNUM", "value"),
                  WHERE = tibble(c("name", "=", "outcomeName")),
                  buttons = list(view=list(id="viewOutcome", label="View", q="b", class=""),
                                 edit=list(id="editOutcome", label="Edit", q="g", class=""))
               )
               # Adjust which buttons will show in table
               if(nrow(Rx)>0) {                     # Skip this if nothing was returned.
                  if(!S$P$Modify) {                    # Without permission to modify all you can do is View
                     Rx = Rx[-4]                       #    delete all button columns but View
                  } else {
                     Rx = Rx[-3]                    # Remove View buttons
                  }
                  Rx <- Rx[,-1]                     # Remove ID column
               }
               ###
               omDT(Rx,
                  if(S$P$Modify) {
                     cnames = c("Outcome Name", "Edit Details")
                  } else {
                     cnames = c("Outcome Name", "View Details")
                  },
                  colesc = c(1),        # columns to escape (minus means don't escape)
                  noText = "No outcomes found for this project"      # What to say when there are no results
               )
            },
            # renderDT() parameters go here:
            server = FALSE
            )
            output$yboxOutcomes = renderUI(tagList(
               bs4("r", class="mt-3", bs4("c12", bs4("cd", q="y", bs4("cdb", bs4("cdt", HTML0(
"<p>If you are a member of this project with appropriate permissions, you can </p>
"))))))
         ))
         },
         "3" = {
            restOfPage = "Trials-Arms-Groups Here"
         },
         "4" = {
            loopR = ""
            for(i in 1:nrow(S$OC$data)) {
               loopR = paste0(loopR, bs4(S$OC$data$widget[i], id=S$OC$data$id[i], ck=S$OC$data$value[i], S$OC$data$label[i]))
            }
            restOfPage = tagList(
               HTML("<pre>", S$OC$data, "</pre>"),
               HTML(loopR),
               bs4("d", class="text-right mt-3",
                  bs4("btn", id="cancel", n=1, q="b", "Cancel"),
                  bs4("btn", id="save", n=1, q="b", "Save Outcome Details")
               )
            )
         }
      )

      # Javascript notes:
      # $('#tinid')[0].value gets what's in a text input
      # $('#cbxid')[0].checked gets status of a checkbox



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
         rv$menuActive = n
         rv$limn = rv$limn+1
      },
      "new" = {
         if(input$outcomeName=="") {
            S$modal_title <<- "Name required."
            S$modal_text <<- HTML("<p>Please provide a name for the new outcome.</p>")
            S$modal_size <<- "s"
            rv$modal_warning <- rv$modal_warning + 1
         } else {
            outcome <- recGet(S$db, "outcome", SELECT="", WHERE="")
            outcome$name[2]  <- "outcomeName"
            outcome$value[2] <- htmlEscape(input$outcomeName)
            outcome <- recSave(outcome, S$db)                # At this point, outcomeNUM is always zero;
            outcome$outcomeNUM[2] <- outcome$outcomeID[2]    #   Change it to outcomeID of this initial record
            outcome <- recSave(outcome, S$db)
            rv$limn = rv$limn+1
         }
      },
      "editOutcome" = {
         S$OC$NUM <<- n
         S$OC$data <<- recGet(S$db, "outcome", SELECT=c("outcomeNUM", "name", "value"), WHERE=tibble(c("outcomeNUM", "=", S$OC$NUM)))
         S$OC$data <<- tibble(widget=c("tin", "cbx"), id=c("tinid", "cbxid"), label=c("text", "checkbox"),
                              value=c("", "TRUE"))
         S$hideMenus <<- TRUE
         rv$menuActive = 4
         rv$limn = rv$limn+1
      },
      "save" = {
         S$hideMenus <<- FALSE
         rv$menuActive = 2
         rv$limn = rv$limn+1
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


