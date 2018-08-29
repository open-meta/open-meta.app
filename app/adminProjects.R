### open-meta.app adminProjects.R
### Tom Weishaar - Feb 2018 - v0.1

# Although users without superpowers can't get here via the menus, they CAN get here by typing in the URL.
S$userOK <- S$U$sPowers >= S$PG$spReq

S$editPage <- FALSE               # editPage flag determines what gets rendered
S$sacredFields <- c("verNum", "verUser", "verTime", "clash", "clashFacts", "deleted")   # sql.core needs these to save a partial record

# Note:
# ...$user is the logged in user

output$uiHead <- renderUI({        # No rv$limn because this only needs to be rendered once.
   if(debugON) {
      cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
   }
   if(S$userOK) {
      text=c("Control Panel","Users", "Pages", "Projects", "User Projects", "Project Users")
      links=c("?adminCP", "?adminUsers", "?adminPages", "?adminProjects", "?adminUserPrj", "?adminPrjUser")
      active=which(links %in% paste0("?", S$PG$pageName))
      if(length(active)==0) {active=0}
      return(tagList(
         slimHead,
         bs4("hrm", text=text, links=links, active=active)))
   } else {
      return(tagList(
         slimHead,
         bs4("hrt", "You haven't been granted access to this page.")
      ))
   }
})

output$uiMeat <- renderUI({rv$limn; isolate({
   if(rv$limn && S$userOK) { # check permissions again
      if(!S$editPage) {                # Show results list
         return(tagList(
            bs4("r", align="hc",
               bs4("c8",
                  bs4("d", id="uiFilters"),
                  DTOutput("uiResults")  # Can't bs4 this one...
               )
            )
         ))
      }
      if(S$editPage) {                 # Edit the page
         loadRCTbutton <- {
            allReviews <- recGet(S$db, "review", c("catalogID"), tibble(c("catalogID", ">", 0)))
            if(allReviews$catalogID[1]==0 && S$px$projectID[2]=="1" &&
               S$px$projectName[2]=='Effect of daily vitamin D<sub>3</sub> supplementation on human health and performance') {
               tagList(bs4("btn", id="loadRCTreviews", n=1, q="r", "Load RCT Reviews"))
            } else {
               tagList("")
            }
         }
         dCard <- deletedCard(S$px)
         cCard <- clashCard(S$px)
         return(tagList(
            bs4("r", align="hc",
               bs4("c5",
               { if((dCard[1])!="") {
                  dCard
               } else {
                  tagList(cCard,
                     HTML0('projectID: ', S$px$projectID[2], '<br/>'),
                        ttextInput(inputId="projectName", label="Project Name",
                           groupClass = "w-100",
                           inputClass = "w-100",
                           value=S$px$projectName[2],
                           autofocus = TRUE),
                     HTML0(bTime(S$px$verTime[2]), ' - Last edit (',
                           S$px$verUser[2], ' v.',
                           S$px$verNum[2], ')<br/>'),

                     HTML('<div class="text-right mt-3">'),
                        loadRCTbutton,
                        bs4("btn", id="Fdelete", n=S$px$projectID[2], q="r", "Delete Project"),
                        bs4("btn", id="cancel", n=S$ux$userID[2], q="b", "Cancel"),
                        bs4("btn", id="save", n=S$ux$userID[2], q="b", "Save"),
                     HTML('</div>')
                  )
               }}
            ))
         ))
      }
   } # if permissions check fails, return nothing, there's already a message in output$uiHead...
})})

### Filters
rv$menuActive=1    # start with Active records only
output$uiFilters <- renderUI({
   return(tagList(
            bs4("md", id="um1", n=1:3, active=rv$menuActive, text=c("Active Projects", "Deleted Projects", "Clashes"))
         ))
})

### Results
uiResultsRV = reactiveValues()                        # Make a new one of these for each output
uiResultsRV$WHERE = tibble(c("deleted", "=", 0))      # default filter
uiResultsRV$newWHERE = 0                              # inc this when WHERE changes

output$uiResults  <- renderDT(
   {z = uiResultsRV$newWHERE
    Rx = omRx(
      db = "om$prime",
      table = "project",
      SELECT = c("projectID", "projectName"),
      WHERE = uiResultsRV$WHERE,
      buttons = list(edit=list(id="edit", label="Edit Details", q="y", class=""))
   )
   # if you need to further modify Rx, you can do it here.
   Rx <- Rx[,-1]                                      # Remove ID column
   omDT(Rx,
      cnames = c("Project Name", "Edit"),             # names for column headers
      colesc = c(-1,-2),                              # columns to escape (minus means don't escape)
      noText = "No projects found"                    # What to say when there are no results
   )},
# renderDT() parameters go here:
   server = FALSE
)

### observer for omclick
observeEvent(input$js.omclick, {
   if(debugON) {
      cat(paste0("\nClick on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "edit" = {
         S$editPage <<- TRUE
         S$px <<- projectGet("**", tibble(c("projectID", "=", n), c("deleted", ">=", "0")))
         S$db <<- paste0("om$prj_", S$px$projectID[2])
         rv$limn = rv$limn + 1
      },
      "um1" = {
         rv$menuActive=n
         if(n==1) {
            uiResultsRV$WHERE = tibble(c("deleted", "=", 0))
         }
         if(n==2) {
            uiResultsRV$WHERE = tibble(c("deleted", "=", 1))
         }
         if(n==3) {
            uiResultsRV$WHERE = tibble(c("clash", ">", 0))
         }
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1
         rv$limn = rv$limn + 1
      },
      "undelete" = {
         S$px$deleted[2] <<- 0
         S$px <<- recSave(S$px)
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1     # didn't change the WHERE, but did change WHERE's results
         rv$limn = rv$limn + 1
      },
      "clearClash" = {
         S$px$clash[2] <<- 0
         S$px <<- recSave(S$px)
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1     # didn't change the WHERE, but did change WHERE's results
         rv$limn = rv$limn + 1
      },
      "Fdelete" = {
         S$px$deleted[2] <<- 1
         S$px <<- recSave(S$px)                   # recSave won't return a deleted record
         S$editPage <<- FALSE                     #   so we have to go back to the list.
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1     # didn't change the WHERE, but did change WHERE's results
         rv$limn = rv$limn + 1
      },
      "save" = {
         S$px$projectName[2] <<- input$projectName   # No esc() here! This is Site Admin only, after all.
         if(checkInput(c("projectName"), c(S$px$projectName[2]))) {
            S$px <<- recSave(S$px)
            S$editPage <<- FALSE
            uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1  # didn't change the WHERE, but did change WHERE's results
            rv$menuActive = 1
         }
         rv$limn = rv$limn + 1                    # Need a new id on the save button or it won't work twice
      },
      "cancelNew" = {
         S$editPage <<- FALSE
         rv$menuActive = 1
         uiResultsRV$WHERE = tibble(c("deleted", "=", 0))
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1
         rv$limn = rv$limn + 1
      },
      "cancel" = {
         S$editPage <<- FALSE
         rv$limn = rv$limn + 1
      },
      "loadRCTreviews" = {
         S$PM$progress <<- shiny::Progress$new()              # Create a Progress object
         S$PM$progress$set(message = "Processing reviews...", value = 0) # Set message
         Xrev <- readRDS("Xrev.RDS")
         nRevs <- nrow(Xrev)
         recs <- recGet(S$db, "catalog", c("catalogID", "title"), tibble(c("dupOf", "=", 0)))
         review <- recGet(S$db, "review", "", "")
         for(i in 1:nRevs) {
            S$PM$progress$set(i/nRevs)
            review$catalogID[2] <- recs$catalogID[recs$title==Xrev$title[i]]
            review$decision[2]  <- as.integer(Xrev$decision[i])
            review$detail[2]    <- Xrev$detail[i]
            review$comment[2]   <- Xrev$comment[i]
            cite = recGet(S$db, "catalog", c("catalogID", "reviewCount", "reviewBest", S$sacredFields), tibble(c("catalogID", "=", review$catalogID[2])))
            cite[2,]=cite[1,]                                              # need two rows for recSave
            cite$reviewCount[2] <- 1L
            cite$reviewBest[2]  <- review$decision[2]
            cite    <- recSave(cite, S$db)
            review2 <- recSave(review, S$db)
         }
         r = recGet(S$db, "settings", "**", WHERE=tibble(c("name", "=", "failBoxNames")))
         r$value[2] = toJSON(c("Not in English", "Not an RCT", "No valid participants",
                               "NVI - not daily", "NVI - not D3", "NVI - other",
                               "No valid comparison", "No valid outcome"))
         r = recSaveR(r, db=S$db)
         S$PM$progress$close()
         S$editPage <<- FALSE
         rv$limn = rv$limn + 1
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

checkInput = function(n, v) { # name, value (can be a vector)
   if(length(n)!=length(v)) { stop("In checkInput, n and v have different lengths.") }
   vmsg <- ""                 # initialize
   for(i in 1:length(n)) {
      switch(n[i],
         projectName = {   # if the name hasn't changed, no error (else we'd get an error on name check!)
            if(str_to_lower(S$px$projectName[1])!=str_to_lower(S$px$projectName[2])) {
               if(v[i]=="") {
                  vmsg = HTML0(vmsg, "<li>Project name can't be blank.</li>")
               }
               if(projectGet("projectID", tibble(c("projectName", "=", v[i])))$projectID > 0) { # check againt all existing projectNames
                  vmsg <- HTML0(vmsg, "<li>", v[i], " is taken.</li>")
               }
            }
         },
         message(paste0("In checkInput, no code for ", incoming[i]))
      )
   }
   if(nchar(vmsg) > 0 ) {
         S$modal_title <<- "Validation Error."
         S$modal_text <<- HTML("<p>One or more fields have invalid values:<ul>", vmsg, "</ul></p>")
         rv$modal_warning <- rv$modal_warning + 1
         return(FALSE)
   }
   return(TRUE)
}
