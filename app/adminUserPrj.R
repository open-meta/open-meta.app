### open-meta.app adminUserPrj.R
### Tom Weishaar - Feb 2018 - v0.1

# Although users without superpowers can't get here via the menus, they CAN get here by typing in the URL.
S$userOK <- S$U$sPowers >= S$PG$spReq

S$showPage <- FALSE               # flags determines what gets rendered

# Note:
# ...S$U is the logged in user

output$uiHead <- renderUI({        # No rv$limn because this only needs to be rendered once.
   if(debugON) {
      cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
   }
   if(S$userOK) {
      text=c("Control Panel","Users", "Pages", "Projects", "User Projects", "Project Users", "Forms")
      links=c("?adminCP", "?adminUsers", "?adminPages", "?adminProjects", "?adminUserPrj", "?adminPrjUser", "?adminForms")
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
   if(rv$limn && S$userOK) {             # check permissions again
      if(!S$showPage) {  # Show results list
         return(tagList(
            bs4("r", align="hc",
               bs4("c6",
                  bs4("d", id="uiFilters"),
                  DTOutput("uiResults")  # Can't bs4 this one...
               )
            )
         ))
      }
      if(S$showPage) {                   # Show the Project's Users
         return(tagList(
            bs4("r", align="hc",
               bs4("c9",
                  tagList(
                     HTML0('<h4>User: ', S$ux$userName, '</h4><br/>'),
                     HTML0("<h5>User's Projects:</h5>"),
                     DTOutput("showResults"),
                     HTML('<div class="text-right mt-3">'),
                        bs4("btn", id="cancel", n=S$ux$userID, q="b", "Return to User List"),
                     HTML('</div>')
                  )
               )
            ))
         )
      }
   } # if permissions check fails, return nothing, there's already a message in output$uiHead...
})})


### Filters
rv$menuActive=1                                       # start with Active records only
output$uiFilters <- renderUI({
   return(tagList(
            bs4("md", id="um1", n=1, active=rv$menuActive, text=c("Active Users"))
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
      table = "user",
      SELECT = c("userID", "userName", "email",
                 "namePrefix","nameFirst", "nameMiddle", "nameLast", "nameSuffix"),
      WHERE = uiResultsRV$WHERE,
      buttons = list(show=list(id="show", label="Show Projects", q="y", class=""))
   )
   # if you need to further modify Rx, you can do it here.
   if(nrow(Rx)>0) {
      fn = rep("", nrow(Rx))                            # if there are rows, exchange 5 name part columns
      for(i in 1:nrow(Rx)) {                            #    for one fullname column
         fn[i] = fullName(Rx[i,])
      }
      Rx[,4] = fn
      Rx = Rx[,-(5:8)]
   }
   Rx <- Rx[,-1]                                        # Remove ID column
   omDT(Rx,
      cnames = c("Username", "Email Adr", "Full Name", "User's Projects"), # names for column headers
      colesc = c(1:3),                                  # columns to escape (all except button columns)
      noText = "No Users found"                         # What to say when there are no results
   )},
# renderDT() parameters go here:
   server = FALSE
)

### showResults (list of users for the selected project)
showResultsRV = reactiveValues()                      # Make a new one of these for each output
showResultsRV$newWHERE = 0                            # inc this when WHERE changes
showResultsRV$WHERE = tibble(c("deleted", "=", 0))

output$showResults  <- renderDT(
   {z = showResultsRV$newWHERE
   Rx = omRx(
      db = "om$prime",
      table = "project",
      SELECT = c("projectID", "projectName"),         # ID (required) + fields to display; (ID field will be removed)
      WHERE = showResultsRV$WHERE,
      buttons = list()
   )
   # if you need to further modify Rx, you can do it here.
   Rx <- Rx[,-1]                                      # Remove ID column
   omDT(Rx,
      cnames = c("Project Name"),
      colesc = c(-1),                                 # columns to escape (all except button columns)
      noText = "No projects found for this user"      # What to say when there are no results
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
      "show" = {
         S$showPage <<- TRUE
         S$ux <<- userGet(c("userID", "userName"), tibble(c("userID", "=", n)))
         S$mem <<- membershipGet(c("projectID"), tibble(c("userID", "=", n)))
         showResultsRV$WHERE = tibble(c("projectID", " IN ", paste0("(", paste0(S$mem$projectID, collapse=","), ")")))
         showResultsRV$newWHERE = showResultsRV$newWHERE + 1
         rv$limn = rv$limn + 1
      },
      "um1" = {},    # do nothing
      "cancel" = {
         S$showPage <<- FALSE
         S$emailPage <<- FALSE
         rv$limn = rv$limn + 1
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

