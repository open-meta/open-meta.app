### open-meta.app prjActive.R
### Tom Weishaar - Feb 2018 - v0.1

#S$userOK <- S$U$sPowers > 0
#S$whoopsPage <- FALSE

rv$limn <- rv$limn + 1

output$uiHead <- renderUI({
   if(debugON) {
      cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
   }
   return(bigHead)
})

output$uiMeat <- renderUI({rv$limn; isolate({
   # if(S$whoopsPage) {
   #    return(tagList(
   #       bs4("r", align="hc",
   #          bs4("c8", class="mt-3",
   #             HTML0("<h5>You must be logged in to join a project and you must register before you can log in.</h5>")
   #          )
   #       ),
   #          bs4("ca", class="mt-4 text-center",
   #             bs4("btn", id="login", n=1, class="mr-2", q=c("b", "l", "p"), "Login"),
   #             bs4("btn", id="register", n=1, q=c("b", "l", "p"), "Register")
   #          )
   #    ))
   # } else {
      return(tagList(
         bs4("r", align="hc",
            bs4("c9",
               DTOutput("uiResults")  # Can't bs4 this one...
            )
         )
      ))
   # }
})})

### Results
uiResultsRV = reactiveValues()                        # Make a new one of these for each output
uiResultsRV$WHERE = tibble(c("status", "<", 2), c("status", "<", 100), c("deleted", "=", 0))      # default filter
uiResultsRV$newWHERE = 0                              # inc this when WHERE changes

output$uiResults  <- renderDT(
   {z = uiResultsRV$newWHERE
    Rx = omRx(
      db = "om$prime",
      table = "project",
      SELECT = c("projectID", "projectName"),         # ID (required) + fields to display; (ID field will be removed)
      WHERE = uiResultsRV$WHERE,
      buttons = list(show=list(id="view", label="View Project", q="b", class=""),
                     join=list(id="join", label="Join Project", q="g", class="")))
   # if you need to further modify Rx, you can do it here.
   Rx <- Rx[,-1]                                      # Remove ID column
   omDT(Rx,
      cnames = c("Project Name", "View", "Join"),     # names for column headers
      colesc = c(-1,-2,-3),                           # columns to escape (minus means don't escape)
      noText = "There are no active projects yet"     # What to say when there are no results
   )},
# renderDT() parameters go here:
   server = FALSE
)

### observer for omclick
observeEvent(input$js.omclick, {
   if(debugON) {
      cat(paste0("Click on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "login" = { js$redirect("?login") },
      "register" = { js$redirect("?profile") },
      "view" = { js$redirect(paste0("?Protocol&prj=", n)) },
      "join" = {
         js$redirect(paste0("?Join&prj=", n))
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

