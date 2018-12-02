### open-meta.app prjFinished.R
### Tom Weishaar - Feb 2018 - v0.1

S$userOK <- S$U$sPowers > 0

output$uiHead <- renderUI(bigHead)   # can use slimHead or bigHead

output$uiMeat <- renderUI({rv$limn; isolate({
   return(tagList(
      bs4("r", align="hc",
         bs4("c6",
            DTOutput("uiResults")  # Can't bs4 this one...
         )
      )
   ))
})})

### Results
uiResultsRV = reactiveValues()                        # Make a new one of these for each output
uiResultsRV$WHERE = tibble(c("status", ">=", 2), c("deleted", "=", 0))      # default filter
uiResultsRV$newWHERE = 0                              # inc this when WHERE changes

output$uiResults  <- renderDT(
   {z = uiResultsRV$newWHERE
    Rx = omRx(
      db = "om$prime",
      table = "project",
      SELECT = c("projectID", "projectName"),         # ID (required) + fields to display; (ID field will be removed)
      WHERE = uiResultsRV$WHERE,
      buttons = list(show=list(id="view", label="View Project", q="b", class="")))
   # if you need to further modify Rx, you can do it here.
   Rx <- Rx[,-1]                                      # Remove ID column
   omDT(Rx,
      cnames = c("Project Name", "View Project"),     # names for column headers
      colesc = c(1:1),                                # columns to escape (all except button columns)
      noText = "There are no finished projects yet."  # What to say when there are no results
   )},
# renderDT() parameters go here:
   server = FALSE
)

### observer for omclick
observeEvent(input$js.omclick, {
   if(A$debugON) {
      cat(paste0("Click on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "login" = { js$redirect("?login") },
      "register" = { js$redirect("?profile") },
      "view" = { js$redirect(paste0("?Protocol&prj=", n)) },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

