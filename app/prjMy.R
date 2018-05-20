### open-meta.app prjMy.R
### Tom Weishaar - Feb 2018 - v0.1

S$userOK <- S$U$sPowers > 0

output$uiHead <- renderUI({
   if(debugON) {
      cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
   }
   return(tagList(bigHead))
})

S$Mlist = membershipGet(c("projectID", "role"), tibble(c("userID", "=", S$U$userID)))

output$uiMeat <- renderUI({rv$limn; isolate({
   if(S$Mlist$projectID==0) {
      return(tagList(
         bs4("r", align="hc",
            bs4("ca",
               tagList(h5(class="text-center m-3 lead", HTML("You haven\'t joined any projects yet.")))
            )
         )
      ))
   } else {
      return(tagList(
         bs4("r", align="hc",
            bs4("c10",
               DTOutput("uiResults")  # Can't bs4 this one...
            )
         )
      ))
   }
})})

### Results
output$uiResults  <- renderDT(
   {Rx = omRx(
      db = "om$prime",
      table = "project",
      SELECT = c("projectID", "projectName", "status"),   # ID (required) + fields to display; (ID field will be removed)
      WHERE = tibble(c("projectID", " IN ", paste0("(", paste0(S$Mlist$projectID, collapse=","), ")"))),
      buttons = list(show=list(id="view", label="View Project", q="b", class="")))
   # if you need to further modify Rx, you can do it here.
   role = rep("", nrow(Rx))
   for(i in 1:nrow(Rx)) {
      role[i] = S$Mlist$role[S$Mlist$projectID == Rx$projectID[i]]        # for each user, determine role
   }
   Rx <- Rx[,-1]                                      # Remove ID column
   Rx[,4] <- Rx[,3]
   Rx[3] <- role
   Rx$status = ifelse(Rx$status==0, "Not yet active", ifelse(Rx$status>=2, "Finished", "Active"))
   omDT(Rx,
      cnames = c("Project Name", "Status", "Your Role", "View Project"),     # names for column headers
      colesc = c(2),                                  # columns to escape (all except button columns)
      noText = "You haven't joined or started any projects yet."  # What to say when there are no results
   )
   },
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
      "view" = { js$redirect(paste0("?Protocol&prj=", n))  },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

