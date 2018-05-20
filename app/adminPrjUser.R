### open-meta.app adminPrjUser.R
### Tom Weishaar - Feb 2018 - v0.1

source("email-core.R", local=TRUE)

# Although users without superpowers can't get here via the menus, they CAN get here by typing in the URL.
S$userOK <- S$U$sPowers >= S$PG$spReq

S$showPage <- FALSE               # flags determines what gets rendered
S$emailPage <- FALSE

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
   if(rv$limn && S$userOK) {             # check permissions again
      if(!S$showPage && !S$emailPage) {  # Show results list
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
                     HTML0('<h4>Project: ', S$PRJ$projectName, '</h4><br/>'),
                     HTML0('<h5>Project members:</h5>'),
                     DTOutput("showResults"),
                     HTML('<div class="text-right mt-3">'),
                        bs4("btn", id="cancel", n=S$ux$userID, q="b", "Return to Project List"),
                     HTML('</div>')
                  )
               )
            ))
         )
      }
      if(S$emailPage) {                   # Email selected user
         h4text = paste0("Sending email to ", S$ux$userName, " (", S$ux$email, ")")
         return(tagList(
            bs4("r", align="hc",
               bs4("c7",
                  emailWrite(h4text)
            ))
         ))
      }

   } # if permissions check fails, return nothing, there's already a message in output$uiHead...
})})


### Filters
rv$menuActive=1                                       # start with Active records only
output$uiFilters <- renderUI({
   return(tagList(
            bs4("md", id="um1", n=1, active=rv$menuActive, text=c("Active Projects"))
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
      SELECT = c("projectID", "projectName"),         # ID (required) + fields to display; (ID field will be removed)
      WHERE = uiResultsRV$WHERE,
      buttons = list(show=list(id="show", label="Show Members", q="y", class="")))
   # if you need to further modify Rx, you can do it here.
   Rx <- Rx[,-1]                                      # Remove ID column
   omDT(Rx,
      cnames = c("Project Name", "Show Members"),     # names for column headers
      colesc = c(-1,-2),                              # columns to escape (minus means don't escape)
      noText = "No projects found"                    # What to say when there are no results
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
      table = "user",
      SELECT = c("userID", "userName", "email",
                 "namePrefix","nameFirst", "nameMiddle", "nameLast", "nameSuffix"),
      WHERE = showResultsRV$WHERE,
      buttons = list(email=list(id="email", label="Send Email", q="g", class="")))
   # if you need to further modify Rx, you can do it here.
   fn = role = contact = rep("", nrow(Rx))
   for(i in 1:nrow(Rx)) {
      fn[i] = fullName(Rx[i,])
      role[i] = S$mem$role[S$mem$userID == Rx$userID[i]]
      contact[i] = S$mem$contact[S$mem$userID == Rx$userID[i]]
   }
   Rx <- Rx[,-1]                                      # Remove ID column
   Rx[,3] = fn
   Rx[,4] = role
   Rx[,5] = ifelse(contact==1, "Yes", "No")
   Rx = Rx[,-(6:7)]
   omDT(Rx,
      cnames = c("UserName", "Email Adr", "Full Name", "Project Role", "Project Contact", "Send Email"),
      colesc = c(1:5),                                # columns to escape (all except button columns)
      noText = "No users found for this project"      # What to say when there are no results
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
         S$mem <<- membershipGet(c("userID", "role", "contact"), tibble(c("projectID", "=", n)))
         showResultsRV$WHERE = tibble(c("userID", " IN ", paste0("(", paste0(S$mem$userID, collapse=","), ")")))
         showResultsRV$newWHERE = showResultsRV$newWHERE + 1
         rv$limn = rv$limn + 1
      },
      "um1" = { },          # do nothing
      "email" = {
         S$showPage <<- FALSE
         S$emailPage <<- TRUE
         S$ux <<- userGet(c("userID", "userName", "email", "emailOK"), tibble(c("userID", "=", n)))
         rv$limn = rv$limn + 1
      },
      "sendEmail" = {
         if(emailCheck()) {
            S$emailName <<- S$ux$userName
            S$emailAdr <<- S$ux$email
            S$emailSubject <<- esc(input$emailSubject)
            S$emailText <<- esc(input$emailText)
            S$emailFromName <<- "Open-Meta Admin"
            S$emailFromAdr <<- S$U$email       ### ASSUMES ALL ADMINS HAVE OPEN-META.ORG EMAIL ADDRESSES
            rv$sendEmail = rv$sendEmail + 1    ###    THAT RECIPIENT CAN REPLY TO...
            S$showPage <<- FALSE
            S$emailPage <<- FALSE
            rv$limn = rv$limn + 1
         }
      },
      "cancel" = {
         S$showPage <<- FALSE
         S$emailPage <<- FALSE
         rv$limn = rv$limn + 1
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)
