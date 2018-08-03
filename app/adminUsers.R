### open-meta.app adminUsers.R
### Tom Weishaar - Oct 2017 - v0.1

source("email-core.R", local=TRUE)

# Although users without superpowers can't get here via the menus, they CAN get here by typing in the URL.
S$userOK <- S$U$sPowers >= S$PG$spReq

S$editPage <- FALSE               # editPage and emailPage flags determine what gets rendered
S$emailPage <- FALSE
# Note:
# ...$user is the logged in user
# ...$ux is the one user selected for editing or emailing

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
      if(!S$editPage && !S$emailPage) {   # Show results list
         return(tagList(
            bs4("r", align="hc",
               bs4("c9",
                  bs4("d", id="uiFilters"),
                  DTOutput("uiResults")  # Can't bs4 this one...
               )
            )
         ))
      }
      if(S$editPage) {                                   # Edit the user
         dCard <- deletedCard(S$ux)
         cCard <- clashCard(S$ux)
         return(tagList(
            bs4("r", align="hc",
               bs4("c5",
               { if((dCard[1])!="") {
                  dCard
               } else {
                  tagList(cCard,
                     HTML0('userID: ', S$ux$userID[2], '<br/>'),
                     HTML0('userName: ', S$ux$userName[2], '<br/>'),
                     HTML0('name: ', fullName(S$ux), '<br/>'),
                     HTML0('email: ', S$ux$email[2]),
                     bs4("btn", id="email", n=S$ux$userID[2], q=c("xs", "b"), class="ml-2", "Send email"),
                     HTML0('<br/>'),
                        ttextInput(inputId="sPowers", label="Super Powers",
                           groupClass = "figure",      # .figure forces inline-block to prevent collapsing y margins
                           inputClass = "w-25",
                           value=S$ux$sPowers[2]),
                     bs4("hr"),
                     bs4("cbx", id="Flogoff", ck=FALSE, '<h5 class="text-danger">Force Logoff</h5>'),
                     bs4("cbx", id="Fpassword", ck=FALSE, '<h5 class="text-danger">Force Password Change</h5>'),
                     bs4("hr"),
                     HTML0(bTime(S$ux$loginDate[2]), ' - Last login<br/>'),
                     HTML0(bTime(S$ux$verTime[2]), ' - Last edit (',
                           S$ux$verUser[2], ' v.',
                           S$ux$verNum[2], ')<br/>'),
                     HTML0(bTime(S$ux$evDate[2]), ' - Date email verified<br/>'),
                     HTML0(bTime(S$ux$regDate[2]), ' - Date registered<br/> '),

                     HTML('<div class="text-right mt-3">'),
                        bs4("btn", id="Fdelete", n=S$ux$userID[2], q="r", "Delete User"),
                        bs4("btn", id="cancel", n=S$ux$userID[2], q="b", "Cancel"),
                        bs4("btn", id="save", n=S$ux$userID[2], q="b", "Save"),
                     HTML('</div>')
                  )
               }}
            ))
         ))
      }
      if(S$emailPage) {                               # Email the user
         h4text = paste0("Sending email to ", S$ux$userName, " (", S$ux$email, ")")
         return(tagList(
            bs4("r", align="hc",
               bs4("c7",
                  emailWrite(h4text, emailOK=S$ux$emailOK)
            ))
         ))
      }
   } # if permissions check fails, return nothing, there's already a message in output$uiHead...
})})

### Filters
rv$menuActive=1    # start with Active records only
output$uiFilters <- renderUI({
   return(tagList(
            bs4("md", id="um1", n=1:3, active=rv$menuActive, text=c("Active Users", "Deleted Users", "Clashes"))
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
      buttons = list(edit=list(id="edit", label="Edit Details", q="y", class=""),
                     email=list(id="email", label="Send Email", q="i", class=""))
   )
   # if you need to further modify Rx, you can do it here.
   if(nrow(Rx)>0) {
      fn = rep("", nrow(Rx))                         # if there are rows, exchange 5 name part columns
      for(i in 1:nrow(Rx)) {                         #    for one fullname column
         fn[i] = fullName(Rx[i,])
      }
      Rx[,4] = fn
      Rx = Rx[,-(5:8)]
   }
   Rx <- Rx[,-1]                                      # Remove ID column
   omDT(Rx,
      cnames = c("Username", "Email Adr", "Full Name", "Edit", "Email"),     # names for column headers
      colesc = c(1:3),                                # columns to escape (all except button columns)
      noText = "No Users found"                    # What to say when there are no results
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
         S$emailPage <<- FALSE
         S$ux <<- userGet("**", tibble(c("userID", "=", n), c("deleted", ">=", "0")))
         rv$limn = rv$limn + 1
      },
      "email" = {
         S$editPage <<- FALSE
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
         S$ux$deleted[2] <<- 0
         S$ux <<- recSave(S$ux)
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1                # didn't change the WHERE, but did change WHERE's results
         rv$limn = rv$limn + 1
      },
      "clearClash" = {
         S$ux$clash[2] <<- 0
         S$ux <<- recSave(S$ux)
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1                # didn't change the WHERE, but did change WHERE's results
         rv$limn = rv$limn + 1
      },
      "Fdelete" = {
         S$ux$deleted[2] <<- 1
         S$ux <<- recSave(S$ux)  # recSave won't return a deleted record
         S$editPage <<- FALSE                  #   so we have to go back to the list.
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1                # didn't change the WHERE, but did change WHERE's results
         rv$limn = rv$limn + 1
      },
      "save" = {
         if(checkInput("superPower", input$sPowers)) {
            S$ux$sPowers[2] <<- input$sPowers
            if(input$Flogoff) {                                   # don't do these unless Superpower input is ok
               S$ux$sessionID[2] <<- generate_id()
            }
            if(input$Fpassword) {
               S$ux$hashedPW[2] <<- hashpw(generate_id())
            }
            S$ux <<- recSave(S$ux)
            S$emailPage <<- FALSE
            S$editPage <<- FALSE
         }
         rv$limn = rv$limn + 1                              # Need a new id on the save button or it won't work twice
      },
      "cancel" = {
         S$emailPage <<- FALSE
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
         superPower = {
            if(is.na(as.numeric(v[i])) || as.numeric(v[i]) < 0 || as.numeric(v[i]) > 1000) {
               vmsg = HTML0(vmsg, "<li>Super powers must be between 0 and 1000.</li>")
            }
         },
         print(paste0("In checkInput, no code for ", incoming[i]))
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
