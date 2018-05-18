### open-meta prjAdmin.R
### Tom Weishaar - Apr 2018 - v0.1

# When we arrive here:
#   * S$U is a 1x6 tibble with userID, userName, hashedPW, email, emailOK, and sPowers
#   * S$PRJ is a 1x5 tibble with projectID, projectName, status, and privacy PLUS "userRole" field from membership table
#   * S$PG is a 1x2 tibble with pageName (stripped of ? and .R) and spReq
source("chokidar.R", local=TRUE)
# When we leave here:
#   * S$P has permissions in it
#   * output$uiHead has been rendered with any necessary error messages and "hrm" menus
#   * if(S$P$Msg=="") the user has permission to see the page and you can render the Meat
#   * S$P$Modify  - whether the user has permission to modify things on this page

source("email-core.R", local=TRUE)

rv$menuActive = 1       # Start out on first sub-menu
S$view <- "memberList"  # On menu 1, start with memberList
S$P$roles <- c("Principal Investigator", "Co-Principal Investigator", "Project Administrator", "Researcher",
                "Investigator", "Reviewer", "Observer", "Non-Member")  # Needed later on this page

if(S$P$Msg=="") {
   output$uiMeat <- renderUI({c(rv$menuActive, rv$limn); isolate({
      if(rv$limn && S$P$Msg=="") {
         if(S$P$Modify) {                              # If modification is allowed we're at *Members & Settings*
            switch(as.character(rv$menuActive),
               "1" = {
                  switch(S$view,
                     "memberList" = {
                        restOfPage = tagList(
                           DTOutput("showMembers"),
                           HTML('<div class="text-right">'),
                           bs4("btn", id="emailAll", n=1, q="g", "Email All Members At Once"),
                           HTML('</div>')
                        )
                        output$showMembers  <- renderDT({                     # Get members of this project
                           S$Mlist <<- membershipGet(c("userID", "role", "contact"), tibble(c("projectID", "=", S$PRJ$projectID)))
                           Rx = omRx(                                         # Now get their user info
                              db = "om$prime",
                              table = "user",
                              SELECT = c("userID", "userName", "email",
                                         "namePrefix","nameFirst", "nameMiddle", "nameLast", "nameSuffix"),
                              WHERE = tibble(c("userID", " IN ", paste0("(", paste0(S$Mlist$userID, collapse=","), ")"))),
                              buttons = list(edit=list(id="editMember", label="Edit Role/Contact", q="y", class=""),
                                             email=list(id="emailMember", label="Send Email", q="i", class=""))
                           )
                           # if you need to further modify Rx, you can do it here.
                           fn = role = contact = rep("", nrow(Rx))
                           for(i in 1:nrow(Rx)) {
                              fn[i] = fullName(Rx[i,])
                              role[i] = S$Mlist$role[S$Mlist$userID == Rx$userID[i]]        # for each user, determine role
                              contact[i] = S$Mlist$contact[S$Mlist$userID == Rx$userID[i]]  #   and contact from S$Mlist
                           }
                           Rx <- Rx[,-1]                                      # Remove ID column
                           Rx[,3] = fn                                        # Full name in col 3 (was namePrefix)
                           Rx[,4] = role                                      # Role in col 4      (was nameFirst)
                           Rx[,5] = ifelse(contact==1, "Yes", "No")           # Contact in col 5   (was nameMiddle)
                           Rx = Rx[,-(6:7)]                                   # Delete cols 6 and 7 (other name parts)
                           omDT(Rx,                                           #    ...moving buttons to 6 and 7
                              cnames = c("UserName", "Email Adr", "Full Name", "Project Role", "Project Contact", "Edit Role", "Send Email"),
                              colesc = c(1:5),                                # columns to escape (minus means don't escape)
                              noText = "No users found for this project"      # What to say when there are no results
                           )
                        },
                        # renderDT() parameters go here:
                        server = FALSE
                        )
                     },
                     "emailMember" = {
                        h4text = paste0("Sending email to ", S$U1$userName, " (", S$U1$email, ")")
                        restOfPage =
                           bs4("r", align="hc",
                              bs4("c7", tagList(
                                 emailWrite(h4text, sendID="email2member", cancelID="return")
                           )))
                     },
                     "emailAll" = {
                        h4text="Email all project members."
                        restOfPage =
                           bs4("r", align="hc",
                              bs4("c7", tagList(
                                 emailWrite(h4text, sendID="email2All", cancelID="return")
                           )))
                     },
                     "editMember" = {
                        restOfPage =tagList(
                           bs4("r", align="hc",
                              bs4("c10", tagList(
                                 bs4("d", class="card bg-warning text-dark mx-auto my-4", bs4("d", class="card-body",
                                    bs4("d", class="card-title", h4(class='text-dark', "Roles & Permissions")),
                                    bs4("d", class="card-text", HTML(
"<p>Permissions work like this:",
"<ul>",
"<li>Non-Member: Anyone can view most pages of your project. Project members you change to Non-Member will still see your project listed on their <i>My Projects</i> page with their role as Non-Member.</li>",
"<li>Observer: Also can see the Search Analysis page (hidden to non-members) and will receive emails you send to All Members of your project. When users join your project, they automatically get this role to start.</li>",
"<li>Reviewer: Also can review citations to see if they meet your inclusion criteria.</li>",
"<li>Investigator: Also can extract data from studies and enter it.</li>",
"<li>Researcher: Also can enter searches.</li>",
"<li>Principal Investigator, Co-Principal Investigator, and Project Administrator: Also can access your project\'s <i>Members & Settings</i> and <i>Publish</i> pages. (These roles have all project permissions.)</li>",
"</ul></p>",
"<p><b>Members are not automatically notified of role changes you make, however, they can see the role you've assigned them on their <i>My Projects</i> page.</b></p>",
"<p>One to all of your members can be project contacts. A project contact receives a copy of emails sent from the project\'s <i>Contact</i> page, which non-PIs see where you see <i>Members & Settings</i>.</p>")
                                 )))
                           ))),
                           bs4("r", align="hc",
                              bs4("c6", tagList(
                                 h4("Who: ", S$U1$userName),
                                 selectInput('userRole', 'Change member\'s role: ', choices=S$P$roles, selected=S$M2$role[2], selectize=FALSE),
                                 radioButtons('userContact', "When non-members send email to this project, should this member receive a copy?", choices = c("Yes", "No"),
                                              selected = ifelse(S$M2$contact[2]==1, "Yes", "No"), inline = TRUE, width = "75%"),
                                 HTML('<div class="text-right mr-5">'),
                                 bs4("btn", id="return", n=1, q="b", "Cancel"),
                                 bs4("btn", id="saveMember", n=1, q="b", "Save"),
                                 HTML('</div>')
                           )))
                        )
                     }
                  )       # end of S$view switch
               },
               "2" = {
                  restOfPage = bs4("r", bs4("ca", "More to come...PC"))
               }
            )
            return(tagList(
               bs4("r", align="hc",
                  bs4("c10", tagList(
                     bs4("md", id="sub", n=1:2, active=rv$menuActive, text=c("Project Members", "Project Settings")),
                     restOfPage
                  ))
               )
            ))
         } else {                                       # Otherwise show *Contact* page
            if(S$U$sPowers == 0) { # not logged in
               return(tagList(
                  bs4("r", align="hc",
                     bs4("ca", class="mt-3 text-center",
                        h5("You must be logged in to contact a project team and you must register before you can log in.")
                     )
                  ),
                  bs4("r", align="hc",
                     bs4("ca", class="mt-4 text-center",
                        bs4("btn", id="login", n=1, class="mr-1", q=c("b", "l", "p"), "Login"),
                        bs4("btn", id="register", n=1, class="ml-1", q=c("b", "l", "p"), "Register")
                     )
                  )
               ))
            } else {
               h4text=paste0("Email the project\'s sponsors")
               return(tagList(
                  bs4("r", align="hc",
                     bs4("c7", tagList(
                        emailWrite(h4text, sendID="email2contacts")
                  )))
               ))
            }
         }
      }
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
         rv$menuActive = n
      },
      "editMember" = {
         S$U1 <<- userGet(c("userID", "userName", "email"), tibble(c("userID", "=", n)))
         S$M2 <<- membershipGet(c("**"), tibble(c("userID", "=", S$U1$userID), c("projectID", "=", S$PRJ$projectID)))
         S$view <<- "editMember"
         rv$limn = rv$limn + 1
      },
      "saveMember" = {
         msg=""
         # Need to make sure there is at least 1 Prin Invest and 1 Contact
         #    Make entered changes in a copy of SMlist
         memList = S$Mlist
         memList$role[memList$userID == S$M2$userID[2]] = input$userRole
         memList$contact[memList$userID == S$M2$userID[2]] = ifelse(input$userContact=="No", 0, 1)
         # Make sure there's still a PI-equivalent
         if(!any(memList$role %in% c("Principal Investigator", "Co-Principal Investigator", "Project Administrator"))) {
            msg="<li>At least one member must be a Principal Investigator, Co-Principal Investigator, or Project Administrator</li>"
         }
         # Make sure there's still a contact
         if(!any(memList$contact == 1)) {
            msg="<li>At least one member must be a contact for incoming project emails.</li>"
         }
         # Save changes or send error message
         if(msg=="") {
            S$M2$role[2] <<- input$userRole
            S$M2$contact[2] <<- ifelse(input$userContact=="No", 0, 1)
            r = recSave(S$M2)
            S$view <<- "memberList"
            rv$limn = rv$limn + 1
         } else {
            S$modal_title <<- "Whoops"
            S$modal_text <<- HTML("<p>Can't update this member because:<ul>", msg, "</ul></p>")
            rv$modal_warning <- rv$modal_warning + 1
         }
      },
      "emailAll" = {
         S$view <<- "emailAll"
         rv$limn = rv$limn + 1
      },
      "emailMember" = {
         S$U1 <<- userGet(c("userID", "userName", "email"), tibble(c("userID", "=", n)))
         S$view <<- "emailMember"
         rv$limn = rv$limn + 1
      },
      "email2All" = {
         if(emailCheck()) {         # send email
            allIDs = membershipGet("userID", tibble(c("projectID", "=", S$PRJ$projectID), c("role", "!=", "Non-Member")))    # Get all userIDs of members
            allAdrs = userGet(c("userName", "email"), tibble(c("userID", " IN ", paste0("(", paste0(allIDs$userID, collapse=","), ")"))))
            emails = character(0)
            for(n in allAdrs$userName) {       # This will work whether the project has one or multiple email allAdrs
               emails[n] =  allAdrs[allAdrs$userName == n, "email"]  # makes a "named" vector, with userNames as names
            }
            S$emailName <<- ""                 # Allowed when S$emailAdr is a named vector, as above
            S$emailAdr <<- emails
            S$emailSubject <<- esc(input$emailSubject)
            S$emailText <<- paste0("To: All members of the Open-Meta.org project: ", S$PRJ$projectName, "\n\n",
                                   "From: ", S$U$email, "\n\nReply to that email address, not to me, I\'m just a mindless robot.\n\n-----\n\n", esc(input$emailText))
            S$emailFromName <<- "Open-Meta Email Robot"
            S$emailFromAdr <<- "email.robot@open-meta.org"
            rv$sendEmail = rv$sendEmail + 1
            S$view <<- "memberList"
            rv$limn = rv$limn + 1
         }
      },
      "email2member" = {
         if(emailCheck()) {         # send email
            S$emailName <<- S$U1$userName       # Got S$U1 on way to emailWrite; this is the TO address
            S$emailAdr <<- S$U1$email
            S$emailSubject <<- esc(input$emailSubject)
            S$emailText <<- paste0("Re: Project: ", S$PRJ$projectName, "\n\n",
                                   "From: ", S$U$email, "\n\nReply to that email address, not to me, I\'m just a mindless robot.\n\n-----\n\n", esc(input$emailText))
            S$emailFromName <<- "Open-Meta Email Robot"
            S$emailFromAdr <<- "email.robot@open-meta.org"
            rv$sendEmail = rv$sendEmail + 1
            S$view <<- "memberList"
            rv$limn = rv$limn + 1
         }
      },
      "email2contacts" = {
         if(emailCheck()) {         # send email
            emailFrom = S$U$email   # user must be logged in to get here; we already have that reply-to email address
            contactIDs = membershipGet("userID", tibble(c("projectID", "=", S$PRJ$projectID),c("contact", "=", 1))) # Get contactIDs
            contacts = userGet(c("userName", "email"), tibble(c("userID", " IN ", paste0("(", paste0(contactIDs$userID, collapse=","), ")"))))
            emails = character(0)
            for(n in contacts$userName) {       # This will work whether the project has one or multiple email contacts
               emails[n] =  contacts[contacts$userName == n, "email"]  # makes a "named" vector, with userNames as names
            }
            S$emailName <<- ""                  # Allowed when S$emailAdr is a named vector, as above
            S$emailAdr <<- emails
            S$emailSubject <<- esc(input$emailSubject)
            S$emailText <<- paste0("To: Contacts of Project: ", S$PRJ$projectName, "\n\n",
                                   "From: ", S$U$email, "\n\nReply to that email address, not to me, I\'m just a mindless robot.\n\n-----\n\n", esc(input$emailText))
            S$emailFromName <<- "Open-Meta Email Robot"
            S$emailFromAdr <<- "email.robot@open-meta.org"
            rv$sendEmail = rv$sendEmail + 1
            js$redirect(paste0("?Protocol&prj=", S$PRJ$projectID))
         }
      },
      "login" = { js$redirect("?login") },
      "register" = { js$redirect("?profile") },
      "cancel" = {
         js$redirect(paste0("?Protocol&prj=", S$PRJ$projectID))
      },
      "return" = {
         S$view <<- "memberList"
         rv$limn = rv$limn + 1
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

