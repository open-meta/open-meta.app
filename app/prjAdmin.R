### open-meta.app prjAdmin.R
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

rv$menuActive = 1       # Start out on first choice
rv$subMenu = 1          # In both cases
S$view <- "memberList"  # On menu 1, start with memberList
S$P$roles <- c("Principal Investigator", "Co-Principal Investigator", "Project Administrator", "Researcher",
                "Investigator", "Reviewer", "Observer", "Non-Member")  # Needed later on this page

S$IN$lastInputID <- 0

S$IN$flag$showAddInputButton <- TRUE      #
S$IN$view <- "Look and Feel"

# dbLink <- poolCheckout(shiny.pool)
# r = dbExecute(dbLink, createTable(S$db, "ids"))
# poolReturn(dbLink)
# print(r)

if(S$P$Msg=="") {
   output$uiMeat <- renderUI({c(rv$menuActive, rv$subMenu, rv$limn); isolate({
      if(rv$limn && S$P$Msg=="") {
         if(S$P$Modify) {                              # If modification is allowed we're at *Members & Settings*
            menubars=tagList(
               bs4("md", id="sub", n=1:2, active=rv$menuActive, text=c("Project Members", "Customize Inputs")),
               bs4("mp", id="custom", n=1:5, active=rv$subMenu, text=c("Stage 1 Review", "Outcomes", "Trials", "Arms", "Groups")),
               bs4("dx", style="height:1.5rem")
            )
            switch(as.character(rv$menuActive),
               "1" = {
                  menubars=tagList(
                     bs4("md", id="sub", n=1:2, active=rv$menuActive, text=c("Project Members", "Customize Inputs"))
                  )
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
                  switch(as.character(rv$subMenu),
                     "1" = {
                     S$REV$failBoxes2 <<- recGet(S$db, "settings", "**", tibble(c("name", "=", "failBoxNames")))
                     S$REV$forceFail2 <<- recGet(S$db, "settings", "**", tibble(c("name", "=", "forceFail")))
                     S$REV$forcePass2 <<- recGet(S$db, "settings", "**", tibble(c("name", "=", "forcePass")))
                     failBoxes <- fromJSON(S$REV$failBoxes2$value[2])
                     usedR = rep(FALSE, 20)                             # Assume nothing is currently in use
                     if(length(failBoxes)==0) {                         # this happens when they've all been blanked out
                        failBoxes[1:20] = ""
                     } else {                                           # Is this failBox already in use?
                        for(i in 1:length(failBoxes)) {                 #   If so, make input readonly so it can't be changed
                           z = recGet(S$db, "review", "reviewID", tibble(c("detail", "LIKE", paste0("%", failBoxes[i], "%"))))
                           usedR[i] = z$reviewID[1] != 0                # Zero means not in use so !=zero means in use
                        }                                               # Fill out list with blanks; [1:20] creates the NAs we're replacing
                        failBoxes[1:20] <- ifelse(is.na(failBoxes[1:20]), "", failBoxes[1:20])
                     }
                     FFck = ifelse(S$REV$forceFail2$value[2]=="T", TRUE, FALSE)
                     FPck = ifelse(S$REV$forcePass2$value[2]=="T", TRUE, FALSE)
                     ids= paste0("r", 1:20)
                     restOfPage <- tagList(
                        bs4("r",
                           bs4("c7",
                              HTML0("<h5>Reason for failure checkboxes</h5>"),
                              bs4("d", class="ml-5 my-3", bs4("btn", id="update_1", q="g", "Update All")),
                              bs4("cbx", id="forceFail", ck=FFck, il=TRUE, "Fail Review: Force reviewer to check at least one <i>reason for failure</i> box."),
                              bs4("cbx", id="forcePass", ck=FPck, il=TRUE, "Pass Review: Force reviewer to uncheck all <i>reason for failure</i> boxes."),
                              bs4("d", class="py-2"),
                              HTML0(paste0('<div class="form-group shiny-input-container"><input id="',
                                           ids, '" type="text" class="form-control" value="', failBoxes, '"',
                                           ifelse(usedR,' readonly="readonly"', ''), '/></div>', collapse=""))
                           ),
                           bs4("c5",
                               bs4("cd", q="y", bs4("cdb", bs4("cdt", HTML0(               # The yellow box
"<p>When your reviewers decide whether a citation Passes or Fails to meet the criteria of your project, you can have
them check one or more <i>reason for failure</i> boxes if you want. This page allows you to designate up to 20 of your
own <i>reason for failure</i> choices and to specify whether you want to force your reviewers to use them or not.</p>
<p>The box descriptions can only be changed <i>on reasons that haven't been used yet</i>! If you're just getting started and
you want to completely redo the descriptions, but some can't be changed, do this:<ul>
<li>First, uncheck the <code>Fail Review</code> box to the left.</li>
<li>Next, go the Review tab and click on the Citation List menu. Check the box for <code>Stage 1 Fails</code> and click the
<code>Filter</code> button.</li>
<li>Go through all the fails one-by-one (whether you actually want to do this or not depends on how many citations have already
been reviewed) and uncheck all the boxes on each citation, then click the <code>Fail</code> button. Repeat until all the boxes
have been unchecked.</li>
<li>Come back to this page and you will now be able to edit all the box descriptions. Also recheck the <code>Fail Review</code>
box on the top of the list on the left.</li>
<li>Go back to Review-Citation List. Again check the <code>Stage 1 Fails</code> box and the <code>Filter</code> button. Now
re-review all those citations.</li>
<li>Better yet, adjust this list to your liking before any reviews have been completed!</li>
</ul></p>
<p>However, keep in mind that accurately recording <i>all</i> of the reasons a citation Does Not Meet Project Criteria (DNMPC)
is a LOT of extra work for reviewers. For speed, skip this and allow reviewers to fail a citation as soon as they find
<i>anything</i> that does not meet the project's criteria. Recording the first failure found, rather than all failures,
is useful to document why a particular citation failed project critera, but an aggregation of this data over all citations
doesn't produce anything very meaningful.</p>
<p>Recording all reasons for failure, on the other hand, produces meaningful aggregate data, but it takes forever. Is the
value of this data to your project actually worth all the extra work for your reviewers?</p>
"))))
                                 )
                              )
                           )
                        },
                     "2" = {
                        imGetFORM("outcome")
                        restOfPage = tagList(
                           output$modifyInputs <- renderUI(modifyInputs()),
                           output$showInputs   <- renderUI(showInputs())
                        )
                     },
                     "3" = {
                        imGetFORM("trial")
                        restOfPage = tagList(
                           output$modifyInputs <- renderUI(modifyInputs()),
                           output$showInputs   <- renderUI(showInputs())
                        )
                     },
                     "4" = {
                        imGetFORM("arm")
                        restOfPage = tagList(
                           output$modifyInputs <- renderUI(modifyInputs()),
                           output$showInputs   <- renderUI(showInputs())
                        )
                     },
                     "5" = {
                        imGetFORM("group")
                        restOfPage = tagList(
                           output$modifyInputs <- renderUI(modifyInputs()),
                           output$showInputs   <- renderUI(showInputs())
                        )
                     }
                  )
               }
            )
            return(tagList(
               bs4("r", align="hc",
                  bs4("c10", tagList(
                     menubars,
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

dynamic <- "exampleInputEmail1"
observeEvent(input[[dynamic]], print(input[[dynamic]]))

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
      "custom" = {
         S$IN$flag$showAddInputButton <<- TRUE
         rv$subMenu = n
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
      "update" = {
         failBoxes = c(input$r1,input$r2,input$r3,input$r4,input$r5,input$r6,input$r7,input$r8,input$r9,input$r10,
                       input$r11,input$r12,input$r13,input$r14,input$r15,input$r16,input$r17,input$r18,input$r19,input$r20)
         blanks = which(failBoxes=="")
         if(length(blanks)>0) { failBoxes = failBoxes[-blanks] }           # remove blanks from vector
         dups = duplicated(failBoxes)                                      # dups is a T/F vector
         if(any(dups)) {
            S$modal_title <<- "Duplicated reasons"
            S$modal_text <<- HTML0("<p>Each reason for failure must be unique. Your duplicates are:<ul>",
                                   paste0("<li>", failBoxes[dups], "</li>", collapse=""),
                                   "</ul></p>")
            S$modal_size <<- "m"
            rv$modal_warning <- rv$modal_warning + 1
         } else {                                                          # OK to save
            S$REV$failBoxes2$value[2] <<- toJSON(failBoxes)
            S$REV$failBoxes2          <<- recSave(S$REV$failBoxes2, S$db)
            S$REV$forceFail2$value[2] <<- ifelse(input$forceFail ,"T", "F")
            S$REV$forceFail2          <<- recSave(S$REV$forceFail2, S$db)
            S$REV$forcePass2$value[2] <<- ifelse(input$forcePass ,"T", "F")
            S$REV$forcePass2          <<- recSave(S$REV$forcePass2, S$db)
            rv$menuActive = 1
            rv$limn = rv$limn + 1
         }
      },
      "return" = {
         S$view <<- "memberList"
         rv$limn = rv$limn + 1
      },
      "addInput" = {                                                     # This is the big green Add button
         S$IN$flag$showAddInputButton <<- FALSE
         S$IN$flag$editingForm <<- FALSE
         S$IN$FORMrow <<- imGetBlankFORMrow(S$IN$codeTypes[S$IN$inputType])
         S$IN$FORMrowform <<- imGetBlankform(S$IN$codeTypes[S$IN$inputType])
         S$IN$FORMrowform$order <<- 1:nrow(S$IN$FORMrowform)
         rv$limn = rv$limn + 1
      },
      "editMe" = {                                                       # This is the green Edit button
         S$IN$flag$showAddInputButton <<- FALSE
         S$IN$flag$editingForm <<- TRUE                                  # disable selector, among other things
         S$IN$FORMrow <<- S$IN$FORM[n,]
         S$IN$FORMrowform <<- imFORMrow2form(S$IN$FORMrow)                   # expand form row into a form
         S$IN$inputType <<- which(S$IN$codeTypes %in% S$IN$FORM[n,"type"])  # get type of input for selector
         rv$limn = rv$limn + 1
      },
      "saveInput" = {                                                    # This button is on the output$modifyAnInput screen
         if(formValidates()) {
            imSaveform2FORMrow()
            S$IN$flag$showAddInputButton <<- TRUE
            rv$limn = rv$limn + 1
         }
      },
      "cancelInput" = {                                                  # This button is also on the output$modifyAnInput screen
         S$IN$flag$showAddInputButton <<- TRUE
         rv$limn = rv$limn + 1
      },
      "deleteMe" = {
         dbLink <- poolCheckout(shiny.pool)                              # When deleting an input, we also need to delete
         on.exit(poolReturn(dbLink), add = TRUE)                         #   its id from the ids table
         r = dbExecute(dbLink, paste0("DELETE FROM `", S$db, "`.`ids` WHERE idsID='", S$IN$FORM[n, "id"], "';"))
         S$IN$FORM <<- S$IN$FORM[-as.numeric(n),]                        # Delete the n row from FORM
         imSaveFORM()                                                    # Save the FORM
         rv$limn = rv$limn + 1
      },
      "upMe" = {
         S$IN$FORM[n,"order"] <<- S$IN$FORM[n,"order"] - 1.5
         S$IN$FORM <<- fixIDnOrder(S$IN$FORM)
         imSaveFORM()
         rv$limn = rv$limn + 1
      },
      "downMe" = {
         S$IN$FORM[n,"order"] <<- S$IN$FORM[n,"order"] + 1.5
         S$IN$FORM <<- fixIDnOrder(S$IN$FORM)
         imSaveFORM()
         rv$limn = rv$limn + 1
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)






### open-meta.app input-meta.R
### Tom Weishaar - Sep 2018 - v0.1

# A FORM is a tibble. Each row describes an input.
# A FORM can be customized by project administrators to add inputs for whatever data they want to collect.
# FORM data is stored in the an associated table (output-trial-arm-group) in a num-name-value format.
# FORMs themselves are JSONized and stored in the settings table.
# A FORMrow describes one input in the FORM

# A "Form" is likewise a tibble with a row for each input.
# Each input in a "Form" collects data for one FORMrow.

# A function with "Form" in the name can handle either a FORM or a Form tibble.

### Create customized inputs

# if(S$P$SA) {
#    S$IN$userTypes <- c("Simple text", "Text editor", "Numeric", "Select (dropdown)", "Radio buttons", "Checkboxes", "TF_Chex")
#    S$IN$codeTypes <- c("text", "quill", "number", "select", "radio", "checkbox", "TFchex")
# } else {
   S$IN$userTypes <- c("Simple text", "Text editor", "Numeric", "Select (dropdown)", "Radio buttons", "Checkboxes")
   S$IN$codeTypes <- c("text", "quill", "number", "select", "radio", "checkbox")
# }

# Load the list of Blankform defintions. This is created by the STAND-ALONE-CREATE-Blankforms-list.R file.
imBlankforms <- readRDS(file="Blankforms.RDS")

imGetBlankform <- function(type) {
   return(imBlankforms[[type]])
}

imGetBlankFORMrow = function(type) {
   return(tibble(
      type=type,                # see S$IN$codeTypes
      id="",                    # identifies one of these parameter for Forms; otherwise the input name
      name="",                  #    a short identifier for tables and figures (must be unique)
      label="",
      width="",                 # for width the possible options are 25%-50%-75%-100%
      value="",                 # selected option(s) for select-radio-checkbox; otherwise current value
      placeholder="",
      maxlength="",             # Number of characters accepted
      min="",                   # These three are for the numeric input type
      max="",                   #  ...the value of these numbers will be in strings, however
      step="",
      options="",               # all the options for select-radio-checkbox, separated by ";"
                                # for "form", the possible options are: inline, sameline, disabled, and locked
      inline=TRUE,              # whether radio buttons and checkboxes are inline or in a column
      sameline=FALSE,           # whether the current input should be on the same line as the previous input (if it would fit)
      disabled=FALSE,           # whether this input should be disabled
      locked=FALSE,             # whether this FORMrow should be locked
      order=999                 # used to reorder the inputs
#      required=,               # requires a true form submit to work, which we don't do
#      readonly=,               # use disabled, which won't accept focus like readonly will
#      size=0,                  # width of input, but form-control class overrides this one
   ))
}

# Some initializations
S$IN$inputType <- 1
S$IN$FORMrowform <- imGetBlankform(S$IN$codeTypes[S$IN$inputType])

# Load a form with values from a FORMrow.
#    a.) get a blank form corresponding the FORMrow's input type
#    b.) loop through the form's inputs, collecting values from the FORMrow's relevant parameters.
#    c.) return the form
imFORMrow2form <- function(FORMrow) {
   form <- imGetBlankform(FORMrow$type)          # Get the correct kind of input form
   for(i in 1:nrow(form)) {                      # Loop through the form
      id <- form$id[i]
      if(id=="other" || id=="direction") {       # These ids need special handling, as they are not columns in FORMrow
         if(id=="direction") {
            v <- ifelse(FORMrow$inline, "across the page", "in a column")
         } else {
            v <- c("sameline"[FORMrow$sameline], "disabled"[FORMrow$disabled], "locked"[FORMrow$locked])
         }
         form[i, "value"] <- paste0(v, collapse=";")
      } else {
         form[i, "value"] <- FORMrow[[form$id[i]]]  # form$id[i] is a column name in FORMrow
      }
#      print(paste0("in imFORMrow2form id ", id, " is ", form[i,"value"]))
   }
   return(form)
}

# Get a FORM from settings file
imGetFORM = function(myTable) {
   S$IN$tableName <<- myTable
   S$IN$settingsName <<- paste0(myTable, "-inputForm")     # save in global since imSaveform2FORMrow() will need this too
   r <-recGet(S$db, "settings", "value", tibble(c("name", "=", S$IN$settingsName)))
   if(r$value!="") {                                       # did recGet return anything?
      S$IN$FORM <<- as.tibble(fromJSON(r$value))           # Yes, and it's a tibble, unJSONize
   } else {
      S$IN$FORM <<- imGetBlankFORMrow("blank")[-1,]        # No, build a zero-row tibble with default input parameters
   }
}

# Make sure the short label is unique and the long label exists
formValidates <- function() {
   msg=""
   if(!S$IN$flag$editingForm) {                            # Only need to validate the name for new inputs
      name <- str_trim(stripHTML(as.character(input[["name"]]))) # trim and strip HTML from input$name
      if(name=="") {
         msg="<li>The short name for this input can't be blank.</li>"
      } else {
         dbLink <- poolCheckout(shiny.pool)                                    # get a dbLink from the pool
         on.exit(poolReturn(dbLink), add = TRUE)                         # return it when done, even if there's an error
         r = dbExecute(dbLink, paste0("SELECT idsID FROM ", S$db, ".ids WHERE idsID='", name, "';"))
         if(r!=0) {
            msg = "<li>Short names must be unique and that name is taken.</li>"
         }
      }
   }
   label <- str_trim(stripHTML(as.character(input[["label"]]))) # trim and strip HTML from input$name
   if(label=="") {
      msg = paste0(msg,"<li>The label for this input can't be blank.</li>")
   }
   if(msg=="") {
      return(TRUE)
   } else {
      S$modal_title <<- "Whoops"
      S$modal_text <<- HTML("<p>Can't save form because:<ul>", msg, "</ul></p>")
      rv$modal_warning <- rv$modal_warning + 1
      return(FALSE)
   }
}


imSaveFORM = function() {
   r <- recGet(S$db, "settings", "**", tibble(c("name", "=", S$IN$settingsName)))
   r$name[2] <- S$IN$settingsName                          # Set up in imGetForm; don't change it elsewhere!
   if(nrow(S$IN$FORM)>0) {
      S$IN$FORM$order <<- 1:nrow(S$IN$FORM)
      r$value[2] <- toJSON(S$IN$FORM)                         # FORM is a tibble, so we have to JSONize it
   } else {
      r$value[2] <- ""                                        # JSON won't save colnames if there are no rows
   }
   r <- recSave(r, S$db)
}

# Put a form's values in a pre-existing FORMrow, put that in a FORM, and save the FORM to the settings file (see below for saving FORM DATA!)
# In general, the row ids of a form match a few of the column names in a FORMrow
#   However, form id "direction" collects data that needs to be converted and stored as T-F in FORMrow column
#      "inline" while form id "other" collects data that needs to be converted and stored as T-F in FORMrow
#      columns "sameline", "disabled", and "locked". Note that we're pulling the form's values out of input[[id]].
imSaveform2FORMrow <- function() {
   FORMrow <- S$IN$FORMrow                    # Get edited row (pre-exisiting or blank; set up by addInput or editMe)
   formtype <- S$IN$codeTypes[S$IN$inputType] # Note that if we are editing a FORMrow, the inputType can't be changed,
   FORMrow$type <- formtype                   #   but if it's a new form it can, so we can't depend on FORMrow's $type
   if(!S$IN$flag$editingForm) {               # Not editing a Form (it's a new one)
      dbLink <- poolCheckout(shiny.pool)                         # get a dbLink from the pool
      on.exit(poolReturn(dbLink), add = TRUE)                    # return it when done, even if there's an error
      name <- str_trim(stripHTML(as.character(input[["name"]]))) # trim and strip HTML from input$name
      id <- paste0("id", paste0(utf8ToInt(name), collapse=""))   # create valid id syntax for both R and JavaScript
      FORMrow[1, "id"] <- id                                     # put this id in FORMrow
      r = dbExecute(dbLink, paste0("INSERT INTO `", S$db, "`.`ids` VALUES ('",
                                   id, "', '", S$IN$tableName, "', '", name, "');"))
      print(paste0("after INSERT INTO ids table, r=", r))
      ## if(r==???)  # However, formValidates() checked for uniqueness just milliseconds ago
   }
   form <- imGetBlankform(formtype)           # Get the form for this type of FORMrow; we need its ids
   for(id in form$id) {                       # Loop through the form ids; some, but not all, are columns in FORMrow
      if(is.null(input[[id]])) {              # If input[[id]] is NULL, it can only be checkboxes where nothing is checked
         rawI <- ""
      } else {
         rawI <- str_trim(stripHTML(as.character(input[[id]]))) # trim and strip HTML from input$
      }
      if(id=="options") {                     # trim trailing ";" from select-radio-checkbox options if there is one
         rawI <- ifelse(str_sub(rawI,-1,-1)==";", str_sub(rawI,1,-2), rawI)
      }
      if(id=="direction" || id=="other") {    # These ids need special handling, as they are not columns in FORMrow
         if(id=="direction") {
            FORMrow[1, "inline"] <- "across the page" %in% rawI
         } else {
            FORMrow[1, "sameline"] <- "sameline" %in% rawI       # Also, in these cases rawI could be
            FORMrow[1, "disabled"] <- "disabled" %in% rawI       #   a vector of checked box names
            FORMrow[1, "locked"] <- "locked" %in% rawI
         }
      } else {
         FORMrow[1,id] <- ifelse(is.na(rawI), "", rawI)      # NA happens when min, max, step, or maxlength are empty
      }
   }
   if(nrow(S$IN$FORM)==0) {                                  # Create a new tibble
      S$IN$FORM <<- FORMrow
   } else {
      if(FORMrow$order==999) {                               # Add an additional row to an existing tibble
         S$IN$FORM <<- rbind(S$IN$FORM, FORMrow)             # $order=999 means it's a new FORMrow
      } else {
         S$IN$FORM[S$IN$FORM$order==FORMrow$order,] <<- FORMrow   # Update an existing row in an exisiing tibble
      }
   }
   imSaveFORM()
}

imSaveFORMData <- function() {

   ### Missing code here

}

# Convert tibblized input description to HTML
imFormRow2HTML = function(tr) {
# Initialize variables that will be pasted together or are otherwise needed
   options = tr$options
   value = tr$value
   if(any(c("select", "radio", "checkbox") %in% tr$type)) {
      options = str_trim(unlist(str_split(options, ";")))       # vectorize and trim
   }
   if(tr$type=="checkbox") {
      value = str_trim(unlist(str_split(value, ";")))           # vectorize and trim
      if(S$P$SA && str_sub(tr$id,1,2)!="id") {                  # if id doesn't start with "id" its a form, not a FORM
         options = c(options, "locked")                         # System Admin also gets the locked option
         if(tr$locked){
            value = c(value, "locked")
         }
      }
      print(options)
      print(value)
   }
   label = tr$label
   labelfor = paste0(' for="', tr$id, '"')
   type = paste0(' type="', tr$type, '"')
   id = paste0(' id="', tr$id, '"')
   class = switch(tr$width,
      "25%" = "w-25",
      "50%" = "w-50",
      "75%" = "w-75",
      "100%" = "w-100",
      ""
   )
   class = ifelse(tr$inline, paste0(class, " inline"), class)
   valueP = ifelse(nchar(value[1])>0, paste0(' value="', value, '"'), '')
   placeholder = ifelse(nchar(tr$placeholder)>0, paste0(' placeholder="', tr$placeholder, '"'), '')
   min = ifelse(tr$min=="", "", paste0(" min=", tr$min))
   max = ifelse(tr$max=="", "", paste0(" max=", tr$max))
   step = ifelse(tr$step=="", "", paste0(" step=", tr$step))
   maxlength = ifelse(tr$maxlength=="", "", paste0(" maxlength=", tr$maxlength))
   disabled = ifelse(tr$disabled, " disabled", "")  # Same as readonly but doesn't allow a click to select
   if(tr$id=="name" && S$IN$flag$editingForm) {disabled = " disabled"}     # can't edit "name" if editing
   r=""
# print("In imFormRow2HTML options and value are...")
# print(options)
# print(value)
### text-password-number
   if(any(c("text", "password", "number") %in% tr$type)) {
      class = paste0(' class="form-control ', class, '"')
r <- HTML0(
'<label class="mb-2"', labelfor, '>', label, '</label><br>',
'<input', type, id, class, valueP, placeholder, min, max, step, maxlength, disabled, '>',
'<div class="clearfix"></div>'[!tr$sameline])
   }
### checkbox/radio
   if(any(c("checkbox", "radio") %in% tr$type)) {
      items = ""
      if(options[1]!="") {             # if there are no options at all, the options vector will have a blank first option
         for(i in 1:length(options)) {
            items = paste0(items, '
            <div class="', tr$type, ' inline'[tr$inline], '">
               <label>
                  <input type="', tr$type, '" name="', tr$id, '" value="', options[i], '"', ' checked="checked"'[options[i] %in% value], '/>
                  <span>', options[i], '</span>
               </label>
            </div>')
         }
r <- HTML0('
<div id="', tr$id, '" class="form-group shiny-input-', tr$type, 'group shiny-input-container', '-inline form-check-inline'[tr$inline],'">
   <label class="mb-2" for="', tr$id, '">', label, '</label>
   <div class="shiny-options-group">',
items, '
   </div>
</div>',
'<div class="clearfix"></div>'[!tr$sameline])
      }
   }
### select
   if("select" %in% tr$type) {
      selected = ifelse(options %in% value, "selected", "")
      optlist=""
      for(i in 1:length(options)) {
         optlist <- paste0(optlist, '<option value="', options[i], '" ', selected[i],'>', options[i], '</option>')
      }
r <- HTML0('<label class="mb-2" for="', tr$id, '">', label, '</label><select id="', tr$id, '" class="form-control ', class ,'"', disabled, '>', optlist, '</select>')
   }
### quill
   if("quill" %in% tr$type) {
      r <- as.character(bs4("quill", id=tr$id, label=label, value=value, placeholder=tr$placeholder))   # as.character to convert from tagList()
   }
   return(r)
}
### TFchex
# never got this to work right and abandoned it
   # if("TFchex" %in% tr$type) {
# print(tr$id)
# print(value)
# print(tr$inline)
# print(tr$disabled)
# print(options)
# print(label)
   #    x <- as.character(bs4("cbx", id=tr$id, ck=value, inline=tr$inline, dis=tr$disabled, options))
   #    r <- HTML0('<label class="mb-2" for="', tr$id, '">', label, '</label><br>', x)
   #
   # }

imForm2HTML <- function(Form) {
   r = "<form>"
   if(nrow(Form)>0) {
      for(i in 1:nrow(Form)) {
         thisr = imFormRow2HTML(Form[i,])
         r = paste0(r, thisr)
      }
   }
   return(HTML0(r, "</form>"))
}

inputID <- function() {
   S$IN$lastInputID <<- S$IN$lastInputID + 1
   return(paste0("id", as.character(S$IN$lastInputID)))
}

# This is called by a render function in modifyInputs() to display the form for creating/editing a FORMrow
modifyAnInput <- function() {
   r = ""
   if(S$IN$flag$editingForm || is.null(input$inputType)) {
      r = imForm2HTML(S$IN$FORMrowform)   # S$IN$FORMrowform is set up by click observer for "addInput" and "editMe"
   } else {
      S$IN$inputType <<- which(S$IN$userTypes %in% input$inputType)         # Get current inputType selector setting
      # Save current values and insert in form?
      r = imForm2HTML(imGetBlankform(S$IN$codeTypes[S$IN$inputType]))       # Display a form of that type
   }
   return(HTML(r))
}

# This handles the top of the page, where inputs are added and edited
modifyInputs <- function() {
   if(S$IN$flag$showAddInputButton) {                     # Show button or inputs?
      return(tagList(
         bs4("btn", id="addInput", q="g", "Add customized input"),
         bs4("hr")
      ))
   } else {
      ir = imGetBlankFORMrow("select")
      ir$id = "inputType"
      ir$value = S$IN$userTypes[S$IN$inputType]
      ir$options = paste0(S$IN$userTypes, collapse=";")
      ir$label = "Type of Input"
      ir$width = "25%"
      ir$inline = FALSE
      if(S$IN$flag$editingForm) {                            # Disable the type selector when editing (rather than creating)
         ir$disabled = TRUE                                  #    a FORMrow
      }
      return(tagList(
         imForm2HTML(ir),
         output$modifyAnInput <- renderUI(modifyAnInput()),
         HTML('<div class="text-right mt-3">'),
         bs4("btn", id="cancelInput", n=1, q="b", "Cancel"),
         bs4("btn", id="saveInput", n=1, q="b", "Save"),
         HTML('</div>'),
         bs4("hr")
      ))
   }
}

# This handles the bottom of the page where current inputs are displayed
showInputs <- function() {
#print("Running showInputs")
   S$IN$view <<- ifelse(is.null(input$view), S$IN$view, input$view)
   ir = imGetBlankFORMrow("radio")
   ir$id = "view"
   ir$value = S$IN$view
   ir$options = "Look and Feel; Action Buttons"
   ir$label = "Show"
   if(S$IN$view=="Look and Feel") {
      r <- tagList(
         HTML0("<h5>Current Look</h5>"),
         imForm2HTML(ir),
         imForm2HTML(S$IN$FORM)
      )
   } else {
      m=""
      if(nrow(S$IN$FORM)>0) {                                                    # A new FORM will have no rows
         for(i in 1:nrow(S$IN$FORM)) {
            if(!S$IN$flag$showAddInputButton || (S$IN$FORM$locked && !S$P$SA)) { # if locked and not Sys Admin, don't allow changes
               m = paste0(m, imForm2HTML(S$IN$FORM[i,]))                         #    also no buttons if modifying an input
            } else {
               m = paste0(m, imForm2HTML(S$IN$FORM[i,]),
                  bs4("btn", id="upMe", n=i, q="b", class="mb-2", "Move up"),
                  bs4("btn", id="editMe", n=i, q="g", class="mb-2", "Edit"),
                  bs4("btn", id="downMe", n=i, q="b", class="mb-2", "Move down"),
                  bs4("btn", id="deleteMe", n=i, q="r", class="mb-2", style="float:right;", "Delete")
                  )
            }
         }
      }
      r <- tagList(
         HTML0("<h5>Current Look</h5>"),
         imForm2HTML(ir),
         HTML0(m)
      )
   }
   return(r)
}

fixIDnOrder <- function(tib) {
   tib <- tib[order(tib$order),]
   tib$order <- 1:nrow(tib)
   tib$id <- paste0("id", 1:nrow(tib))
   return(tib)
}



      # switch(id,                                           # Store rawI in Row, accomodating different ids
      #    "class" = {
      #       switch(rawI,                                   # convert width to a valid class name
      #          "25%" = {
      #             Row[1,id] <- "w-25"
      #          },
      #          "50%" = {
      #             Row[1,id] <- "w-50"
      #          },
      #          "75%" = {
      #             Row[1,id] <- "w-75"
      #          },
      #          "100%" = {
      #             Row[1,id] <- "w-100"
      #          }
      #       )
      #    },
      #    "min" = {
      #       Row[1,id] <- ifelse(is.na(rawI), "", as.character(rawI))
      #    },
      #    "max" = {
      #       Row[1,id] <- ifelse(is.na(rawI), "", as.character(rawI))
      #    },
      #    "step" = {
      #       Row[1,id] <- ifelse(is.na(rawI), "", as.character(rawI))
      #    },
      #    "maxlength" = {
      #       Row[1,id] <- ifelse(is.na(rawI), "", as.character(rawI))
      #    },
      #    "inline" = {
      #       if(rawI == "across the page") {
      #          Row[1,id] <- TRUE
      #       }
      #       if(rawI == "in a column") {
      #          Row[1,id] <- FALSE
      #       }
      #    },
      #    "other" = {
      #       for(c in rawI) {
      #          Row[1,c] <- TRUE                           # disabled or eol
      #       }
      #    },
      #    "locked" = {
      #       if(S$P$SA) {                                  # Must be Sys Admin to lock an input
      #          for(c in rawI) {
      #             Row[1,c] <- TRUE                        # locked or... ### HOW TO UNLOCK??
      #          }
      #       }
      #    },
      #    { Row[1,id] <- rawI }                            # all ids where rawI doesn't need to be modified
      # )
