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

S$IN$flag$showAddInputButton <- TRUE      #
S$IN$view <- "Look and Feel"

if(S$P$Msg=="") {
   output$uiMeat <- renderUI({c(rv$menuActive, rv$subMenu, rv$limn); isolate({
      if(rv$limn && S$P$Msg=="") {
         if(S$P$Modify) {                              # If modification is allowed we're at *Members & Settings*
            if(S$hideMenus) {
               menubars = ""
            } else {
               menubars=tagList(
                  bs4("md", id="sub", n=1:3, active=rv$menuActive, text=c("Project Members", "Customize Data Collection Forms", "Activate without Protocol")),
                  bs4("mp", id="custom", n=1:9, active=rv$subMenu, text=c("Stage 1 Review", "Trials", "Arms", "Groups", "Participants",
                     "Interventions", "Comparisons", "Outcomes", "Time Spans")),
                  bs4("dx", style="height:1.5rem")
               )
            }
            switch(as.character(rv$menuActive),
               "1" = {                                  # For this choice, we don't want the submenu after all...
                  menubars=tagList(
                     bs4("md", id="sub", n=1:3, active=rv$menuActive, text=c("Project Members", "Customize Data Collection Forms", "Activate without Protocol"))
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
                              bs4("c9", tagList(
                                 emailWrite(h4text, sendID="email2member", cancelID="return")
                           )))
                     },
                     "emailAll" = {
                        h4text="Email all project members."
                        restOfPage =
                           bs4("r", align="hc",
                              bs4("c9", tagList(
                                 emailWrite(h4text, sendID="email2All", cancelID="return")
                           )))
                     },
                     "editMember" = {
                        restOfPage =tagList(
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
                           ))),
                           bs4("r", align="hc",
                              bs4("c10", tagList(
                                 bs4("d", class="card bg-warning text-dark mx-auto my-4", bs4("d", class="card-body",
                                    bs4("d", class="card-title", h4(class='text-dark', "Roles & Permissions")),
                                    bs4("d", class="card-text", HTML(
"<p>Permissions work like this:",
"<ul>",
"<li>Non-Member: Anyone can view most pages of your project. Project members you change to Non-Member will still see your project listed on their <i>My Projects</i> page with their role as Non-Member.</li>",
"<li>Observer: Also will receive emails you send to All Members of your project. When users join your project, they automatically get this role to start.</li>",
"<li>Reviewer: Also can review citations to see if they meet your inclusion criteria.</li>",
"<li>Investigator: Also can extract data from studies and enter it.</li>",
"<li>Researcher: Also can enter searches.</li>",
"<li>Principal Investigator, Co-Principal Investigator, and Project Administrator: Also can access your project\'s <i>Members & Settings</i> and <i>Publish</i> pages. (These roles have all project permissions.)</li>",
"</ul></p>",
"<p><b>Members are not automatically notified of role changes you make, however, they can see the role you've assigned them on their <i>My Projects</i> page.</b></p>",
"<p>One to all of your members can be project contacts. A project contact receives a copy of emails sent from the project\'s <i>Contact</i> page, which non-PIs see where you see <i>Members & Settings</i>.</p>")
                                 )))
                           )))                        )
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
                        FORMname <- "PrjForm-Trial"
                     },
                     "3" = {
                        FORMname <- "PrjForm-Arm"
                     },
                     "4" = {
                        FORMname <- "PrjForm-Group"
                     },
                     "5" = {
                        FORMname <- "PrjForm-Participant Grp"
                     },
                     "6" = {
                        FORMname <- "PrjForm-Intervention"
                     },
                     "7" = {
                        FORMname <- "PrjForm-Comparison"
                     },
                     "8" = {
                        FORMname <- "PrjForm-Outcome"
                     },
                     "9" = {
                        FORMname <- "PrjForm-Time Span"
                     }
                  )
                  if(rv$subMenu>1) {
                     S$IN$FORM <<- imGetFORM(FORMname)
                     restOfPage = tagList(
                        output$modifyInputs <- renderUI(imModifyInputs()),
                        output$showInputs   <- renderUI(imShowInputs()),
                        output$yellowbox    <- renderUI(yellowbox(form))
                     )
                  }
               },
               "3" = {
                  menubars=tagList(
                     bs4("md", id="sub", n=1:3, active=rv$menuActive, text=c("Project Members", "Customize Data Collection Forms", "Activate without Protocol"))
                  )
                  prj = projectGet("status", WHERE=tibble(c("projectID", "=", S$PRJ$projectID)))
                  if(prj$status[1] == 1) {
                     restOfPage <- HTML0("<h5>Your project is activated.</h5>")
                  } else {
                     restOfPage <- tagList(
                        bs4("cd", q="y", bs4("cdb", bs4("cdt", HTML0("
Although for any real project you will want to enforce Protocol completion before project activation, for testing
and learning about meta-analysis and the Open-Meta app it often makes sense to activate your project without a
Protocol. If that's the kind of project you want to create, you can activate it now without a Protocol by clicking
the following button.
")))),
                        bs4("btn", id="activate", n=1, class="mr-1 mt-2", q=c("b", "p"), "Activate Project without Protocol")
                     )
                  }
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
                     bs4("c9", tagList(
                        emailWrite(h4text, sendID="email2contacts")
                  )))
               ))
            }
         }
      }
   })})
}

yellowbox <- function(t) {
   return(bs4("cd", q="y", bs4("cdb", bs4("cdt", HTML0(               # The yellow box
"<p>This is where you can add your own custom inputs to the forms that collect information about specific aspects of
your project's research studies. When you arrive here, you will see the standard forms we provide. The customized
inputs you add to the forms can collect any type of information you like. However, the information you collect with custom
inputs is typically used for sub-group analysis and for meta-regression.</p>
<p>Keep in mind that any customized inputs you add on this page can help you make your data <i>less granular</i>
but not more granular. That is, they can help you to collect groups of related items together. For example,
if the outcomes you are studying include both physical measures, like grip strength, and mental measures, like
memory tests, you can add a custom input to <i>Outcomes</i> that allows you to say which outcomes are related
to physical performance and which are related to mental performance. This would allow you to analyze all the
physical performance outcomes, or all the mental performance outcomes, together as a group. If you don't collect
this informtion, you will be limited to analyzing only individual outcomes or all of the outcomes together.</p>
<p>To make your data <i>more granular</i>, you're in the wrong place. Instead, go to your project's <i>Extract/
PICOT setup</i> menu. That's were you can enter any number of different participant groups, interventions,
comparisons, outcomes, and time spans. The relationship between this page and that one is that you will use the
input form you design here to collect data on the specific items you enter there.</p><hr>
<p>This page has two sections. The upper section either has a green button for adding an additional input to a
specific form that collects information about trials, arms, and so on. This upper section exapands both when
adding a new input and editing one you've already created.</p>
<p>The lower section displays either what your inputs currently look like or provides buttons for editing, moving,
or deleting inputs you have added. These buttons will never appear, however, for the standard inputs, which you
can't edit, move, or delete.</p>
<p>When you add or edit an input, you first select what <i>Type of input</i> you want. When you change this setting,
the rest of the form may change. You can select any of these six types of inputs:<ul>
<li>a standard, simple text input for collecting one line of text</li>
<li>a text editor, for collecting as much text as you like</li>
<li>a numeric input, for collecting a single number</li>
<li>a dropdown selector, to capture exactly one of several choices you've specified</li>
<li>radio buttons, which also capture exactly one of several choices</li>
<li>checkboxes, which can collect multiple choices from a group of choices you've specified</li>
</ul><p>Note that when you set the <i>combined</i> width of multiple inputs to 100% or less, the inputs will appear in
one row across the screen if you check the <i>sameline</i> checkbox on all but the first one. If you never check
<i>sameline</i>, or if you check <i>sameline</i> but your inputs are all too wide to allow multiple inputs on the same
line, your inputs will appear in a vertical column under the standard inputs.</p>
")))))
}


### observer for omclick
observeEvent(input$js.omclick, {
   if(A$debugON) {
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
            # emails = character(0)
            # for(n in allAdrs$userName) {       # This will work whether the project has one or multiple email allAdrs
            #    emails[n] =  allAdrs[allAdrs$userName == n, "email"]  # makes a "named" vector, with userNames as names
            # }
            # S$emailName <<- ""                 # Allowed when S$emailAdr is a named vector, as above
            # S$emailAdr <<- emails
            S$emailName <<- allAdrs$userName
            S$emailAdr <<- allAdrs$email
            S$emailSubject <<- escHTML(input$emailSubject)
            S$emailText <<- paste0("To: All members of the Open-Meta.org project: ", S$PRJ$projectName, "\n\n", escHTML(input$emailText))
            S$emailFromName <<- "Open-Meta Email Robot"
            S$emailFromAdr <<- "email.robot@open-meta.org"
            S$emailReplytoName <<- S$U$userName
            S$emailReplytoAdr <<- S$U$email
            rv$sendEmail = rv$sendEmail + 1
            S$view <<- "memberList"
            rv$limn = rv$limn + 1
         }
      },
      "email2member" = {
         if(emailCheck()) {         # send email
            S$emailName <<- S$U1$userName       # Got S$U1 on way to emailWrite; this is the TO address
            S$emailAdr <<- S$U1$email
            S$emailSubject <<- escHTML(input$emailSubject)
            S$emailText <<- paste0("Re: Project: ", S$PRJ$projectName, "\n\n", escHTML(input$emailText))
            S$emailFromName <<- "Open-Meta Email Robot"
            S$emailFromAdr <<- "email.robot@open-meta.org"
            S$emailReplytoName <<- S$U$userName
            S$emailReplytoAdr <<- S$U$email
            rv$sendEmail = rv$sendEmail + 1
            S$view <<- "memberList"
            rv$limn = rv$limn + 1
         }
      },
      "email2contacts" = {
         if(emailCheck()) {         # send email
            # emailFrom = S$U$email   # user must be logged in to get here; we already have that reply-to email address
            contactIDs = membershipGet("userID", tibble(c("projectID", "=", S$PRJ$projectID),c("contact", "=", 1))) # Get contactIDs
            contacts = userGet(c("userName", "email"), tibble(c("userID", " IN ", paste0("(", paste0(contactIDs$userID, collapse=","), ")"))))
            # emails = character(0)
            # for(n in contacts$userName) {       # This will work whether the project has one or multiple email contacts
            #    emails[n] =  contacts[contacts$userName == n, "email"]  # makes a "named" vector, with userNames as names
            # }
            # S$emailName <<- ""                  # Allowed when S$emailAdr is a named vector, as above
            # S$emailAdr <<- emails
            S$emailName <<- contacts$userName
            S$emailAdr <<- contacts$email
            S$emailSubject <<- escHTML(input$emailSubject)
            S$emailText <<- paste0("To: Contacts of Project: ", S$PRJ$projectName, "\n\n", escHTML(input$emailText))
            S$emailFromName <<- "Open-Meta Email Robot"
            S$emailFromAdr <<- "email.robot@open-meta.org"
            S$emailReplytoName <<- S$U$userName
            S$emailReplytoAdr <<- S$U$email
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
      "inputAdd" = {                                                     # This is the big green Add button
         if(S$P$Modify) {
            S$IN$flag$showAddInputButton <<- FALSE
            S$hideMenus <<- TRUE
            S$IN$flag$oldInput <<- FALSE
            S$IN$inputNUM <<- 0
            S$IN$FORMrow <<- imGetBlankFORMrow(S$IN$codeTypes[S$IN$inputType])
            S$IN$FORMrowform <<- imGetBlankform(S$IN$codeTypes[S$IN$inputType])
            S$IN$FORMrowform$order <<- 1:nrow(S$IN$FORMrowform)
            rv$limn = rv$limn + 1
         }
      },
      "inputEdit" = {                                                    # This is the green Edit button
         if(S$P$SA || (S$P$Modify && !S$IN$FORM[[n, "locked"]])) {
            S$IN$flag$showAddInputButton <<- FALSE
            S$hideMenus <<- TRUE
            S$IN$flag$oldInput <<- TRUE                                  # disable selector, among other things
            S$IN$inputNUM <<- as.numeric(n)
            S$IN$FORMrow <<- S$IN$FORM[n,]
            S$IN$FORMrowform <<- imFORMrow2form(S$IN$FORMrow)                    # expand form row into a form
            S$IN$inputType <<- which(S$IN$codeTypes %in% S$IN$FORM[[n,"type"]])  # get type of input for selector
            rv$limn = rv$limn + 1
         }
      },
      "inputSave" = {                                                    # This button is on the output$modifyAnInput screen
         if(S$P$Modify && imInputValidates()) {
            imSaveform2FORMrow()
            S$IN$flag$showAddInputButton <<- TRUE
            S$hideMenus <<- FALSE
            ###  Activate this section only to save *default* FORMs in om$prime; must be Admin account to do this
            #       (FORMs are transferred from om$prime to S$db when new projects are created.)
            if(S$U$userName=="Admin") {
               saveDB <- S$db
               S$db <<- "om$prime"
               imSaveFORM()
               S$db <<- saveDB
            }
            ###
            rv$limn = rv$limn + 1
         }
      },
      "inputCancel" = {                                                  # This button is on the output$modifyAnInput screen
         S$IN$flag$showAddInputButton <<- TRUE
         S$hideMenus <<- FALSE
         rv$limn = rv$limn + 1
      },
      "inputDelete" = {
         if(S$P$Modify && !S$IN$FORM[[n,"locked"]]) {
            dbLink <- poolCheckout(shiny.pool)                              # When deleting an input, we also need to delete
            on.exit(poolReturn(dbLink), add = TRUE)                         #   its id from the ids table
            r = dbGetQuery(dbLink, paste0("DELETE FROM `", S$db, "`.`ids` WHERE idsID='", S$IN$FORM[n, "id"], "';"))
            S$IN$FORM <<- S$IN$FORM[-as.numeric(n),]                        # Delete the n row from FORM
            imSaveFORM()                                                    # Save the FORM
            rv$limn = rv$limn + 1
         }
      },
      "inputUp" = {
         if(S$P$Modify) {
            S$IN$FORM[n,"order"] <<- S$IN$FORM[n,"order"] - 1.5
            imFixOrder()                                                    # imFixOrder also saves and updates S$IN$FORM
            rv$limn = rv$limn + 1
         }
      },
      "inputDown" = {
         if(S$P$Modify) {
            S$IN$FORM[n,"order"] <<- S$IN$FORM[n,"order"] + 1.5
            imFixOrder()                                                    # imFixOrder also saves and updates S$IN$FORM
            rv$limn = rv$limn + 1
         }
      },
      "activate" = {
         prj = projectGet("**", WHERE=tibble(c("projectID", "=", S$PRJ$projectID)))
         prj$status[2] = 1
         recSave(prj)
         js$redirect(paste0("?prjAdmin&prj=", S$PRJ$projectID))             # Rerender page to update top menu
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# load the inputMeta.R code (also used by other pages)
source("inputMeta.R", local=TRUE)

### flags
# S$IN$flag$showAddInputButton
# S$IN$flag$oldInput

### inputMeta.r functions used in this file
# imGetBlankform
# imGetBlankFORMrow
# imFORMrow2form
# imModifyInputs
# imShowInputs
# imInputValidates
# imSaveform2FORMrow
# imFixOrder
# imSaveFORM



