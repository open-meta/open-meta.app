### om_skeleton profile.R
### Tom Weishaar - Oct 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

# Need email
source("email-core.R", local=TRUE)

# SQL simplification function used multiple times on this page to get a user's record for updating
getUser2 = function(id) { return(userGet("**", tibble(c("userID", "=", id)))) }

S$fixingemail <- FALSE        # flag to separate fixing email from new regisitation
S$wrongcode <- 0              # counter to prevent code guessing

output$uiHead <- renderUI(tagList(slimHead, bs4("hr")))

output$uiMeat <- renderUI({rv$limn; isolate({
   if(rv$limn) {                             # don't run this unless rv$limn is 1 or more
      if(debugON) {
         cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
      }
      if(S$U$sPowers==0) { # not logged in; return registration inputs
         pageText <- tagList(
            bs4("r", bs4("ca", class="mx-auto", style="width:300px",
               ttextInput("userName", "User Name:", value="", autofocus=TRUE),
               passwordInput("password1", "Password:", value=""),
               passwordInput("password2", "Repeat Password:", value=""),
               ttextInput("email", "Email address (for account verification):", value=""),
               actionButton("register_btn", "Register", class="btn-primary w-100")
               )
            )
         )
      } else {                               # logged in but email unverified, send email and return verification inputs
         if(S$U$sPowers && !S$U$emailOK) {
            pageText <- tagList(
               bs4("r", align="ha",
                  bs4("ca", style="width:300px",
                     HTML(paste0(
                        "<h3>Almost there...</h3><p>We've just sent an email to <b>", S$U$email,
                        "</b> with a 6-digit temporarty PIN. To verify your email address, ",
                        "enter the PIN here and click the OK button.</p>")),
                     ttextInput("tempcode", "PIN from email", value=""),
                     actionButton("ok_btn", "OK", class="btn-primary w-100"),
                     HTML('<div class="panel panel-primary" style="margin:2em 0 0 0">',
                        '<div class="panel-heading"><b><i>Need to revise your email address?</i></b></div>',
                        '<div class="panel-body">'),
                     ttextInput("email", "", value=S$U$email),
                     actionButton("fixemail_btn", "Whoops, fix my email address", class="btn-danger w-100 mb-4"),
                     HTML('</div></div>')
                  ),
                  bs4("ca", style="width:300px",
                      HTML(paste0(
                        "<h3>Your email address</h3>",
                        "<p>We will use your email address for administrative details like helping you recover a lost password.</p>",
                        "<p>You will also receive emails from any projects that you join.</p>",
                        "<p>If you start a project, we will forward email sent to that project to your email address.</p>",
                        "<p>But we don't share email addresses with others outside our system.</p>"
                      ))
                  )
               )
            )                                            # send the email
            S$tempcode <<- generate_code()
            if(S$fixingemail) {                          # if email is already verified, user is changing email address
               S$emailSubject <<- paste0("Code to verify your ", site_name," account.")
               S$emailText <<- paste0("Here's the code you must enter to change your ", site_name," email address: ", S$tempcode)
            } else {                                     #    otherwise user is verifing email address for a new account
               S$emailSubject <<- paste0("Code to verify your new ", site_name," account.")
               S$emailText <<- paste0("Here's the code you must enter to complete your ", site_name," registration: ", S$tempcode)
            }
            S$emailName <<- S$U$userName
            S$emailAdr  <<- S$U$email
            S$emailReplytoName <<- "Verification Robot"
            S$emailReplytoAdr <<- "email-verification@open-meta.org"
            rv$sendEmail <- rv$sendEmail +1
         } else {                                        # logged in and email verified; show profile update inputs
            if(S$U$sPowers) {
               S$fixingemail <<- TRUE
               user <- userGet(c("namePrefix", "nameFirst", "nameMiddle", "nameLast", "nameSuffix"), tibble(c("userID", "=", S$U$userID)))
               pageText <- tagList(
                  bs4("r", align="hc",
                     bs4("ca",
                        HTML("<h4 style='margin-bottom: 1em;'>Your user name:", S$U$userName,"</h4>")
                     )
                  ),
                  bs4("r", align="ha",
                     bs4("ca", style="width:300px",
                        ttextInput("namePrefix", "Your name - prefix:", value=user$namePrefix),
                        ttextInput("nameFirst", "Your name - first:", value=user$nameFirst),
                        ttextInput("nameMiddle", "Your name - middle:", value=user$nameMiddle),
                        ttextInput("nameLast", "Your name - last/family:", value=user$nameLast),
                        ttextInput("nameSuffix", "Your name - suffix:", value=user$nameSuffix),
                        actionButton("fixname_btn", "Update name", class="btn-primary w-100 mb-4")
                     ),
                     bs4("ca", style="width:300px",
                        ttextInput("email", "Update email address:", value=S$U$email),
                        actionButton("fixemail_btn", "Update email address", class="btn-primary w-100"),
                        bs4("hr"),
                        passwordInput("password", "Confirm Old Password:", value="", width="100%"),
                        passwordInput("password1", "New Password:", value="", width="100%"),
                        passwordInput("password2", "Repeat New Password:", value="", width="100%"),
                        actionButton("chgPassword_btn", "Change Password", class="btn-primary w-100")
                     )
                  ))
            }
         }
      }
      return(pageText)     # This is an "explicit return". Remeber that renderUI() is an R function!
   }                       #    Shiny expects renderUI to return some text, which may have embedded
})})                       #    HTML. Although Shiny examples rarely use it, if you include an
                           #    explicit return, your code looks more R-like and it helps to keep
                           #    straight what part of your renderUI is actual code and what part
                           #    is building and returning the HTML. Nested tagLists() are ok.
                           #    Other pages here embed the returns in the code rather than using a
                           #    variable that is returned at the end of the code. Either way is ok.

# This observer is for initial registration
observeEvent(input$register_btn, {
   userName <- str_replace_all(input$userName, "[^[:alnum:]]", "")   # make userName alpahnumeric...
   userName <- str_replace_all(userName, " ", "")                    #    ...with no spaces
#   updateTextInput(session, "userName", value=userName)
   alertText <- ""

   if(userName=="") {
      alertText <- paste0(alertText, "<p>Please provide a user name.</p>")
   }
   if(nchar(userName)!=nchar(input$userName)) {
      alertText <- paste0(alertText, "<p>User name can't have punctuation or spaces.</p>")
   }
   if(nchar(userName) > 30) {
      alertText <- paste0(alertText, "<p>User name is limited to 30 characters.</p>")
   }
   if(userGet("userID", tibble(c("userName", "=", userName)))$userID > 0) { # check againt all existing userNames
      alertText <- paste0(alertText, "<p>", userName, " is taken.</p>")
   }
   if(input$password1 == "") {
      alertText <- paste0(alertText, "<p>Password can't be blank.</p>")
   }
   if(nchar(input$password1) > 30) {
      alertText <- paste0(alertText, "<p>Password is limited to 30 characters.</p>")
   }
   if(input$password1 != input$password2) {
      alertText <- paste0(alertText, "<p>Passwords don't match.</p>")
   }
   if(input$email=="") {
      alertText <- paste0(alertText, "<p>Please provide an email address.</p>")
   } else {
      if(!grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", input$email, ignore.case=TRUE)) {  # from r-bloggers.com
         alertText <- paste0(alertText, "<p>Please provide a valid email address.</p>")
      }
   }
   if(nchar(input$email) > 254) {
      alertText <- paste0(alertText, "<p>An email address is limited to 254 characters.</p>")
   }
   if(alertText=="") {                                 # Register and login with emailOK <- FALSE
      cookie <- generate_id()                          # create a new session id
      js$setCookie("sessionID", cookie, days=0)        # set cookie
      user2 = userGet()                                # get a blank user table
      user2$userName[2] <- userName
      user2$hashedPW[2] <- hashpw(input$password1)
      user2$email[2] <- esc(input$email)
      user2$sessionID[2] <- cookie
      user2$regDate[2] <- sTime()
      user2$loginDate[2] <- sTime()
      user2$sPowers[2] <- 1                            # new registrants have a superpower of 1
      user2$emailOK[2] <- FALSE                        #   and emailOK <- FALSE
      user2 <- recSave(user2)                          # Save and reload to get SQL-assigned userID
      S$U <<- user1SU(tibble(c("userID", "=", user2$userID[2]))) # Refresh S$U
      rv$limn <- rv$limn + 1                           # re-render to show email verification text
   } else {
      S$modal_title <<- "Whoops!"
      S$modal_text <<- alertText
      rv$modal_warning <- rv$modal_warning + 1
#     rv$limn <- rv$limn + 1   # A re-render initializes fields to their original values; no render leaves user-entered text
   }
})

# This observer allows registered users to change their email address and also
#    takes care of the fix email address button on the "enter emailed code" page
observeEvent(input$fixemail_btn, {
   alertText <- ""
   if(input$email=="") {                               # error checking on email address user entered
      alertText <- paste0(alertText, "<p>Please provide an email address.</p>")
   } else {
      if(!grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", input$email, ignore.case=TRUE)) {  # from r-bloggers.com
         alertText <- paste0(alertText, "<p>Please provide a valid email address.</p>")
      }
   }
   if(alertText=="") {                                 # No error
      user2 <- getUser2(S$U$userID)                    # Get full record
      user2$email[2] <- esc(input$email)               # Get new email address
      user2$emailOK[2] <- FALSE                        # This will take us to verification on render
      user2 <- recSave(user2)                          # Save what we have so far
      S$U <<- user1SU(tibble(c("userID", "=", S$U$userID))) # Refresh S$U
      rv$limn <- rv$limn + 1                           # This will send an email with a new code.
   } else {
      S$modal_title <<- "Whoops!"
      S$modal_text <<- alertText
      rv$modal_warning <- rv$modal_warning + 1
   }
})

# This observer is for the OK button after the user enters the code from the email
observeEvent(input$ok_btn, {
   if(str_trim(input$tempcode) == S$tempcode) {        # If the codes match, save user and login
      user2 <- getUser2(S$U$userID)                    # Get full record
      user2$emailOK[2] <- TRUE                         # set emailOK
      user2$evDate[2] <- sTime()                       # set emailverified date
      user2 <- recSave(user2)                          #    and save the record
      S$U <<- user1SU(tibble(c("userID", "=", S$U$userID))) # Refresh S$U
      if(S$modal_title=="") {
         S$modal_title <<- "Success"
         S$modal_text <<- "<p>Your email address has been verified.</p>"
         rv$modal_warning <- rv$modal_warning + 1
      }
      rv$limn <- rv$limn + 1                           # reload page
   } else {
      S$wrongcode <<- S$wrongcode + 1
      if(S$wrongcode<4) {                              # You only get 3 tries to enter the code
         S$modal_title <<- "Whoops!"
         S$modal_text <<- "<p>Wrong code. Recheck that email and try again.</p>"
         rv$modal_warning <- rv$modal_warning + 1
      } else {
         S$modal_title <<- "Whoops!"
         S$modal_text <<- "<p>Sorry, that's too many bad tries. Let's start over. Look for a newer email.</p>"
         rv$modal_warning <- rv$modal_warning + 1
         S$wrongcode <<- 0
         rv$limn <- rv$limn + 1
      }
   }
})

# This observer allows registered users to change their password
observeEvent(input$chgPassword_btn, {
   S$modal_title <<- "Whoops!"
   alertText <- ""
   if(!checkpw(input$password, S$U$hashedPW[2])) {          # right password?
      alertText <- paste0(alertText, "<p>That's not your current password.</p>")
   }
   if(input$password1 == "") {
      alertText <- paste0(alertText, "<p>Your new password can't be blank.</p>")
   }
   if(nchar(input$password1) > 30) {
      alertText <- paste0(alertText, "<p>Passwords are limited to 30 characters.</p>")
   }
   if(input$password1 != input$password2) {
      alertText <- paste0(alertText, "<p>Your new passwords don't match.</p>")
   }
   if(nchar(alertText)==0) {                                # Success!
      user2 <- getUser2(S$U$userID)                         # Get full record
      user2$hashedPW[2] <- hashpw(input$password1)          # Change password
      user2 <- recSave(user2)                               # Save it
      S$U <<- user1SU(tibble(c("userID", "=", S$U$userID))) # Refresh S$U
      js$redirect("?profile")                               # after successful password change, reload page
   }
   S$modal_text <<- alertText
   rv$modal_warning <- rv$modal_warning + 1
})

# This observer allows registered users to add and edit their names
observeEvent(input$fixname_btn, {
   user2 <- getUser2(S$U$userID)                            # Get full record
   user2$namePrefix[2] <- esc(input$namePrefix)             # Add name fields
   user2$nameFirst[2] <- esc(input$nameFirst)
   user2$nameMiddle[2] <- esc(input$nameMiddle)
   user2$nameLast[2] <- esc(input$nameLast)
   user2$nameSuffix[2] <- esc(input$nameSuffix)
   user2 <- recSave(user2)                                  # Save, no refresh needed as name fields are in S$U
   if(S$modal_title=="") {
      S$modal_title <<- paste0("Thank you, ", fullName(user2))
      S$modal_text <<- "<p>Your name has been added to your profile.</p>"
      rv$modal_warning <- rv$modal_warning + 1
   }
   rv$limn <- rv$limn + 1                              # reload page
}, ignoreNULL = TRUE, ignoreInit = TRUE)

