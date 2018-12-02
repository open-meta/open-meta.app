### open-meta.app lostpassword.R
### Tom Weishaar - Oct 2017 - v0.1

source("email-core.R", local=TRUE)

S$userOK <- FALSE                 # flags for where we are in the process
S$codematch <- FALSE

output$uiHead <- renderUI(tagList(slimHead, bs4("hr")))

output$uiMeat <- renderUI({rv$limn; isolate({
   if(rv$limn) {                             # don't run this unless rv$limn is 1 or more
      if(A$debugON) {
         cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
      }

      # step 0 - User is already logged in (can happen via URL): redirect to ?profile
      if(S$U$sPowers && !S$userOK) {
         return(js$redirect("?profile"))
      }

      # step 1 - No email code match and no username: get the userName
      if(!S$codematch && !S$userOK) {
         return(tagList(
            bs4("r", bs4("ca", class="mx-auto", style="width:300px",
               ttextInput("userName", "User Name", value="", autofocus=TRUE),
               actionButton("submitName_btn", "Submit", class="btn-primary w-100")
               )
            )
         ))
      }

      # step 2 - No email code match, but we have username: send email and allow input of code
      if(!S$codematch && S$userOK) {
         S$tempcode <<- generate_code()
         S$emailSubject <<- paste0("Code to verify your ", site_name," account.")
         S$emailText <<- paste0("Here's the code you must enter to reset your ", site_name," password: ", S$tempcode)
         S$emailName <<- S$U2$userName[2]           # No S$U to consult, but we have S$U2
         S$emailAdr  <<- S$U2$email[2]
         S$emailReplytoName <<- "Verification Robot"
         S$emailReplytoAdr <<- "password-verification@open-meta.org"
         rv$sendEmail <- rv$sendEmail +1
         return(tagList(
            bs4("r", bs4("ca", class="mx-auto", style="width:300px",
                  HTML(paste0(
                     "<h3>Almost there...</h3><p>We've just sent an email to <b>", S$U$email,
                     "</b> with a 6-digit temporarty PIN. To update your password, ",
                     "enter the PIN here and click the OK button.</p>")),
                  ttextInput("tempcode", "PIN from email", value=""),
                  actionButton("submitCode_btn", "OK", class="btn-primary w-100")
               )
            )
         ))
      }

      # step 3 - Email code match: allow user to change password
      if(S$codematch) {
         return(tagList(
            bs4("r", bs4("ca", class="mx-auto", style="width:300px",
               HTML("<h5>Enter your new password:</h5>"),
               passwordInput("password1", "New Password:", value="", width="100%"),
               passwordInput("password2", "Repeat New Password:", value="", width="100%"),
               actionButton("submitPW_btn", "Submit", class="btn-primary w-100 mb-1")
               )
            )
         ))
      }
   }
})})

observeEvent(input$submitName_btn, {                 # Handle userName submission
   alertText <- ""
   S$U2 <<- userGet("**", tibble(c("userName", "=", esc(input$userName))))
   if(S$U2$userID[2] == 0) {
      alertText <- paste0(alertText, "<p>Sorry, we don't have a user with that name.")
   } else {
      if(!S$U2$emailOK[2]) {
         alertText <- paste0(alertText, "<p>Sorry, we don't have a verified email address for that name, so we cannot
         send you a code to reset your password. You can abandon this user name and create a new account.</p>")
      }
   }
   if(nchar(alertText)>0) {
      S$modal_title <<- "Whoops!"
      S$modal_text <<- alertText
      rv$modal_warning <- rv$modal_warning + 1
   } else {
      S$userOK <<- TRUE                              # We have a user with a verified email; go get the code
      rv$limn <- rv$limn + 1
   }
}, ignoreNULL = TRUE, ignoreInit = TRUE)

observeEvent(input$submitCode_btn, {                 # Did user get our email?
   wrongcode <- 0                                    # counter to limit code guessing
   if(str_trim(input$tempcode) != S$tempcode) {                # handle wrong code
      wrongcode <- wrongcode + 1
      if(wrongcode<4) {                              # You only get 3 tries to enter the code
         S$modal_title <<- "Whoops!"
         S$modal_text <<- "<p>Wrong code. Recheck that email and try again.</p>"
         rv$modal_warning <- rv$modal_warning + 1
      } else {
         S$modal_title <<- "Whoops!"
         S$modal_text <<- "<p>Sorry, that's too many bad tries. Let's start over.</p>"
         rv$modal_warning <- rv$modal_warning + 1
         js$redirect("?prjActive")
      }
   } else {
      S$codematch <<- TRUE                           # go to step 3, getting the new password
      rv$limn <- rv$limn + 1
   }
}, ignoreNULL = TRUE, ignoreInit = TRUE)

observeEvent(input$submitPW_btn, {                   # make sure the password is ok
   alertText <- ""
   if(input$password1 == "") {
      alertText <- paste0(alertText, "<p>Password can't be blank.</p>")
   }
   if(nchar(input$password1) > 30) {
      alertText <- paste0(alertText, "<p>Password is limited to 30 characters.</p>")
   }
   if(input$password1 != input$password2) {
      alertText <- paste0(alertText, "<p>Passwords don't match.</p>")
   }
   if(nchar(alertText)>0) {
      S$modal_title <<- "Whoops!"
      S$modal_text <<- alertText
      rv$modal_warning <- rv$modal_warning + 1
   } else {
      cookie <- generate_id()                        # create a new session id
      js$setCookie("sessionID", cookie, days=0)      # set cookie
      S$U2$hashedPW[2] <<- hashpw(input$password1)   # set password; already have userName and email
      S$U2$sessionID[2] <<- cookie                   # set sessionID in user
      S$U2$loginDate[2] <<- sTime()                  # set login date
      S$U2 <<- recSave(S$U2)                         # and save the record
      if(S$modal_title=="") {
         S$modal_title <<- "All set!"
         S$modal_text <<- "<p>Your password has been changed.</p>"
         rv$modal_warning <- rv$modal_warning + 1
      }
      js$redirect("?prjMy")                                         # Successful password change leads to home
   }
}, ignoreNULL = TRUE, ignoreInit = TRUE)
