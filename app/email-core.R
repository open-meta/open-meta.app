### open-meta.app email-core.R
### Tom Weishaar - May 2018 - v0.2

# v0.2 - Added "noEmail" flag to put send emails in a modal dialog; this makes it possible to run the app
#    on a development system without needing an email host.

# More info on email setup at:
#   http://www.open-meta.org/technology/how-to-send-email-from-r-with-the-help-of-amazon-ses-and-mailr/

# As of today, Amazon SES requires both FROM and REPLY-TO addresses to be AWS-verified.
# While all addresses on the open-meta.org domain are domain-verified, verifying user addresses would
#    involve the user responding to an email from AWS, which seems a little out of the question.
#    If any address could be a REPLY-TO, we could send emails FROM open-meta.org with the Reply-To
#    set to the user who sent the message. But we can't so we use the "mindless robot" nonsense.
#    See: https://forums.aws.amazon.com/thread.jspa?threadID=59110

# If AWS changes its policy, we could add REPLY-TO addresses to this code, but for now it's just redundant.
#    BTW, all email sent to open-meta.org, including bounces, etc., now goes to openmeta.org@gmail.com

# Initialize
rv$sendEmail <- 0           # buzzer for sending email asynchronously

S$emailName <- S$emailAdr <- S$emailSubject <- S$emailText <- character(0)
S$emailFromName <- S$emailFromAdr <- S$emailReplytoName <- S$emailReplytoAdr <- character(0)

### sendEmail
# This observer uses the mailR package and Smtp variables from credentials
observeEvent(rv$sendEmail, {
   if(rv$sendEmail) {
      if(noEmail) {
         S$modal_title <<- "Email not sent, but text is:"
         S$modal_text <<- HTML("<p>Subject:", S$emailSubject, "</p>",
                               "<p>Body:", S$emailText, "</p>")
         S$modal_size <<- "l"
         rv$modal_warning <- rv$modal_warning + 1
      } else {
         withProgress(min=.3, max=1, message="Sending...", {   # Added progress bar because mailR takes so long
            if(length(S$emailFromName)==0) S$emailFromName <<- SmtpName
            if(length(S$emailFromAdr)==0) S$emailFromAdr <<- SmtpAdr
            # if(length(S$emailReplytoName)==0) S$emailReplytoName <<-
            # if(length(S$emailReplytoAdr)==0) S$emailReplytoAdr <<-
            S$emailSubject = paste0("[", site_name, "] ", S$emailSubject)
            # allow a vector of names/adrs in to.adr
            if(length(names(S$emailName)!=0)) {                # If this isn't empty it's in the single-to format
               names(S$emailAdr) <- S$emailName                # Convert to the multi-to format for FOR
            }
            adr.vec = S$emailAdr
            names = names(adr.vec)
            n = length(adr.vec)
            for(i in 1:n) {                                    # Send multiple TO addresses one at a time
               incProgress(1/n/3)
               to.name = names[i]
               to.adr = adr.vec[i]
               send.mail(
                  from = paste0(S$emailFromName, " <", S$emailFromAdr, ">"),
                  to = paste0(to.name, " <", to.adr, ">"),
   #               replyTo = paste0(S$emailReplytoName, " <", S$emailReplytoAdr, ">"),
                  subject = S$emailSubject,
                  body = S$emailText,
                  smtp = list(host.name = SmtpServer,
                              port = SmtpPort,
                              user.name = SmtpUsername,
                              passwd = SmtpPassword,
                              ssl = TRUE),
                  authenticate = TRUE,
                  send = TRUE)
            }
         })
      }
      # Re-initialize S$email variables...
      S$emailName <- S$emailAdr <- S$emailSubject <- S$emailText <- character(0)
      S$emailFromName <- S$emailFromAdr <- S$emailReplytoName <- S$emailReplytoAdr <- character(0)
   }
})

# A standard way to present the email form, which is used in a number of different places
emailWrite = function (h4text="", emailOK=TRUE, sendID="sendEmail", cancelID="cancel", loggedIn = TRUE) {
   emailFrom <- emailUnverfiedWarning <- ""
   if(!loggedIn) { # not logged in
      emailFrom <- ttextInput(inputId="emailFromAdr", label="Your email address")
   }
   if(!emailOK) {
      emailUnverfiedWarning = "<p><b>NOTE: This user's email address has not been verified.</b></p>"
   }
   return(tagList(
      HTML(emailUnverfiedWarning),
      h4(h4text),
      emailFrom,
      ttextInput(inputId="emailSubject", label="Subject"),
      HTML('<div class="form-group shiny-input-container w-100">',
              '<label for="emailText">Text</label>',
              '<textarea id="emailText" class="form-control edLess" style="height:12em;"></textarea>',
           '</div>'),
      HTML('<div class="text-right">'),
      bs4("btn", id=cancelID, n=1, q="b", "Cancel"),
      bs4("btn", id=sendID, n=1, q="b", "Send"),
      HTML('</div>')
   ))
}

emailCheck = function(loggedIn = TRUE) {
      msg=""
      if(!loggedIn) {     # if not logged in, also check email address given
         if(!grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", input$emailFromAdr, ignore.case=TRUE)) {  # from r-bloggers.com
            msg <- paste0(msg, "<li>Please provide a valid email address.</li>")  # This will also catch a blank field
         }
         if(nchar(input$emailFromAdr) > 254) {
         msg <- paste0(msg, "<li>An email address is limited to 254 characters.</li>")
         }
      }
      if(input$emailSubject == "") {
         msg = paste0(msg, "<li>Subject cannot be blank</li>")
      }
      if(input$emailText == "") {
         msg = paste0(msg, "<li>Message text cannot be blank</li>")
      }
      if(msg!="") {                                                        # error, send alert
         S$modal_title <<- "Whoops"
         S$modal_text <<- HTML("<p>Can't send email because:<ul>", msg, "</ul></p>")
         rv$modal_warning <- rv$modal_warning + 1
      }
      return(msg=="")  # No message = TRUE = proceed with sending email
}

# Sample bs4 code for emailWriter

# output$uiMeat <- renderUI({rv$limn; isolate({
#    h4text="Email me your questions and comments"
#    return(tagList(
#       bs4("r", align="hc",
#          bs4("c7", tagList(
#             bs4("d", class="card bg-warning text-dark mx-auto my-4", bs4("d", class="card-body",
#                bs4("d", class="card-title", h4(class='text-dark', "Help / Contact")),
#                bs4("d", class="card-text", "Hi, I\'m Tom Weishaar, the post-retirement Heatlh Education doctoral student who is building Open-Meta.org. At the moment there are no help files, but I'm happy to answer any questions, eager to receive comments, and delighted with bug reports. You can contact me using the form below.")
#             )),
#             emailWrite(h4text, loggedIn = S$U$sPowers)
#       )))
#    ))
# })})

# Sample button click handler

### observer for omclick
# observeEvent(input$js.omclick, {
#    if(debugON) {
#       cat(paste0("\nClick on ", input$js.omclick, "\n"))
#    }
#    uid = str_split(input$js.omclick, "_")
#    id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
#    n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
#    switch(id,
#       "sendEmail" = {
#          if(emailCheck(S$U$sPowers > 0)) {
#             uA = userGet(c("userName", "email"), tibble(c("userID", "=", 1))) # userID = 1 is Admin
#             S$emailName <<- uA$userName
#             S$emailAdr <<- uA$email
#             S$emailSubject <<- esc(input$emailSubject)
#             S$emailText <<- paste0(esc(input$emailFromAdr), " left a message for you on Open-Meta.org\n\n-----\n\n", esc(input$emailText))
#             S$emailFromName <<- "Open-Meta Email Robot"
#             S$emailFromAdr <<- "email.robot@open-meta.org"
#             rv$sendEmail = rv$sendEmail + 1
#             js$redirect("?prjActive")
#          }
#       },
#       "cancel" = {
#          js$redirect("?prjActive")
#       },
#       message(paste0("In input$js.omclick observer, no handler for ", id, "."))
#    )
# }, ignoreNULL = TRUE, ignoreInit = TRUE)

# At one time I used this to make the progress bar bigger, but it doesn't seem to work anymore.
   # .shiny-notification {
   #  width: 200%;
   #  height: 100px;
   #  margin-left: auto;
   #  margin-right: auto;
   # }
   # .shiny-progress-notification .progress {
   #  margin-top: 20px;
   #  height: 25px;
   #  width: 90%;
   # }

