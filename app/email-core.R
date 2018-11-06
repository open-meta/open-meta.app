### open-meta.app email-core.R
### Tom Weishaar - May 2018 - v0.2

# v0.2 - Added "noEmail" flag to put send emails in a modal dialog; this makes it possible to run the app
#    on a development system without needing an email host.

# v0.3 - Nov 2018 - Email now uses the AWS SES api rather than SMTP & mailR.

# Initialize
rv$sendEmail <- 0           # buzzer for sending email asynchronously

S$emailName <- S$emailAdr <- S$emailSubject <- S$emailText <- character(0)
S$emailFromName <- S$emailFromAdr <- S$emailReplytoName <- S$emailReplytoAdr <- character(0)

### sendEmail
observeEvent(rv$sendEmail, {
   if(rv$sendEmail) {
      if(noEmail) {
         S$modal_title <<- "Email not sent, but text is:"
         S$modal_text <<- HTML("<p>Subject:", S$emailSubject, "</p>",
                               "<p>Body:", S$emailText, "</p>")
         S$modal_size <<- "l"
         rv$modal_warning <- rv$modal_warning + 1
      } else {
         pauseFor <- SESdelay - (seconds(now()-AppGlobal$SES_lastTime)*1000)
         if(pauseFor > 0) {                                  # Pausing so other code gets a shot at the processor
            return(invalidateLater(max(c(75,pauseFor))))     #    and to honor SES 14 emails per second limit
         }
         AppGlobal$SES_lastTime <<- now()                    # Note end time of search execution
         if(length(S$emailFromName)==0) S$emailFromName <<- SESfromName
         if(length(S$emailFromAdr)==0) S$emailFromAdr <<- SESfromAdr
         S$emailSubject = paste0("[", site_name, "] ", S$emailSubject)

         # SES emails have a limit of 50 addresses; if more, we'll re-run observer with remainder
         to.name <- S$emailName[1:50]                        # Grab the first 50 for now
         to.name <- to.name[!is.na(to.name)]                 # Delete any extras, which are NA
         S$emailName <<- S$emailName[-50:-1]                 # Delete this group from global
         to.adr <- S$emailAdr[1:50]                          #   same
         to.adr <- to.adr[!is.na(to.adr)]                    #   same
         S$emailAdr <<- S$emailAdr[-50:-1]                   #   same

         r <- SESemail(
            from = paste0(S$emailFromName, " <", S$emailFromAdr, ">"),
            to = paste0(to.name, " <", to.adr, ">"),         # Note these are the groups of 1 to 50 & a vector is ok
            replyTo = paste0(S$emailReplytoName, " <", S$emailReplytoAdr, ">"),
            subject = S$emailSubject,
            message = S$emailText)
         if(r!="Success: (200) OK") {
            warning(paste0("\nEMAIL SEND FAILURE: ", r, "\n\n"))
         }
         if(length(S$emailName>0)) {
            return(invalidateLater(75))                      # If there are more than 50 addresses, re-run observer
         }
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

# email.aws.R
# code from aws.ses package, modified by TomW Nov 2018

SESemail <- function(message,
                     html,
                     subject,
                     from,
                     to = NULL,
                     cc = NULL,
                     bcc = NULL,
                     replyto = NULL,
                     charset.subject = "UTF-8",
                     charset.message = "UTF-8",
                     charset.html = "UTF-8",
                     key = SESkey,
                     secret = SESsecret,
                     region = SESregion,
                     ...) {

   query <- list(Source = from)

   # configure message body and subject
   query[["Action"]] <- "SendEmail"
   if (missing(message) & missing(html)) {
      stop("Must specify 'message', 'html', or both of them.")
   }
   if (!missing(message)) {
      query[["Message.Body.Text.Data"]] <- message
      if (!is.null(charset.message)) {
          query[["Message.Message.Charset"]] <- charset.message
      }
   }
   if (!missing(html)) {
      query[["Message.Body.Html.Data"]] <- html
      if (!is.null(charset.html)) {
          query[["Message.Body.Html.Charset"]] <- charset.html
      }
   }
   query[["Message.Subject.Data"]] <- subject
   if (!is.null(charset.subject)) {
      query[["Message.Subject.Charset"]] <- charset.subject
   }

   # configure recipients
   if (length(c(to,cc,bcc)) > 50L) {
     stop("The total number of recipients cannot exceed 50.")
   }
   if (!is.null(to)) {
     names(to) <- paste0("Destination.ToAddresses.member.", seq_along(to))
     query <- c(query, to)
   }
   if (!is.null(cc)) {
     names(cc) <- paste0("Destination.CcAddresses.member.", seq_along(cc))
     query <- c(query, cc)
   }
   if (!is.null(bcc)) {
     names(bcc) <- paste0("Destination.BccAddresses.member.", seq_along(bcc))
     query <- c(query, bcc)
   }
   if (!is.null(replyto)) {
     names(replyto) <- paste0("ReplyToAddresses.member.", seq_along(replyto))
     query <- c(query, replyto)
   }

   # result of combining with http.R
   body = query
   query = list()
   headers = list()
   verbose = getOption("verbose", FALSE)

   # generate request signature
   uri <- paste0("https://email.",region,".amazonaws.com")
   d_timestamp <- format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
   body_to_sign <- if (is.null(body)) {
     ""
   } else {
     paste0(names(body), "=", sapply(unname(body), utils::URLencode, reserved = TRUE), collapse = "&")
   }
   Sig <- aws.signature::signature_v4_auth(
        datetime = d_timestamp,
        region = region,
        service = "email",
        verb = "POST",
        action = "/",
        query_args = query,
        canonical_headers = list(host = paste0("email.",region,".amazonaws.com"),
                                 `x-amz-date` = d_timestamp),
        request_body = body_to_sign,
        key = key,
        secret = secret,
        verbose = verbose)

   # setup request headers
   headers[["x-amz-date"]] <- d_timestamp
   headers[["x-amz-content-sha256"]] <- Sig$BodyHash
   headers[["Authorization"]] <- Sig[["SignatureHeader"]]
   H <- do.call(httr::add_headers, headers)
   r <- httr::POST(uri, H, body = body, encode = "form", ...)
   return(httr::http_status(r$status_code)$message)
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

