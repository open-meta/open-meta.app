### Credentials and Initializations
#
#   Make sure you don't put this file on GitHub or
#      allow it to be visible on a web site.

### Put your data here
admin_email_address <- ""     # Email address and password for the initial administrator superuser account
admin_password <- "admin.pw"  #    Username of this account is "Admin".
site_name <- "Open-Meta"      # Deprecated because I don't remember to use it. Still found in email subject lines.

if(admin_password=="admin.pw") {
   stop("You need to add four passwords to credentials.R to get started.")
}

# Fill in all three passwords, but you only need to create the Root account; the app will
#    create the other two with the passwords you give when it initializes the database.
SQL.Passwords <- c("root.pw", "admin.pw", "shiny.pw")

### For sending email with mailR package
#    For details see:
#    http://www.open-meta.org/technology/how-to-send-email-from-r-with-the-help-of-amazon-ses-and-mailr/

Smtp.Username <- ""        # You can still work on code development if these are blank.
Smtp.Password <- ""        # In that case the if() statement below will tell the email code to put all
Smtp.Server <- ""          #   sent emails in a modal dialog so you can see the subject and body
Smtp.Port <- ""            #   but no email will actually be sent.
Smtp.From <- ""

if(Smtp.Username=="") {    # When noEmail is TRUE, in some places (e.g. trying to contact a project's
   noEmail = TRUE          #   leaders), sending email is immediately followed by a redirect to a
} else {                   #   different page. The redirect erases the modal dialog before you can see it.
   noEmail = FALSE
}
