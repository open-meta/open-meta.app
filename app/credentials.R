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

# You can work on code development without Amazon Simple Email Service credentials.
#   In that case noEmail will be set to TRUE and sent emails will appear in a modal
#   dialog so you can see the subject and body. No email will actually be sent.
#   However, in some places (e.g. trying to contact a  project's leaders), sending
#   email is immediately followed by a redirect to a different page. The redirect
#   erases the modal dialog before you can see it.

### For sending email with the Amazon Simple Email Service api.
#    For details on how to obtain the needed credentials see:
#    http://www.open-meta.org//
SESkey <- ""
SESsecret <- ""
SESregion <- ""            # The region of your SES server (us-east-1, us-west-2, or eu-west-1)
SESname <- ""              # Default "From" name and address. Must be AWS verified.
SESfrom <- ""

if(SESkey=="") {           # When noEmail is TRUE, in some places (e.g. trying to contact a project's
   noEmail = TRUE          #   leaders), sending email is immediately followed by a redirect to a
} else {                   #   different page. The redirect erases the modal dialog before you can see it.
   noEmail = FALSE
}

### MySQL credentials
#   Order of passwords (and permissions) is root, admin, shiny
#   Fill in all three passwords, but you only need to create the Root account; the app will
#      create the other two with the passwords you give when it initializes the database.
SQL.Passwords <- c("root.pw", "admin.pw", "shiny.pw")

### PubMed credentials
PubMed.Key <- ""           # PubMed search will work even if you don't have this.
PubMed.Delay <- 400        # If you have a valid key, you can set this to 100; it's the minimum
                           #     delay in milliseconds between PubMed (Entrez) API requests.
                           #     Details at https://www.ncbi.nlm.nih.gov/books/NBK25497/
                           #        search that page for "API keys"
