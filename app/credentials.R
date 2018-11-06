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

### You can work on code development without Amazon Simple Email Service credentials.
#   In that case noEmail will be set to TRUE and sent emails will appear in a modal
#   dialog so you can see the subject and body. No email will actually be sent.
#   However, in some places (e.g. trying to contact a  project's leaders), sending
#   email is immediately followed by a redirect to a different page. The redirect
#   erases the modal dialog before you can see it.

### To send email with the Amazon Simple Email Service api.
#   To obtain the needed credentials, log into the AWS console. In "AWS services" choose
#   Identity and Access Management (IAM). In the vertical menu at the left, pick "Users".
#   In the screen that appears, pick "Add User". Give this user a name like "SESapi" and
#   for Access Type pick "Programmatic access". Click "Next: Permissions" and on that page
#   pick the box for "Attach Existing Policies Directly". In "Filter policies" enter "SES".
#   This will show you two permission policies for SES, one for Full Access and one for
#   read-only access. Since you have to POST (ie, write) to send email, check the box on
#   the Full Access line and click Next: Review. On the review page, click Create User.
#   You now get your one and only chance to download the key and secret you need here as
#   a .csv file.

SESkey <- ""
SESsecret <- ""
SESregion <- ""            # The region of your SES server (us-east-1, us-west-2, or eu-west-1)
SESfromName <- ""
SESfromAdr <- ""           # Default "From" name and address. Address must be AWS verified.
SESdelay <- 75             # ~ 14 emails per second max

if(SESkey=="") {
   noEmail = TRUE          # Email appears in a modal dialog
} else {
   noEmail = FALSE         # Email is sent using AWS-SES api
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
