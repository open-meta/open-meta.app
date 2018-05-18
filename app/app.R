### om_app app.R
### Tom Weishaar - May 2018 - v0.3

### FLAGS

#burnItAllDown = TRUE          # CAREFUL - TRUE deletes the entire Open-Meta database and starts over
burnItAllDown = FALSE

showSysMsg = TRUE              # Controls whether the big red box shows on the main site pages or not
sink(file=stderr())            # When on a server, this makes sure what we print() appears in the log...
debugON <- TRUE                # if TRUE, prints debugging info to the console (or server log)

### CREDENTIALS
# You need to add four passwords to the credentials.R file to get started
#    DO NOT put your credentials file on GitHub!
if(file.exists("credentials.R")) {
   source("credentials.R", local=TRUE)
   if(debugON) { print("Local credentials loaded...") }
} else {
   source("../om2_credentials.R", local=TRUE)       # The cloud app hides the credentials in the app's parent's folder
   if(debugON) { print("Credentials loaded...") }
}

### libraries
library(shiny)
library(htmltools)
library(lubridate)
library(DT)
library(mailR)
library(DBI)
library(pool)
library(RMariaDB)
library(bcrypt)         # 2 commands, hashpw("password") and checkpw("password", hash)
library(jsonlite)
library(tidyverse)      # Need to add these to AWS instance
library(stringi)
library(RefManageR)     # BibTeX library

###  GLOBAL FUNCTIONS
# Time Functions (way up here because sTime() is needed by sql-initialization)
# Use sTime() to put the current time on the server. bTime() is inside server function.
sTime = function(t=now(tz="UTC")) {                    # sTime() = now() as UTC string (aka "server time")
   return(paste(as.character.POSIXt(t), tz(t)))
}
   # escape input to prevent HTML injection
esc = function(s) {
#   s = str_replace_all(s, '"', "&quot;")
#   s = str_replace_all(s, "'", "&#39;")
#   s = str_replace_all(s, "`", "&#96;")
   return(htmlEscape(s))
}

nat = function(x) {          # convert NAs in a logical vector to TRUE
   x = as.logical(x)            # in case vector comes wrapped in a tibble
   return(ifelse(is.na(x), TRUE, x))
}
naf = function(x) {          # convert NAs in a logical vector to FALSE
   x = as.logical(x)            # in case vector comes wrapped in a tibble
   return(ifelse(is.na(x), FALSE, x))
}

### Users table and data persistence
###    See: https://shiny.rstudio.com/articles/persistent-data-storage.html

### sql-core functions
# These functions are fairly automated, but the automation comes with some constraints.
#    To save a record, you only need to call:
#       recSave(rec, (db))
#    recSave() always uses the `table`ID field to determine which table and record to update; zero is a new record.
# The rec must have two rows. All of the Get functions below can return a new or existing record with
#    two idential rows. The first row belongs to recSave() and is assumed to be the data before
#    editing. You should only read from or write to row 2.
# recSave returns the record as it exists on the server after the INSERT/UPDATE unless there was an
#    INSERT/UPDATE error, in which case it returns the record as passed to recSave(). In addition, if there
#    has been an update clash, clash will = 1 and the record will be returned and cannot be resaved until
#    the clash is cleared.
#
# Calling a Get function with no parameters, ie, userGet(), will return a new record with two rows.
# The Get function parameters are SELECT and WHERE.
#    SELECT options:
#       "**" means two rows, all columns
#       "*"  means one row, all columns
#       c("col1","col2") returns only one row and only the requested columns
#    WHERE is a tibble with three rows; a column name, an operator, and a value and as many columns as you need.
#       The column results are ANDed together to return the records you want. By default, you get active
#       records only. You must include a "deleted" column in the tibble to get deleted records.
#       Example: WHERE=tibble(c("userName", "=", "Admin"), c("deleted", ">=", 0))
#       No WHERE returns all active records.
#
# The Get functions are:
#   for tables inside a project:
#     recGet(S$db, table, SELECT, WHERE)
#   for om$prime tables:
#     userGet(SELECT, WHERE) # any of these without parameters returns a new, 2-row record for that table type
#     prjGet(SELECT, WHERE)
#     pageGet(SELECT, WHERE)
#     membershipGet(SELECT, WHERE)
#

   # Return empty 1-row S$U; return real 1-row S$U; return full 2-row SU for an update
emptySU = function() { return( tibble(userID=0, userName="", hashedPW="", email="", emailOK=FALSE, sPowers=0)) }
user1SU = function(WHERE) { return(userGet(c("userID", "userName", "hashedPW", "email", "emailOK", "sPowers"), WHERE)) }
user2SU = function(id) { return(userGet("**", tibble(c("userID", "=", id)))) }

source("sql-core.R", local=TRUE)

if(debugON) { print("Getting SQL.DBs...") }
root.pool <- dbPool(                                  # log in as root to see if there are databases
   drv = RMariaDB::MariaDB(),
   host = "localhost",
   user = "root",
   password = SQL.Passwords[1]
)
SQL.DBs <- dbGetQuery(root.pool, "SHOW DATABASES")
if(burnItAllDown || !("om$prime" %in% SQL.DBs$Database)) {
   if(debugON) { print("Initializing database...") }
   source("sql-initialization.R", local=TRUE)         # This file is loaded only if we need to init databases
   r = DB.Initialization(root.pool, burnItAllDown)
   if(debugON) { print(r) }                           # r is just an initialization completion message
}
poolClose(root.pool)                                  # log out of root

shiny.pool <- dbPool(                                 # log in as shiny
   drv = RMariaDB::MariaDB(),
   dbname = "",
   host = "localhost",
   user = "shiny",
   password = SQL.Passwords[3]
)
zzz = onStop(function() {poolClose(shiny.pool)})
# sql is ready to go...
# shiny.pool exists at the app level, not the session level...
### End of SQL initialization

## This is an alternate for htmltools::HTML that does paste0() instead of paste(), but it can't process tagLists()
HTML0 = function(...) {
   htmlText = c(...)
   if(class(htmlText)!="character") {stop("All parameters in HTML0() must be character strings, no tagLists().")}
   htmlText = paste0(htmlText, collapse="")
   attr(htmlText, "html") <- TRUE
   class(htmlText) <- c("html", "character")
   return(htmlText)
}

`%AND%` = shiny:::`%AND%`   # Needed for ttextInput function

   # Modified textInput allows for autofocus and multiple style and class options
   # Note, autofocus works only on page load, ie, the first render.
ttextInput <- function (inputId, label, value=NULL, groupStyle=NULL, labelStyle=NULL, inputStyle=NULL,
                        groupClass="", labelClass="", inputClass="", autofocus=NULL, placeholder=NULL) {
    value <- restoreInput(id = inputId, default = value)
    af <- if(!is.null(autofocus)) {"autofocus"}
    div(class = paste("form-group shiny-input-container", groupClass), style=groupStyle, label %AND%
        tags$label(label, `class`=labelClass, `style`=labelStyle, `for` = inputId), tags$input(id = inputId,
        type = "text", class = paste("form-control", inputClass), style=inputStyle, autofocus=af, value=value,
        placeholder=placeholder))
}
# ttextInput <- function(inputId, label, value="", style="width:100%;", groupClass="", inputClass="", autofocus=FALSE){
#    af <- if(autofocus) {"autofocus='autofocus'"} else {""}
#    return(tagList(
#       bs4("d", class = paste0("'form-group ", groupClass, "'"),
#       tags$label('for' = inputId, class="control-label", label),
#       HTML(paste0('<input id="', inputId, '" label="', label, '" value="', value,
#                   '" class="form-control shiny-input-container ', inputClass), '" style="',
#                   style, '" type="text" ', af, '/>'))
#    ))
# }


### Create full name from name parts; expects tibble with one row
fullName = function(u) {
   suffix=""
   if(nchar(u$nameSuffix[1])>0) { suffix = paste0(", ", u$nameSuffix[1]) }
   r =  paste0(u$namePrefix[1], " ", u$nameFirst[1], " ",  u$nameMiddle[1], " ",  u$nameLast[1], suffix)
   return(esc(r))
}

   # Generate a long, meaningless, and unique id for the sessionID
generate_id <- function() {
   dup <- TRUE
   while (dup) {                                              # try, try again until it's unique
      newID <- paste(collapse = '', sample(x = c(letters, LETTERS, 0:9), size = 16, replace = TRUE))
      dup <- userGet("userID", tibble(c("sessionID", "=", newID)))$userID != 0      # if userGet() returns a userID>0, sessionID is a dup
   }
   return(newID)
}

   # Generate a short, numeric-only code for email verification and lost passwords
generate_code <- function() {
   return(paste(collapse = '', sample(0:9, size = 6, replace = TRUE)))    # this one doesn't need to be unique
}

   # Generate a short random number used to create new input ids on each render (see bs4 buttons)
generate_rnd <- function(z=4) {
   return(paste0(sample(c(as.character(0:9), letters, str_to_upper(letters)), size=z, replace=TRUE), collapse=""))
}

   # Get our Bootstrap4 stuff
source("bs4.R", local=TRUE)

   # This gets data from MySQL, then formats it for omDT()
omRx = function(db, table, SELECT, WHERE, buttons) {  # SELECT must start with tableID or buttons won't work right
   Rx <- recGet(db, table, SELECT, WHERE)             # Get records to display
   if(Rx[1,1]==0) {                                   # if [1,1]==0, nothing was returned
      Rx <- Rx[-1,]                                   # delete row 1
   } else {                                           # otherwise
      if(length(buttons)>0) {
         for(i in 1:length(buttons)) {                # Add a column of buttons to a tibble
            pattern = rep("XxX", nrow(Rx))
            btn = bs4("btn", id=buttons[[i]]$id, n="XxX", q=buttons[[i]]$q, class=buttons[[i]]$class, buttons[[i]]$label)
            Rx[,ncol(Rx)+1] = str_replace_all(btn, pattern, as.character(Rx[[1]]))   # str_replace is vectorized
         }
      }
   }
   return(Rx)
}

   # Call DT's datatable function with required parameters
omDT = function(Rx, cnames, colesc, noText = "No records found") {
   noText = str_replace_all(noText, "'", "&apos;")    # MUST remove aspostrophes for paste0 below!!!
   return(datatable(Rx,
      options=list(
   # DataTables jQuery plugin parameters go here:
         scrollY = "55vh",
         scrollCollapse = TRUE,
         paging = FALSE,
         initComplete = JS(
            "function(settings, json) {",
               "const t = document.getElementsByTagName('table')",
               "for (var i = 0; i< t.length; i++) {t[i].classList = 'table'}",
               "$('.dataTables_filter').css('color', '#cfd2da');",
               "$('input').css('background-color', '#434857');",
               "$('input').css('border-color', '#434857');",
               "$('input').css('color', '#cfd2da');",
               "$('.dataTables_info').css('color', '#cfd2da');",
               "$('.table').css('margin-bottom', '0');",
               paste0("$('.dataTables_empty').html('<h5>", noText, "</h5>');"),
            "}"
         )
      ),
   # datatable() parameters go here:
      rownames = FALSE,       # No rownames
      filter = "none",        # No filters
      colnames = cnames,
      escape = colesc,
      style = "default",      # boostrap or default; easier to modify default
#     fillContainer = TRUE,   # give me scroll bars, (things go better without this; let datatable plugin figure it out...)
      selection = "none",     # Don't bother with click notifications
      editable = FALSE
      ))
}

   # cards - call these from inside the code for a specific record to deal with deletions and clashes.
deletedCard <- function(t) {
   r = ""
   if (t$deleted[2]) {
      r <- tagList(
         bs4("cd", q="r", class="mb-2",
            bs4("cdh", HTML0('<h5>Deleted Record</h5>')),
            bs4("cdb", HTML0('<p>This record has been deleted and cannot be edited without undeleting it first.</p>'),
               bs4("btn", id="undelete", n=t[[2,1]], q=c("y"), "Undelete")    # n is ID
            )
         ),
         HTML('<div class="text-right mt-3">'),
            bs4("btn", id="exit", n=t[[2,1]], q="b", "Exit"),
         HTML('</div>')
         )
   }
   return(r)
}
clashCard <- function(t) {
   r = ""
   if(t$clash[2]) {
      r <- tagList(
         bs4("cd", q="y", class="mb-2",
             bs4("cdh", HTML('<h5 class="text-dark">Simultaneous update clashes</h5>')),
             bs4("cdb", HTML(t$clashFacts[2]),
                bs4("btn", id="clearClash", n=t[[2,1]], q="r", "Clear clash")
             )
      ))
   }
   return(r)
}
if(showSysMsg) {
   sysMsg = tagList(
      bs4("r", align="hc",
         bs4("c12", tagList(
            bs4("d", class="card bg-danger mx-auto my-4", bs4("d", class="card-body",
                  bs4("d", class="card-title", h4("Alpha-Test Mode")),
                  bs4("d", class="card-text", style="color:#FFF;", HTML("Several parts ",
                     "of the Open-Meta app are in alpha-test; others are unfinished. Feel ",
                     "free to try to break things; nothing you do will be permanent -- I ",
                     "still regularly have to burn down the database and start over. If you ",
                     "have any questions, comments, or bug reports, <a href='?Help', ",
                     "style='color:#FDD;'><u>I\'m eager to hear them</u></a>."))
               ))
   ))))
} else {
   sysMsg = ""
}


### This is the ui; it uses the index.html template to load CSS and JavaScript files, etc.
###    The <html> tag includes: style="font-size:1.8vw;" in an effort to make the font size
###       sensitive to the screen size. The <body> tag has no styles. Note that BootStrap CSS
###       has additional styles for both tags.

ui <- htmlTemplate("index.html",
         uiBody = bs4("cf",
            bs4("r", bs4("c12", id="uiHead")),            # Multiple rows to solve problem with Shiny browser lining the
            bs4("r", bs4("c12", id="uiMeat")),            #    columns up horizontally with a horizontal scroll bar.
            bs4("hr")
      ))

bigHead <- tagList(
      bs4("r", align=c("vb","hb"), class="my-2",
         bs4("ca",
            bs4("r", align="vb",
               bs4("ca", HTML('<img src="http://assets.open-meta.org/images/om-sleuth-icon.png", style="max-width:12vh; height:auto;"/>')),
               bs4("ca", HTML('<h2>Open-Meta.org</h2><p class="mb-0 pb-0">Crowd-sourced systematic reviews and meta-analyses</p>'))
            )
         ),
         bs4("ca", class="ml-auto", id="menuHead")
      ),
      sysMsg,
      bs4("d", id="menuBig")
   )

slimHead <- tagList(
   bs4("r", align=c("vb","hb"), class="my-2",
      bs4("ca",
         bs4("r", align="vb",
            bs4("ca", HTML('<img src="http://assets.open-meta.org/images/om-sleuth-icon.png", style="max-width:4vh; height:auto;"/>')),
            bs4("ca", HTML('<h4 class="mb-0">Open-Meta.org</h4>'))
         )
      ),
      bs4("ca", class="ml-auto", id="menuHead")
   )
)

sessionNum = 0   # This app variable allows us to identify a session in the debugging output

incSN = function() {
   sessionNum <<- sessionNum + 1
   return(sessionNum)
}

# this is a tibble of all pages; this can be shared by all sessions, so we'll load it here
allPages = pageGet(c("pageName", "spReq"), tibble(c("deleted", "=", 0)))

# How large a file can be uploaded. This can be changed in Admin Panel. Setting is app-wide, not session-specific.
r = settingsGet("value", tibble(c("name", "=", "uploadMaxMB")))$value
options(shiny.maxRequestSize = as.numeric(r)*1024^2 ) # convert MB to bytes


### Server Function
### Note that what's above here loads only one time, when the app starts.
### What's below here (the server function) runs every time a new session starts and stops when the session ends.
###    Multiple users have different sessions from each other, of course, but when you use this skeleton,
###    even single users end a session and start a new one every time they go to a different page on the site.

server <- function(input, output, session) {

### Plain old functions that are used by multiple sessions go above the server function, so they only load once.
###    Session-specific variables, however, have to be inside the server function and load every session.

### S$ versus session$userData$
###    The advantage of using S$ for session-specific variables is that the name is short.
###    The disadvantage is that to set it from inside a function (including render... and observe...) you
###        have to use <<- rather than <- or =. Moreover, if you forget to use <<- you might expect a
###        "variable does not exist (inside the function)" error, but you won't get one; this is a hard
###       bug to find.

###    On the other hand, having to use "session$userData" in front of everything is a lot of characters.
###       I started using S$ on 2/7/2018, it seemed worthwhile even with the <<- disadvantage, and went
###       back and changed all session$userData$ variables to S$ on 2/19.

### see: https://community.rstudio.com/t/best-practice-for-session-specific-variables/4994

### NOTE: like session$userData, you can set reactiveValues from inside a function with either <<-, <-, or =.
###    Another possibility is to use these for everything, but I don't know the hit to performance.
### NOTE2: Lists attached to reactiveValues() buzz the entire list when any element changes. Just use an
###    additional reactiveValue, as you can have any number of them and they are all list-like.

   S <- list()                 # session static values
   rv <- reactiveValues()      # session reactive values
                               # for non-reactive variables, use the S$ static-variable list

   rv$loadPage <- 0            # buzzer for ready to load rest of page
   rv$limn <- 0                # render/re-render page buzzer
   rv$modal_warning <- 0       # used with an observer below to bring up modal warning dialogs
   S$modal_title <- ""         # These need to be initalized...
   S$modal_text <- ""
   S$modal_size <- ""
   S$hideMenus = FALSE         # TRUE when only choice should be Save or Cancel

   if(debugON) {
      S$sessionNum <- incSN()   # get an id number for this session
      cat(paste0("Session ", S$sessionNum, " started @ ", now(), ".\n"))
      onSessionEnded(function() {
         cat(paste0("Session ", S$sessionNum, " ended @ ", now(), ".\n\n"))
      })
   }

   # This section shows how to build a menu that's sensitive to whether the user is logged in (user superpower > 0).
   #   Note that you can present various menu options based on the user's superpower level, as with the Admin menu here.
   #   Also note that the code that makes sure this doesn't run until after the cookie observer has finished, because
   #      until then, S$U will be null.
   output$menuHead <- renderUI({c(rv$loadPage, rv$limn); isolate({
      if(rv$loadPage>0) {                                       # skip initialization run
         if(S$U$sPowers==0) {
            links = c("?prjActive", "?profile", "?login")
            text = c("Home", "Register", "Login")
         } else {
            links = c("?prjMy", "?profile", "?login")
            text = c("Home", paste0(S$U$userName,":Profile"), "Logout")
         }
         if(S$U$sPowers >=500) {
            links = c(links, "?adminCP")
            text = c(text, "Admin")
         } else {
            links = c(links, "?Help")
            text = c(text, "Help/Contact")
         }
         if(S$hideMenus) {
            return(HTML("Click <b>Save</b> or <b>Cancel</b> at the bottom of the page."))
         } else {
            return(bs4("mp", text=text, links=links, active=0))
         }
      }
   })})

   output$menuBig <- renderUI({c(rv$loadPage, rv$limn); isolate({
      if(rv$loadPage>0) {                                       # skip initialization run
         if(req(S$U$sPowers)==0) {
            text=c("Active Projects", "Finished Projects", "Start a Project")
            links=c("?prjActive", "?prjFinished", "?prjNew")
            active=which(links %in% paste0("?", S$PG$pageName))
            if(length(active)==0) {active=0}
         } else {
            text=c("My Projects", "Active Projects", "Finished Projects", "Start a Project")
            links=c("?prjMy", "?prjActive", "?prjFinished", "?prjNew")
            active=which(links %in% paste0("?", S$PG$pageName))
            if(length(active)==0) {active=0}
         }
         return(bs4("hrm", text=text, links=links, active=active))

      }
   })})

# Functions for running javascript on the browser
#   Because these communicate using the session object, they have to be in the server.
#   In general, the first parameter is the name of the function and the second is a list of named parameters
#   The javascript code is loaded by the header template and is in the file open-meta.js
#   All these functions are stored in the js$ global
   js = list()

   js$redirect = function(url) {
      session$sendCustomMessage("redirect", url)  # This one expects a string, not a list
   }

   js$setCookie = function(cookieType, cookie, daysTillExpire=0) {
      session$sendCustomMessage("setCookie", list(cookieType=cookieType, cookie=cookie, days=daysTillExpire))
   }

   js$getCookie = function(cookieType) {
      session$sendCustomMessage("getCookie", list(cookieType=cookieType))
   }

   js$removeCookie = function(cookieType) {
      session$sendCustomMessage("removeCookie", list(cookieType=cookieType))
   }

   js$getUTC2me = function() {
      session$sendCustomMessage("getUTC2me", "")
   }

   js$tipsOn = function() {                      # call for browser sessions (?pages) that have BS4 tool tips
      session$sendCustomMessage("tipsOn", "")
   }

   js$quill = function(id, placeholder) {                    # send section code so it can send it back with text
      session$sendCustomMessage("quill", list(id=id, placeholder=placeholder))
   }

   js$getEdit = function(s) {                    # send section code so it can send it back with text
      session$sendCustomMessage("getEdit", s)
   }

### recSave()
   # This needs to be in the server to access session variables
   recSave = function(SET, db="om$prime", pool=shiny.pool) {
      verUser = S$U$userName
      r = recSaveR(SET, verUser, db, pool)
      if(r$modalMsg$title!="") {
         S$modal_title <- r$modalMsg$title
         S$modal_text <- r$modalMsg$text
         rv$modal_warning <- rv$modal_warning + 1
      }
      return(r$r)
   }

### Modal Warning
   # an observer to send modal warnings
   # to call:
   # S$modal_title <- ""
   # S$modal_text <- ""   embedded HTML is ok
   # rv$modal_warning <- rv$modal_warning + 1
   observeEvent(rv$modal_warning, {
         if(S$modal_size=="") { S$modal_size <<- "m" }
         showModal(modalDialog(
            title = HTML(S$modal_title),
            HTML(S$modal_text),
            footer = modalButton("Ok"),
            size=S$modal_size
         ))
         S$modal_title <<- ""
         S$modal_text <<- ""
         S$modal_size <<- ""
   }, ignoreNULL = TRUE, ignoreInit = TRUE)

### All the action starts here! ###

   # Wait until javascript files have finished loading before asking for cookies and UTC2me
   observeEvent(session$clientData$url_port, {  # This is just a hack, but now it's safe
      js$getCookie("sessionID")                 #    to request the sessionID from the user's browser
      js$getCookie("projectDB")
      js$getUTC2me()
   })
#   }, ignoreNULL = TRUE, ignoreInit = TRUE)    # This breaks it!

   # Cookie observer for local time offset from UTC
   observeEvent(input$js.UTC2me, {
      S$UTC2me <<- -minutes(input$js.UTC2me)   # make it date-math friendly
   })

   ### bTime()
   #   Use bTime to display server times in user's local time. For sTime(), see the top of app.R
   bTime = function(s, UTC2me=S$UTC2me) {  # bTime(t) = server time as "browser time" string
      t = ymd_hms(s) + UTC2me                             # depends on UTC2me offset, see js$getUTC2me().
      return(as.character.POSIXt(t))
   }

   # Cookie observer to determine login status
   observeEvent(input$js.sessionID, {                 # Buzzer is a change in the cookie status; only happens once per session
      if(debugON) { cat("Checking cookies...\n")}
      if(input$js.sessionID=="") {                    # not logged in;
         S$U <<- emptySU( )                           # Use a blank S$U
         if(debugON) { cat("...cookie is blank.\n") }
      } else {
         #S$user <<- userGet("**", tibble(c("sessionID", "=", input$js.sessionID)))   # depricated
         S$U <<- user1SU(tibble(c("sessionID", "=", input$js.sessionID)))
         if(S$U$userID != 0) {   # Already logged in
            if(debugON) {
               cat(paste0("...user is ", S$U$userName, "\n"))
            }
         } else {                                     # This happens when we BurnItAllDown
            cat(paste0("\nWARNING: browser session id ", input$js.sessionID, " not in users table.\n\n"))
            S$U <<- emptySU()                         # Use a blank S$U
         }
      }
      rv$loadPage <- rv$loadPage + 1                  # Now load the rest of the server for this URL...
   }, ignoreNULL = TRUE, ignoreInit = TRUE)

   sessionEnvironment = environment()                 # Needed to load page source in right place in the following observer

   observeEvent(rv$loadPage, {
      url <- session$clientData$url_search
      urlParts <- str_split(url, "&")                 # split off any ampersands
      url = urlParts[[1]][1]                          # first part is the url
      if(length(urlParts[[1]])>1) {                   # if there are more parts
         ampParts = unlist(str_split(urlParts[[1]][-1], "="))
         ampPrj = which("prj" %in% ampParts)          # look for &prj
##### Set up for a specific project
         if (ampPrj>0) {                              #    if found in url
            S$PRJ <<- projectGet(c("projectID", "projectName", "status", "privacy"), tibble(c("projectID", "=", ampParts[ampPrj+1])))
            if(S$PRJ$projectID > 0) {                 #    and in database
               S$db <<- paste0("om$prj_", S$PRJ$projectID)   # create DB string for recGet/recSave
      ##### Get User Role in project
               if(S$U$sPowers>=900) {
                  S$PRJ$userRole <<- "Sys-Admin"
               } else {
                  r <<- membershipGet("role", tibble(c("userID", "=", S$U$userID), c("projectID", "=", S$PRJ$projectID)))
                  if(nchar(r$role)>0) {
                     S$PRJ$userRole <<- r$role        # add user role to S$PRJ
                  } else {
                     S$PRJ$userRole <<- "Non-Member"
                  }
               }
      #####
            } else {
               url = "?NoSuchProject"
            }
         }
#####
      }
      if(S$U$sPowers>0 && !S$U$emailOK && url!="?profile") {
         js$redirect("?profile")                      # if logged in but email unverified; force verification next
         return()
      }
      if(url=="") {                                   # blank means "home" page
         if(S$U$sPowers>0) {
            url <- "?prjMy"                           # if logged in, start in My Projects
         } else {
            url <- "?prjActive"                       # otherwise, go to Active Projects
         }
      }
      url <- substr(url, 2, nchar(url))               # remove leading "?"
      i.vec = allPages$pageName == url
      if(any(i.vec)) {                                # is that one of our files?
         S$PG <<- allPages[i.vec,]
         source(paste0(S$PG$pageName, ".R"), local=sessionEnvironment)   # load and run server code for this page
         rv$limn <- rv$limn + 1                       # buzz page's render function
      } else {
         output$ui <- renderUI(bigHead)
         output$uiMeat <- renderUI({
            if(debugON) {
               cat(paste0("Rendering 404 Error v.", rv$loadPage, "\n"))
            }
            if(url=="NoSuchProject") {
               return(tagList(
                  slimHead,
                  bs4("r", bs4("ca", class="mx-auto", style="width:300px",
                  HTML("<h4>404 Not Found Error:</h4><p>That project doesn't exist.</p>")
                  ))
               ))
            } else {
               return(tagList(
                  slimHead,
                  bs4("r", bs4("ca", class="mx-auto", style="width:300px",
                     HTML("<h4>404 Not Found Error:</h4><p>That URL doesn't exist. Use the",
                    "menu above to navigate to the page you were looking for.</p>")
                  ))
               ))
            }
         })
      }
   }, ignoreNULL = TRUE, ignoreInit = TRUE)

} # end of server

# Run the application
shinyApp(ui = ui, server = server)
