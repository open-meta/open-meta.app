### open-meta.app adminPages.R
### Tom Weishaar - Dec 2017 - v0.1

# Although users without superpowers can't get here via the menus, they CAN get here by typing in the URL.
S$userOK <- S$U$sPowers >= S$PG$spReq

S$editPage <- FALSE               # editPage and newPage flags determine what gets rendered
S$newPage <- FALSE
# Note:
# ...$user is the logged in user

output$uiHead <- renderUI({        # No rv$limn because this only needs to be rendered once.
   if(A$debugON) {
      cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
   }
   if(S$userOK) {
      text=c("Control Panel","Users", "Pages", "Projects", "User Projects", "Project Users", "Forms")
      links=c("?adminCP", "?adminUsers", "?adminPages", "?adminProjects", "?adminUserPrj", "?adminPrjUser", "?adminForms")
      active=which(links %in% paste0("?", S$PG$pageName))
      if(length(active)==0) {active=0}
      return(tagList(
         slimHead,
         bs4("hrm", text=text, links=links, active=active)))
   } else {
      return(tagList(
         slimHead,
         bs4("hrt", "You haven't been granted access to this page.")
      ))
   }
})

output$uiMeat <- renderUI({rv$limn; isolate({
   if(rv$limn && S$userOK) { # check permissions again
      if(!S$editPage && !S$newPage) {                # Show results list
         return(tagList(
            bs4("r", align="hc",
               bs4("c8",
                  bs4("d", id="uiFilters"),
                  DTOutput("uiResults")  # Can't bs4 this one...
               )
            )
         ))
      }
      if(S$newPage) {                 # Edit the page
         return(tagList(
            bs4("r", align="hc",
               bs4("c5",
                  tagList(
                     ttextInput(inputId="pageName", label="Page Name",
                        groupClass = "w-100",
                        inputClass = "w-100",
                        autofocus = TRUE),
                     ttextInput(inputId="spReq", label="Super Powers Required",
                        groupClass = "w-100",
                        inputClass = "w-25"),
                     HTML('<div class="text-right mt-3">'),
                        bs4("btn", id="cancelNew", n=0, q="r", "Cancel"),
                        bs4("btn", id="save", n=0, q="b", "Save"),
                     HTML('</div>')
                  )
               )
            )
         ))
      }
      if(S$editPage) {                 # Edit the page
         dCard <- deletedCard(S$px)
         cCard <- clashCard(S$px)
         return(tagList(
            bs4("r", align="hc",
               bs4("c5",
               { if((dCard[1])!="") {
                  dCard
               } else {
                  tagList(cCard,
                     HTML0('pageID: ', S$px$pageID[2], '<br/>'),
                     ttextInput(inputId="pageName", label="Page Name",
                        groupClass = "w-100",
                        inputClass = "w-100",
                        autofocus = TRUE,
                        value=S$px$pageName[2]),
                     ttextInput(inputId="spReq", label="Required Super Powers",
                        groupClass = "w-100",
                        inputClass = "w-25",
                        value=S$px$spReq[2]),
                     HTML0(bTime(S$px$verTime[2]), ' - Last edit (',
                           S$px$verUser[2], ' v.',
                           S$px$verNum[2], ')<br/>'),
                     HTML('<div class="text-right mt-3">'),
                        bs4("btn", id="Fdelete", n=S$px$pageID[2], q="r", "Delete Page"),
                        bs4("btn", id="cancel", n=S$px$userID[2], q="b", "Cancel"),
                        bs4("btn", id="save", n=S$px$userID[2], q="b", "Save"),
                     HTML('</div>')
                  )
               }}
            ))
         ))
      }
   } # if permissions check fails, return nothing, there's already a message in output$uiHead...
})})

### Filters
rv$menuActive=1    # start with Active records only
output$uiFilters <- renderUI({
   return(tagList(
            bs4("md", id="um1", n=1:4, active=rv$menuActive, text=c("Active Pages", "Deleted Pages", "Clashes", "New Page"))
         ))
})

### Results
uiResultsRV = reactiveValues()                        # Make a new one of these for each output
uiResultsRV$WHERE = tibble(c("deleted", "=", 0))      # default filter
uiResultsRV$newWHERE = 0                              # inc this when WHERE changes

output$uiResults  <- renderDT(
   {z = uiResultsRV$newWHERE
    Rx = omRx(
      db = "om$prime",
      table = "page",
      SELECT = c("pageID", "pageName", "spReq"),
      WHERE = uiResultsRV$WHERE,
      buttons = list(edit=list(id="edit", label="Edit Details", q="y", class=""))
   )
   # if you need to further modify Rx, you can do it here.
   Rx <- Rx[,-1]                                      # Remove ID column
   omDT(Rx,
      cnames = c("Page Name", "Required Super Powers", "Edit"),     # names for column headers
      colesc = c(1:2),                                # columns to escape (all except button columns)
      noText = "No pages found"                    # What to say when there are no results
   )},
# renderDT() parameters go here:
   server = FALSE
)

# rowBuilder = function(t) {
#    rows=tagList()
#    for (i in 1:nrow(t)) {
#       rows[[i]] = tagList(
#          HTML('<tr><td><h6>', esc(t$pageName[i]), '</h6></td>',
#                   '<td>Super Powers Required to Access Page: ', esc(t$spReq[i]), '</td>',
#                    '<td style="vertical-align:middle">'
#              ),
#              bs4("btn", id="edit", n=t$pageID[i], q=c("y"), class="mr-2", "Edit Details"),
#          HTML('</td></tr>')
#       )
#    }
#    return(rows)
# }

### observer for omclick
observeEvent(input$js.omclick, {
   if(A$debugON) {
      cat(paste0("\nClick on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   vmsg = ""               # Flag and message for validation errors
   switch(id,
      "edit" = {
         S$editPage <<- TRUE
         S$newPage <<- FALSE
         S$px <<- pageGet("**", tibble(c("pageID", "=", n), c("deleted", ">=", "0")))
         rv$limn = rv$limn + 1
      },
      "um1" = {
         rv$menuActive=n
         if(n==1) {
            uiResultsRV$WHERE = tibble(c("deleted", "=", 0))
         }
         if(n==2) {
            uiResultsRV$WHERE = tibble(c("deleted", "=", 1))
         }
         if(n==3) {
            uiResultsRV$WHERE = tibble(c("clash", ">", 0))
         }
         if(n==4) {
            S$editPage <<- FALSE
            S$newPage <<- TRUE
            S$px <<- pageGet()
         }
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1
         rv$limn = rv$limn + 1
      },
      "undelete" = {
         S$px$deleted[2] <<- 0
         S$px <<- recSave(S$px)
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1    # didn't change the WHERE, but did change WHERE's results
         rv$limn = rv$limn + 1
      },
      "clearClash" = {
         S$px$clash[2] <<- 0
         S$px <<- recSave(S$px)
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1    # didn't change the WHERE, but did change WHERE's results
         rv$limn = rv$limn + 1
      },
      "Fdelete" = {
         S$px$deleted[2] <<- 1
         S$px <<- recSave(S$px)                              # recSave won't return a deleted record
         S$editPage <<- FALSE                                #   so we have to go back to the list.
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1    # didn't change the WHERE, but did change WHERE's results
         rv$limn = rv$limn + 1
      },
      "save" = {
         if(checkInput(c("pageName", "superPower"), c(input$pageName, input$spReq))) {
            S$px$pageName[2] <<- input$pageName
            S$px$spReq[2] <<- input$spReq
            S$px <<- recSave(S$px)
            S$newPage <<- FALSE
            S$editPage <<- FALSE
            uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1 # didn't change the WHERE, but did change WHERE's results
            rv$menuActive = 1
         }
         rv$limn = rv$limn + 1                               # Need a new id on the save button or it won't work twice
      },
      "cancelNew" = {
         S$newPage <<- FALSE
         S$editPage <<- FALSE
         rv$menuActive = 1
         uiResultsRV$WHERE = tibble(c("deleted", "=", 0))
         uiResultsRV$newWHERE <- uiResultsRV$newWHERE + 1
         rv$limn = rv$limn + 1
      },
      "cancel" = {
         S$newPage <<- FALSE
         S$editPage <<- FALSE
         rv$limn = rv$limn + 1
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

checkInput = function(n, v) { # name, value (can be a vector)
   if(length(n)!=length(v)) { stop("In checkInput, n and v have different lengths.") }
   vmsg <- ""                 # initialize
   for(i in 1:length(n)) {
      switch(n[i],
         pageName = {
            t <- v[i]
            t <- str_replace_all(t, "[^[:alnum:]]", "")         # make input alpahnumeric...
            t <- str_replace_all(t, " ", "")                    #    ...with no spaces
            if(nchar(t)!=nchar(v[i]) || t=="") {
               vmsg = HTML0(vmsg, "<li>Page name can't be blank, have spaces, or have special characters.</li>")
            }
            if(pageGet("pageID", tibble(c("pageName", "=", v[i])))$pageID > 0) { # check againt all existing pageNames
               vmsg <- HTML0(vmsg, "<li>", v[i], " is taken.</li>")
            }
         },
         superPower = {
            if(is.na(as.numeric(v[i])) || as.numeric(v[i]) < 0 || as.numeric(v[i]) > 1000) {
               vmsg = HTML0(vmsg, "<li>Super powers must be between 0 and 1000.</li>")
            }
         },
         message(paste0("In checkInput, no code for ", incoming[i]))
      )
   }
   if(nchar(vmsg) > 0 ) {
         S$modal_title <<- "Validation Error."
         S$modal_text <<- HTML("<p>One or more fields have invalid values:<ul>", vmsg, "</ul></p>")
         rv$modal_warning <- rv$modal_warning + 1
         return(FALSE)
   }
   return(TRUE)
}
