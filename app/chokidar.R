### open-meta chokidar.R
### Tom Weishaar - Apr 2018 - v0.1

# This file provides one place to deal with permissions...

# When we arrive here:
#   * S$U is a 1x6 tibble with userID, userName, hashedPW, email, emailOK, and sPowers
#   * S$PRJ is a 1x5 tibble with projectID, projectName, status, and privacy PLUS "userRole" field from membership table
#   * S$PG is a 1x2 tibble with pageName (stripped of ? and .R) and spReq
#source("chokidar.R", local=TRUE)
# When we leave here:
#   * S$P has permissions in it
#   * output$uiHead has been rendered with any necessary error messages and "hrm" menus
#   * if(S$P$Msg=="") the user has permission to see the page and you can render the Meat
#   * S$P$Modify  - whether the user has permission to modify things on this page

# These will be TRUE if current user has specified role OR HIGHER.
S$P$SA   <-  S$PRJ$userRole == "Sys-Admin"
S$P$PI   <-  S$P$SA || S$PRJ$userRole %in% c("Principal Investigator", "Co-Principal Investigator", "Project Administrator")
S$P$R    <-  S$P$PI || S$PRJ$userRole == "Researcher"
S$P$I    <-  S$P$R || S$PRJ$userRole == "Investigator"
S$P$Rev  <-  S$P$I || S$PRJ$userRole == "Reviewer"
S$P$O    <-  S$P$Rev || S$PRJ$userRole == "Observer"
S$P$Log  <-  S$U$sPowers > 0         # Is user even logged in?
S$P$Modify <- FALSE                  # Flag for whether the user can edit on this page
S$P$Msg  <- ""                       # Error flag and message

if(S$PRJ$privacy==1 && !S$P$O) {
   S$P$Msg <- "This project is private."
}

if(S$P$Msg=="") {
   switch(S$PG$pageName,
      "Protocol" = {
         if(S$PRJ$status==0 && !S$P$Rev) {    # Before activation, you can't see the Protocol unless you are Rev & up.
            S$P$Msg <- "You cannot view the Protocol until the project sponsors have completed it."
         }
         if(S$PRJ$status==0 && S$P$I) { S$P$Modify <- TRUE }  # Before activation, Investigators & up can work on Protocoll
         if(S$PRJ$status!=0 && S$P$PI) {S$P$Modify <- TRUE }  # After activation, only PIs can Amend Protocol
      },
      "Join" = {
         if(S$PRJ$status==2) {
            S$P$Msg <- "This project is complete. There is no further work to do so there is no reason to join."
         }
         if(S$P$O) {
            S$P$Msg <- "You are already a member of this project."
         }
         if(S$P$SA) {
            S$P$Msg <- "Admin accounts aren't allowed to join projects."
         }
      },
      "Search" = {
         if(S$PRJ$status==0) {
            S$P$Msg <- "Project must be activated before this feature will be available."
         }
         if(S$PRJ$status==2 && S$P$SA) { S$P$Modify <- TRUE }
         if(S$PRJ$status==1 && S$P$R) { S$P$Modify <- TRUE }
      },
      "Review" = {
         if(S$PRJ$status==0) {
            S$P$Msg <- "Project must be activated before this feature will be available."
         }
         if(S$PRJ$status==2 && S$P$SA)  { S$P$Modify <- TRUE }
         if(S$PRJ$status==1 && S$P$Rev) { S$P$Modify <- TRUE }
      },
      "Extract" = {
         if(S$PRJ$status==0) {
            S$P$Msg <- "Project must be activated before this feature will be available."
         }
         if(S$PRJ$status==2 && S$P$SA) { S$P$Modify <- TRUE }
         if(S$PRJ$status==1 && S$P$I)  { S$P$Modify <- TRUE }
      },
      "Synthesize" = {
         if(S$PRJ$status==0) {
            S$P$Msg <- "Project must be activated before this feature will be available."
         }
         if(S$PRJ$status==2 && S$P$SA) { S$P$Modify <- TRUE }
         if(S$PRJ$status==1 && S$P$PI) { S$P$Modify <- TRUE }
      },
      "Publish" = {
         S$P$Msg <- "The Publish functionality allows the project\'s principal investigators to download publication-ready graphs, tables, and reference lists, but it\'s not available until the project is marked as finished."
         if(S$PRJ$status==2 && S$P$PI) {
            S$P$Modify <- TRUE
            S$P$Msg <- ""
         }
      },
      "prjAdmin" = {
         if(S$P$PI) { S$P$Modify <- TRUE }            # Show Members and Settings
      }
   )

}

output$uiHead <- renderUI({rv$limn; isolate({
   if(rv$limn) {                          # prevents a premature run while rv$limn = 0
      if(debugON) {                       # debug message
         cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
      }
      if(S$P$Msg != "") {                 # we have an error
          S$P$Msg <<- tagList(h5(class="text-center m-5 lead", HTML(S$P$Msg)))
      }
      if(S$PRJ$status==0 && !S$P$O) {
         text=c("Protocol", "Join", "Contact")
         links=c("?Protocol", "?Join", "?prjAdmin")
      }
      if(S$PRJ$status==0 && S$P$O) {
         text=c("Protocol", "Contact")
         links=c("?Protocol", "?prjAdmin")
      }
      if(S$PRJ$status==1  && !S$P$O) {
         text=c("Protocol", "Join", "Search", "Review", "Extract", "Synthesize", "Publish", "Contact")
         links=c("?Protocol", "?Join", "?Search", "?Review", "?Extract", "?Synthesize", "?Publish", "?prjAdmin")
      }
      if((S$PRJ$status==1 && S$P$O) || S$PRJ$status==2) {
         text=c("Protocol", "Search", "Review", "Extract", "Synthesize", "Publish", "Contact")
         links=c("?Protocol", "?Search", "?Review", "?Extract", "?Synthesize", "?Publish", "?prjAdmin")
      }
      if(S$P$PI) {                    # For prjAdmin, PI and up gets "Members & Settings" rather than "Contact"
         text[length(text)] = "Members & Settings"
      }
      active=which(links %in% paste0("?", S$PG$pageName))   # figure out which link is active
      links = paste0(links, "&prj=", S$PRJ$projectID)        # add projectID to links
      if(length(active)==0) {active=0}
      if(S$hideMenus) {
         return(tagList(                  # just slimhead, no menus
            slimHead,
            bs4("hr")
         ))
      } else {
         return(tagList(                  # render slimhead with projectName and links
            slimHead,
            bs4("r", align="hc",
               bs4("c12",
                  bs4("cd", class="mx-auto my-4 w-75", q="b",
                      bs4("cdb", class="p-2",
                      bs4("cdt", h5(class="text-center m-0 lead", HTML(S$PRJ$projectName))))),
                  bs4("hrm", text=text, links=links, active=active),
                  S$P$Msg)
            )
         ))
      }
   }
})})
