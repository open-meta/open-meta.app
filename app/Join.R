### open-meta.app Join.R
### Tom Weishaar - Apr 2018 - v0.1

# When we arrive here:
#   * S$U is a 1x5 tibble with userID, userName, email, sPowers, and emailOK
#   * S$PRJ is a 1x5 tibble with projectID, projectName, status, and privacy PLUS "userRole" field from membership table
#   * S$PG is a 1x2 tibble with pageName (stripped of ? and .R) and spReq
source("chokidar.R", local=TRUE)
# When we leave here:
#   * S$P has permissions in it
#   * output$uiHead has been rendered with any necessary error messages and "hrm" menus
#   * if(S$P$Msg=="") the user has permission to see the page and you can render the Meat
#   * S$P$Modify  - whether the user has permission to modify things on this page

if(S$P$Msg=="") {
   output$uiMeat <- renderUI({rv$limn; isolate({
      if(S$U$sPowers == 0) {                       # user isn't logged in
         return(tagList(
            bs4("r", align="hc",
               bs4("ca", class="mt-3 text-center",
                  h5("You must be logged in to join a project and you must register before you can log in.")
               )
            ),
            bs4("r", align="hc",
               bs4("ca", class="mt-4 text-center",
                  bs4("btn", id="login", n=1, class="mr-1", q=c("b", "l", "p"), "Login"),
                  bs4("btn", id="register", n=1, class="ml-1", q=c("b", "l", "p"), "Register")
               )
            )
         ))
      }
      return(tagList(                              # show Join button
         bs4("r", align="hc",
            bs4("ca", class="mt-4 text-center",
               bs4("btn", id="join", n=S$PRJ$projectID, q=c("b", "l", "p"), "Join This Project")
            )
         ))
      )
   })})

   ### observer for omclick
   observeEvent(input$js.omclick, {
      if(debugON) {
         cat(paste0("Click on ", input$js.omclick, "\n"))
      }
      uid = str_split(input$js.omclick, "_")
      id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
      n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
      switch(id,
         "login" = { js$redirect("?login") },
         "register" = { js$redirect("?profile") },
         "join" = {
            membership <- membershipGet("**", tibble(c("projectID", "=", n), c("userID", "=", S$U$userID)))
            if(membership$membershipID[1] == 0) {    # if membershipID != 0...
               membership$projectID[2] <- n          #    ...then user is already a member of the project...
               membership$userID[2] <- S$U$userID    #    ...skip this
               membership$role[2] <- "Observer"
               membership$contact[2] <- 0
               membership <- recSave(membership)
            }
            js$redirect("?prjMy")
         },
         message(paste0("In input$js.omclick observer, no handler for ", id, "."))
      )
   }, ignoreNULL = TRUE, ignoreInit = TRUE)
}
