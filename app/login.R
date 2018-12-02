### open-meta.app login.R
### Tom Weishaar - April 2018 - v0.2

# v.2 Make this the logout page as well.

output$uiHead <- renderUI(tagList(slimHead, bs4("hr")))

# Logout anyone who gets here and is logged in
if(S$U$sPowers) {                            # sPower of 1 or more (TRUE) means a user is logged in
   js$removeCookie("sessionID")              # Delete session id from browser...
   S$U <- emptySU()                          # Get an empty user for S$U
}

output$uiMeat <- renderUI({rv$limn; isolate({
   if(rv$limn) {                             # don't run this unless rv$limn is 1 or more
      if(A$debugON) {
         cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
      }
      return(tagList(
         bs4("r", bs4("ca", class="mx-auto", style="width:300px",
            bs4("r",
               bs4("c12",
                  ttextInput("userName", "User Name", value="",
                             groupClass = "w-100", inputClass = "w-100", autofocus=TRUE),
                  passwordInput("password", "Password:", value="", width="100%")
            )),
            bs4("r", align="hc",
               bs4("c12",
                  HTML('<div class="form-group shiny-input-container mb-1">',
                          '<div class="checkbox text-center">',
                             '<label>',
                                '<input id="stayloggedin" type="checkbox"/>',
                                '<span>Stay logged in.</span>',
                             '</label>',
                          '</div>',
                       '</div>')
               ),
               bs4("c12",
                  actionButton("login_btn", "Login", class="btn-primary w-100 mb-1")
               )
            ),
            bs4("r",
               bs4("ca", class="mx-auto mt-2",
                  HTML('<h5 style="text-align:center;"><a href="?lostpassword">Lost Password</a></h5>',
                       '<h5 style="text-align:center;"><a href="?profile">Register</a></h5>'               )
               )
            )
      ))))
   }
})})

observeEvent(input$login_btn, {
   errormsg <- ""
   user2 <- userGet("**", tibble(c("userName", "=", esc(input$userName))))
   if(user2$userName[2] != "") {                                        # valid userName?
      if(checkpw(input$password, user2$hashedPW[2])) {                  # right password?
         cookie <- generate_id()                                        # create a new session id
         if(input$stayloggedin) {                                       # save ID to user's browser
            js$setCookie("sessionID", cookie, days=14)                  # expire after 14 days
         } else {
            js$setCookie("sessionID", cookie, days=0)                   # expire after session
         }
         user2$sessionID[2] <- cookie                                   # update $user$sessionID
         user2$loginDate[2] <- sTime()                                  # update last login date
         user2 <- recSave(user2)                                        # save new user data
         S$U <<- emptySU()                                              # Now get an empty user.
         if(!user2$emailOK[2]) {
            js$redirect("?profile")                                     # if email is invalid, go to profile to fix it
         } else {
         js$redirect("?prjMy")                                          # on successful login, go to home page
         }
      } else {
         errormsg <- "Incorrect Password."
      }
   } else {
      errormsg <- "Unknown User Name."
   }
   if(nchar(errormsg)>0) {
      S$modal_title <<- "Whoops!"
      S$modal_text <<- paste0("<p>", errormsg, "</p>")
      rv$modal_warning <- rv$modal_warning + 1
   }
})

