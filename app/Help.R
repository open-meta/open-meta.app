### open-meta.app Help.R
### Tom Weishaar - Apr 2018 - v0.1

# No chokidar because anyone can use this page; even login isn't required

source("email-core.R", local=TRUE)

output$uiHead <- renderUI({        # No rv$limn because this only needs to be rendered once.
   if(debugON) {
      cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
   }
   return(tagList(
      slimHead
   ))
})

output$uiMeat <- renderUI({rv$limn; isolate({
   h4text="Email me your questions and comments"
   return(tagList(
      bs4("r", align="hc",
         bs4("c7", tagList(
            bs4("d", class="card bg-warning text-dark mx-auto my-4", bs4("d", class="card-body",
               bs4("d", class="card-title", h4(class='text-dark', "Help / Contact")),
               bs4("d", class="card-text", "Hi, I\'m Tom Weishaar, the post-retirement Heatlh Education doctoral student who is building Open-Meta.org. At the moment there are no help files, but I'm happy to answer any questions, eager to receive comments, and delighted with bug reports. You can contact me using the form below.")
            )),
            emailWrite(h4text, loggedIn = S$U$sPowers)
      )))
   ))
})})


### observer for omclick
observeEvent(input$js.omclick, {
   if(debugON) {
      cat(paste0("\nClick on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "sendEmail" = {
         if(emailCheck(S$U$sPowers > 0)) {
            uA = userGet(c("userName", "email"), tibble(c("userID", "=", 1))) # userID = 1 is Admin
            S$emailName <<- uA$userName
            S$emailAdr <<- uA$email
            S$emailSubject <<- esc(input$emailSubject)
            S$emailText <<- paste0(esc(input$emailFromAdr), " has asked for help.\n\n-----\n\n", esc(input$emailText))
            S$emailFromName <<- "Open-Meta Email Robot"
            S$emailFromAdr <<- "email.robot@open-meta.org"
            rv$sendEmail = rv$sendEmail + 1
            js$redirect("?prjActive")
         }
      },
      "cancel" = {
         js$redirect("?prjActive")
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

