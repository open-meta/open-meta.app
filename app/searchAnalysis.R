### open-meta.app searchAnalysis.R
### Tom Weishaar - placeholder

output$uiHead <- renderUI({        # No rv$limn because this only needs to be rendered once.
   if(A$debugON) {
      cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
   }
   return(tagList(bigHead))
})

output$uiMeat <- renderUI({rv$limn; isolate({
   if(rv$limn) {
      if(S$U$sPowers>0) {      # if logged in
         return(tagList(
            bs4("r", align="hc",
               bs4("c8",
                  ttextInput(inputId="projectName", label="Project Name",
                             groupClass = "my-2 w-100", autofocus=TRUE, value=""),
                  HTML('<div class="text-right mt-3">'),
                     bs4("btn", id="cancel", n=1, q="b", "Cancel"),
                     bs4("btn", id="save", n=1, q="b", "Start Project"),
                  HTML('</div>'),
                  HTML('<div class="card bg-warning text-dark w-3 mx-auto my-4"><div class="card-body"><div class="card-title">',
                       '<h5 class="text-dark">Your New Project</h5></div>',
                       '<div class="card-text">After you name your project and click <i>Start Project</i>, your project',
                       "will appear in your <b><i>My Projects</i></b> list. However, it will not appear as an",
                       "<b><i>Active Project</i></b> until you complete a detailed description of your project.",
                       "Open-Meta supports the PRISMA-P protocol for describing systematic reviews and meta analyses",
                       "(<a href='https://www.bmj.com/content/bmj/349/bmj.g7647.full.pdf' target='_blank'>Shamseer",
                       "et al., 2015</a>).</div></div></div>")
               )
            )
         ))
      } else {                                      # if not logged in
         return(tagList(
            bs4("r", align="hc",
               bs4("c8", class="mt-3",
                  HTML0("<h5>You must be logged in to start a project and you must register before you can log in.</h5>")
               )
            ),
               bs4("ca", class="mt-4 text-center",
                  bs4("btn", id="login", n=1, class="mr-2", q=c("b", "l", "p"), "Login"),
                  bs4("btn", id="register", n=1, q=c("b", "l", "p"), "Register")
               )
         ))
      }
   } # if permissions check fails, return nothing, there's already a message in output$uiHead...
})})

### observer for omclick
observeEvent(input$js.omclick, {
   if(A$debugON) {
      cat(paste0("Click on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   vmsg = ""               # Flag and message for validation errors
   switch(id,
      "cancel" = {
         if(S$U$sPowers>0) {      # if logged in
            js$redirect("?prjMy")
         } else {
            js$redirect("?prjActive")
         }
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

checkInput = function(n, v) { # name, value (can be a vector)
   if(length(n)!=length(v)) { stop("In checkInput, n and v have different lengths.") }
   vmsg <- ""                 # initialize
   for(i in 1:length(n)) {
      switch(n[i],
         projectName = {
            if(v[i]=="") {
               vmsg = HTML0(vmsg, "<li>Project name can't be blank.</li>")
            }
            if(projectGet("projectID", tibble(c("projectName", "=", v[i])))$projectID > 0) { # check againt all existing projectNames
               vmsg <- HTML0(vmsg, "<li>", v[i], " is taken.</li>")
            }
         },
         message(paste0("In checkInput, no code for ", incoming[i]))
      )
   }
   if(nchar(vmsg) > 0 ) {
         S$modal_title <<- "Validation Error."
         S$modal_text <<- HTML("<p>Project Name isn't valid:<ul>", vmsg, "</ul></p>")
         rv$modal_warning <- rv$modal_warning + 1
         return(FALSE)
   }
   return(TRUE)
}




