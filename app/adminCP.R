### open-meta.app adminCP.R
### Tom Weishaar - Dec 2017 - v0.1

output$uiHead <- renderUI({rv$limn; isolate({
   if(rv$limn) {
      if(debugON) {
         cat(paste0("Rendering ", S$PG$pageName, " v.", rv$limn, "\n"))
      }
      if(S$U$sPowers < S$PG$spReq) {   # Although users without superpowers can't get here via the
         return(tagList(               #    menus, they CAN get here by typing in the URL.
            slimHead,
            bs4("hrt", "You haven't been granted access to this page.")
         ))
      } else {
      text=c("Control Panel","Users", "Pages", "Projects", "User Projects", "Project Users", "Forms")
      links=c("?adminCP", "?adminUsers", "?adminPages", "?adminProjects", "?adminUserPrj", "?adminPrjUser", "?adminForms")
         active=which(links %in% paste0("?", S$PG$pageName))
         if(length(active)==0) {active=0}
         return(tagList(
            slimHead,
            bs4("hrm", text=text, links=links, active=active)))
      }
   }
})})

output$uiMeat <- renderUI({rv$limn; isolate({
   if(rv$limn) {
      if(S$U$sPowers >= S$PG$spReq) {   # hide meat from weaklings
         # uploadMaxMB = settingsGet(c("value","comment"), tibble(c("name", "=", "uploadMaxMB")))
         uploadMaxCites = settingsGet(c("value","comment"), tibble(c("name", "=", "uploadMaxCites")))
         shinyinfo = ""
         if(str_sub(getwd(),2,2)!=":") { # skip when on Windows
            shinyinfo = HTML0("<pre>", str_trim(paste0(system('shiny-server --version', intern = TRUE), collapse="\n")), "</pre>")
         }
         return(tagList(
            bs4("r", align="hc", bs4("c8",
#               ttextInput("uploadMaxMB", "Maximum File Upload Size", uploadMaxMB$value),
               ttextInput("uploadMaxCites", "Maximum Citations per Search", uploadMaxCites$value),
               bs4("quill", id="maxCitesCmt", uploadMaxCites$comment),
               bs4("d", class="text-right", bs4("btn", id="SaveUpSize", q=c("b"), class="mt-2", "Save")),
               bs4("hr")
            )),
            bs4("r", align="hc", bs4("c8",
               bs4("btn", id="restart", n=1, q="r", "Restart App"),
               bs4("hr")
            )),
            shinyinfo,
            HTML0("<pre>", str_trim(paste0(capture.output(sessionInfo(), file=NULL), collapse="\n")), "</pre>")
         ))
      }
   }
})})

# Save runs this
observeEvent(input$js.editorText, {
   s = input$js.editorText[1]                  # The quill id
   t = input$js.editorText[2]                  # The edited text
   switch(s,
      "maxCitesCmt" = {
         # settings2 = settingsGet("**", tibble(c("name", "=", "uploadMaxMB")))
         # settings2$value[2] = input$uploadMaxMB
         # recSave(settings2)
         settings2 = settingsGet("**", tibble(c("name", "=", "uploadMaxCites")))
         settings2$value[2] = input$uploadMaxCites
         settings2$comment[2] = t
         recSave(settings2)
#         options(shiny.maxRequestSize = as.numeric(input$uploadMaxMB)*1024^2 ) # convert MB to bytes
         S$modal_title <<- "Success"
         S$modal_text <<- HTML("<p>Maximum Citations per Search saved.</p>")
         rv$modal_warning <- rv$modal_warning + 1
      }
   )
})

### observer for omclick
observeEvent(input$js.omclick, {
   if(debugON) {
      cat(paste0("Click on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "SaveUpSize" = {
         js$getEdit("maxCitesCmt")
      },
      "restart" = {
         stopApp()
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)
