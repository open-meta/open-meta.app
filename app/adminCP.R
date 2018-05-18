### open-meta adminUsers.R
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
      text=c("Control Panel","Users", "Pages", "Projects", "User Projects", "Project Users")
      links=c("?adminCP", "?adminUsers", "?adminPages", "?adminProjects", "?adminUserPrj", "?adminPrjUser")
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
         settings = settingsGet(c("value","comment"), tibble(c("name", "=", "uploadMaxMB")))
         return(tagList(
            bs4("r", align="hc", bs4("c8",
               ttextInput("uploadMaxMB", "Maximum File Upload Size", settings$value),
               bs4("quill", id="upSizeCmt", settings$comment),
               bs4("d", class="text-right", bs4("btn", id="SaveUpSize", q=c("b"), class="mt-2", "Save")),
               bs4("hr")
            )),
            bs4("r", align="hc", bs4("c8",
               actionButton("restart_btn", "Restart App", class="btn-danger"),
               bs4("hr")
            ))
         ))
      }
   }
})})

# databases = fromJSON(settingsGet("value", tibble(c("name", "=", "databases")))$value)
# citeFormats = fromJSON(settingsGet("value", tibble(c("name", "=", "citeFormats")))$value)
      # s = settingsGet()
      # s$name[2] = "databases"
      # s$value[2] = toJSON(c("PubMed-Live", "Cochrane-TXT", "Web of Knowledge-CIW", "Embase-CSV", "Ovid-RIS", "PMIDs-TXT", "Other"))
      # s$comment[2] = paste0("<p></p>")
      # r = recSaveR(s, pool=root.pool)
      # s$name[2] = "citeFormats"
      # s$value[2] = toJSON(c("RIS", "CSV", "CIW", "TXT", "BibTEX", "BibLatex"))
      # s$comment[2] = paste0("<p></p>")
      # r = recSaveR(s, pool=root.pool)

observeEvent(input$restart_btn, {
   stopApp()
})

# Save runs this
observeEvent(input$js.editorText, {
   s = input$js.editorText[1]                  # The quill id
   t = input$js.editorText[2]                  # The edited text
   switch(s,
      "upSizeCmt" = {
         settings = settingsGet("**", tibble(c("name", "=", "uploadMaxMB")))
         settings$value[2] = input$uploadMaxMB
         settings$comment[2] = t
         recSave(settings)
         options(shiny.maxRequestSize = as.numeric(input$uploadMaxMB)*1024^2 ) # convert MB to bytes
         S$modal_title <<- "Success"
         S$modal_text <<- HTML("<p>Maximum File Upload Size Saved.</p>")
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
         js$getEdit("upSizeCmt")
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)
