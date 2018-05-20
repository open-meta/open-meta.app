### open-meta.app Publish.R
### Tom Weishaar - placeholder

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

rv$menuActive = 1    # Start out on first sub-menu

if(S$P$Msg=="") {
   output$uiMeat <- renderUI({c(rv$menuActive, rv$limn); isolate({
      if(rv$limn && S$P$Msg=="") {
         if(S$P$Modify) {                               # Modification permission required to see anything here!
            return(tagList(
               bs4("md", id="sub", n=1:3, active=rv$menuActive, text=c("Graphs", "Tables", "Reference Lists")),
               bs4("r", bs4("ca", "More to come..."))
            ))
         }
      }
   })})
}

### observer for omclick
observeEvent(input$js.omclick, {
   if(debugON) {
      cat(paste0("Click on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "sub" = {
         rv$menuActive = n
      },
      # "view" = {
      #    S$modal_title <<- "Under Construction."
      #    S$modal_text <<- HTML("<p>Sorry, this feature isn't available yet.</p>")
      #    rv$modal_warning <- rv$modal_warning + 1
      # },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)


