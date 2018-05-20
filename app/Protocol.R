### open-meta.app Protocol.R
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
   # Get protoHelp
   pH = recGet("om$prime", "protoHelp", "*", WHERE=tibble(c("protoHelpID", ">", "0")))
   proto = recGet(paste0("om$prj_", S$PRJ$projectID), "protocol", "*", WHERE=tibble(c("protocolID", ">", "0")))

   # set up a reactive triggers for each section
   for(s in pH$order) {
      rv[[s]] = 0
   }

   # trigger each section
   for(s in pH$order) {
      rv[[s]] = rv[[s]] + 1
   }

   # Add head tags to titles based on length of order (longer = smaller header)
   pH$h = nchar(pH$order)
   pH$S = ifelse(pH$h==1, "",
            ifelse(pH$h==2, " style='padding-left:3rem;'",
              ifelse(pH$h==3, " style='padding-left:6rem;'",
                " style='padding-left:9rem;'")))
   pH$H = ifelse(pH$h==1, "h2", ifelse(pH$h==2, "h4", ifelse(pH$h==3, "h5", "h6")))


      # Hard-coded sections
      # proto_A is Overall Protocol Instructions
   proto_A = if(S$PRJ$status>0) {    # skip this after project has been activated
         ""
      } else {
         myTitle = HTML(paste0("<", pH$H[1], " class='text-dark'>", pH$title[1], "<", pH$H[1], ">"))
         bs4("d", class="card bg-warning text-dark mx-auto my-4", bs4("d", class="card-body",
            bs4("d", class="card-title", HTML(myTitle)),
            bs4("d", class="card-text", HTML(pH$helpText[pH$order=="A"]))
         ))
      }

      # proto_E is the Activation Instructions and Button at the bottom
   proto_E = if(S$PRJ$status>0) {    # skip this after project has been activated
         ""
      } else {
         i = length(pH$H)
         myTitle = HTML(paste0("<", pH$H[i], " class='text-dark'>", pH$title[i], "<", pH$H[i], ">"))
         # If all the buttons are green, add an activation button.
         if(all(proto$text[!(proto$order %in% c("A", "Aa", "Ab", "B", "C", "D", "Da", "Dd", "Dh", "E"))] != "<p><br></p>")) {
            actButton = tagList(
               bs4("d", class="text-center", bs4("btn", id="activate", class="w-75", q=c("b","l"), "Activate Project"))
            )
         } else {
            actButton = ""
         }
         tagList(
            bs4("d", class="card bg-warning text-dark mx-auto my-4", bs4("d", class="card-body",
               bs4("d", class="card-title", HTML(myTitle)),
               bs4("d", class="card-text", HTML(pH$helpText[pH$order=="E"]))
            )),
            actButton
         )
      }

   editMe = FALSE # initialize
   output$uiMeat <- renderUI({c(rv$menuActive, rv$limn); isolate({
      if(rv$limn) { tagList(
         bs4("r", align="hc", bs4("c9", id="A")),
         bs4("r", align="hc", bs4("c9", id="Aa")),
         bs4("r", align="hc", bs4("c9", id="Ab")),
         bs4("hr"),
         bs4("r", align="hc", bs4("c9", id="B")),
         bs4("r", align="hc", bs4("c9", id="Ba")),
         bs4("r", align="hc", bs4("c9", id="Bb")),
         bs4("r", align="hc", bs4("c9", id="Bc")),
         bs4("r", align="hc", bs4("c9", id="Bd")),
         bs4("r", align="hc", bs4("c9", id="Be")),
         bs4("r", align="hc", bs4("c9", id="Bf")),
         bs4("r", align="hc", bs4("c9", id="Bg")),
         bs4("hr"),
         bs4("r", align="hc", bs4("c9", id="C")),
         bs4("r", align="hc", bs4("c9", id="Ca")),
         bs4("r", align="hc", bs4("c9", id="Cb")),
         bs4("hr"),
         bs4("r", align="hc", bs4("c9", id="D")),
         bs4("r", align="hc", bs4("c9", id="Da")),
         bs4("r", align="hc", bs4("c9", id="Daa")),
         bs4("r", align="hc", bs4("c9", id="Dab")),
         bs4("r", align="hc", bs4("c9", id="Dac")),
         bs4("r", align="hc", bs4("c9", id="Dad")),
         bs4("r", align="hc", bs4("c9", id="Dae")),
         bs4("r", align="hc", bs4("c9", id="Daf")),
         bs4("r", align="hc", bs4("c9", id="Db")),
         bs4("r", align="hc", bs4("c9", id="Dc")),
         bs4("r", align="hc", bs4("c9", id="Dd")),
         bs4("r", align="hc", bs4("c9", id="Dda")),
         bs4("r", align="hc", bs4("c9", id="Ddb")),
         bs4("r", align="hc", bs4("c9", id="Ddc")),
         bs4("r", align="hc", bs4("c9", id="De")),
         bs4("r", align="hc", bs4("c9", id="Df")),
         bs4("r", align="hc", bs4("c9", id="Dg")),
         bs4("r", align="hc", bs4("c9", id="Dh")),
         bs4("r", align="hc", bs4("c9", id="Dha")),
         bs4("r", align="hc", bs4("c9", id="Dhb")),
         bs4("r", align="hc", bs4("c9", id="Dhc")),
         bs4("r", align="hc", bs4("c9", id="Dhd")),
         bs4("r", align="hc", bs4("c9", id="Di")),
         bs4("r", align="hc", bs4("c9", id="Dj")),
         bs4("hr"),
         bs4("r", align="hc", bs4("c9", id="E"))
      )}
   })})

   output$A    <- renderUI({ rv[['A']]; proto_A })
   output$Aa   <- renderUI({ rv[['Aa']]; isolate({ sectionBuilder('Aa', editMe)}) })
   output$Ab   <- renderUI({ rv[['Ab']]; isolate({ sectionBuilder('Ab', editMe)}) })
   output$B    <- renderUI({ rv[['B']]; isolate({ sectionBuilder('B', editMe)}) })
   output$Ba   <- renderUI({ rv[['Ba']]; isolate({ sectionBuilder('Ba', editMe)}) })
   output$Bb   <- renderUI({ rv[['Bb']]; isolate({ sectionBuilder('Bb', editMe)}) })
   output$Bc   <- renderUI({ rv[['Bc']]; isolate({ sectionBuilder('Bc', editMe)}) })
   output$Bd   <- renderUI({ rv[['Bd']]; isolate({ sectionBuilder('Bd', editMe)}) })
   output$Be   <- renderUI({ rv[['Be']]; isolate({ sectionBuilder('Be', editMe)}) })
   output$Bf   <- renderUI({ rv[['Bf']]; isolate({ sectionBuilder('Bf', editMe)}) })
   output$Bg   <- renderUI({ rv[['Bg']]; isolate({ sectionBuilder('Bg', editMe)}) })
   output$C    <- renderUI({ rv[['C']]; isolate({ sectionBuilder('C', editMe)}) })
   output$Ca   <- renderUI({ rv[['Ca']]; isolate({ sectionBuilder('Ca', editMe)}) })
   output$Cb   <- renderUI({ rv[['Cb']]; isolate({ sectionBuilder('Cb', editMe)}) })
   output$D    <- renderUI({ rv[['D']]; isolate({ sectionBuilder('D', editMe)}) })
   output$Da   <- renderUI({ rv[['Da']]; isolate({ sectionBuilder('Da', editMe)}) })
   output$Daa  <- renderUI({ rv[['Daa']]; isolate({ sectionBuilder('Daa', editMe)}) })
   output$Dab  <- renderUI({ rv[['Dab']]; isolate({ sectionBuilder('Dab', editMe)}) })
   output$Dac  <- renderUI({ rv[['Dac']]; isolate({ sectionBuilder('Dac', editMe)}) })
   output$Dad  <- renderUI({ rv[['Dad']]; isolate({ sectionBuilder('Dad', editMe)}) })
   output$Dae  <- renderUI({ rv[['Dae']]; isolate({ sectionBuilder('Dae', editMe)}) })
   output$Daf  <- renderUI({ rv[['Daf']]; isolate({ sectionBuilder('Daf', editMe)}) })
   output$Db   <- renderUI({ rv[['Db']]; isolate({ sectionBuilder('Db', editMe)}) })
   output$Dc   <- renderUI({ rv[['Dc']]; isolate({ sectionBuilder('Dc', editMe)}) })
   output$Dd   <- renderUI({ rv[['Dd']]; isolate({ sectionBuilder('Dd', editMe)}) })
   output$Dda  <- renderUI({ rv[['Dda']]; isolate({ sectionBuilder('Dda', editMe)}) })
   output$Ddb  <- renderUI({ rv[['Ddb']]; isolate({ sectionBuilder('Ddb', editMe)}) })
   output$Ddc  <- renderUI({ rv[['Ddc']]; isolate({ sectionBuilder('Ddc', editMe)}) })
   output$De   <- renderUI({ rv[['De']]; isolate({ sectionBuilder('De', editMe)}) })
   output$Df   <- renderUI({ rv[['Df']]; isolate({ sectionBuilder('Df', editMe)}) })
   output$Dg   <- renderUI({ rv[['Dg']]; isolate({ sectionBuilder('Dg', editMe)}) })
   output$Dh   <- renderUI({ rv[['Dh']]; isolate({ sectionBuilder('Dh', editMe)}) })
   output$Dha  <- renderUI({ rv[['Dha']]; isolate({ sectionBuilder('Dha', editMe)}) })
   output$Dhb  <- renderUI({ rv[['Dhb']]; isolate({ sectionBuilder('Dhb', editMe)}) })
   output$Dhc  <- renderUI({ rv[['Dhc']]; isolate({ sectionBuilder('Dhc', editMe)}) })
   output$Dhd  <- renderUI({ rv[['Dhd']]; isolate({ sectionBuilder('Dhd', editMe)}) })
   output$Di   <- renderUI({ rv[['Di']]; isolate({ sectionBuilder('Di', editMe)}) })
   output$Dj   <- renderUI({ rv[['Dj']]; isolate({ sectionBuilder('Dj', editMe)}) })
   output$E    <- renderUI({ rv[['E']]; proto_E })

   sectionBuilder = function(section, editMe) {
      i = which(section==pH$order)
      if(section %in% c("Aa", "Ab")) {             # Amendments & Publications only when status == 1
         editOK = S$PRJ$status==1 && S$P$Modify
      } else {                                     # Everything else when status == 0
         editOK = S$PRJ$status==0 && S$P$Modify
      }
      sectionTitle = HTML(paste0("<", pH$H[i], pH$S[i], ">", pH$title[i], "</", pH$H[i], ">")) # Fix title with right hn and indent
      if(editMe && editOK) {                       # Do you want (and can you have) a piece of me?
         sectionTitle=""                           # Fix the title so it can go in the Yellow box instead
         myTitle = HTML(paste0("<", pH$H[i], " class='text-dark'>", pH$title[i], "</", pH$H[i], ">"))
         switch(section,                           # Special inputs for...
            "Aa" = {                                  # ...Amendments
               inputSection = tagList(
                  HTML(proto$text[i]),
                  selectInput('sectionList', 'Protocol section you are amending: ', pH$title[nchar(pH$order)>1], selectize=FALSE),
                  bs4("quill", id=paste0(section, "Ed"))
               )
            },
            "Ab" = {                                  # ...Publications
               inputSection = tagList(
                  HTML(proto$text[i]),
                  bs4("quill", id=paste0(section, "Ed"))
               )
            },
            "Ba" = {                                  # ...Project Name
               inputSection = tagList(
                  bs4("quill", id=paste0(section, "Ed"), S$PRJ$projectName)
               )
            },
            {                                         # ...everything else
               inputSection = tagList(
                  bs4("quill", id=paste0(section, "Ed"),  proto$text[i])
               )
            }
         )
         sectionText = tagList(
            bs4("d", class="card bg-warning text-dark mt-2 mb-4", bs4("d", class="card-body",
               bs4("d", class="card-title", HTML(myTitle)),
               bs4("d", class="card-text", HTML(pH$helpText[i]))
            )),
            inputSection
         )
         sectionButton = bs4("d", class="text-right mt-2", bs4("btn", id="cancel", n=i, q="b", "Cancel"), bs4("btn", id="save", n=i, q="b", "Save"))
      } else {                                        # Otherwise no instructions or editor
         if(section=="Ba") {                          # Special handling for Project Name
            sectionText = HTML(S$PRJ$projectName)
         } else {
            sectionText = HTML(proto$text[i])
         }
         if(editOK && nchar(pH$helpText[i])>0) {      # If no helpText, it's just a header
            if(sectionText != "<p><br></p>"){ q="g" } else { q="i"}
            sectionButton = bs4("d", class="text-right", bs4("btn", id="edit", n=i, q=q, "Edit"))
         } else {
            sectionButton = ""
         }
      }
      skipHR  = section %in% c("Ab", "Bg", "Cb", "Dj")
      return(tagList(
               sectionTitle,
               sectionText,
               sectionButton,
               if(!skipHR && nchar(pH$helpText[i])>0) bs4("hr")
      ))
   }

   observeEvent(input$js.editorText, {
      s = input$js.editorText[1]              # The section code
      s = str_sub(s, 1, nchar(s)-2)           # Remove "ED"
      t = input$js.editorText[2]              # The edited text
      if(s == "Aa") {              # Special handling for Amendments and Publications
         t2 = proto$text[s==proto$order]
         t2 = paste0(t2, "<p>==========<br>Date of Amendment: ", as.character(today()), "<br>")
         t2 = paste0(t2, "Author of Amendment: ", S$U$userName, ", ", S$PRJ$userRole, "<br>")
         t2 = paste0(t2, "Section Amended: ", esc(input$sectionList), "</p>")
         t = paste0(t2, t)
      }
      if(s == "Ab") {              # Special handling for Amendments and Publications
         t2 = proto$text[s==proto$order]
         t2 = paste0(t2, "<p>==========<br>Date of Addition: ", as.character(today()), "<br>")
         t2 = paste0(t2, "Added by: ", S$U$userName, ", ", S$PRJ$userRole, "</p>")
         t = paste0(t2, t)
      }
      if(s == "Ba") {              # Special handling for Project name; gets saved in three extra places!
         t = str_replace_all(t, "<p>", "")
         t = str_replace_all(t, "</p>", "")   # Quill adds these but they aren't helpful here
         if(nchar(t)>254) {
            t = str_sub(t,1,254)
            S$modal_title <<- "Whoops."
            S$modal_text <<- HTML("<p>Project name is limited to 254 characters.</p>")
            rv$modal_warning <- rv$modal_warning + 1
         }
         prj = projectGet("**", WHERE=tibble(c("projectID", "=", S$PRJ$projectID)))
         prj$projectName[2] = t
         recSave(prj)                         # In the project file
         proto$text[proto$order==s] <<- t     # In the proto tibble
         S$PRJ$projectName <<- t              # And in the S variable
         rv$limn = rv$limn + 1                # Rerender page to correct projectName in header
      }
      pro = recGet(S$db, "protocol", "**", WHERE=tibble(c("order", "=", s)))
      pro$text[2] = t
      r = recSave(pro, S$db)                  # Save the edited text in sQL
      proto$text[proto$order==s] <<- t        # Also update proto tibble
      rv[[s]] = rv[[s]] + 1                   # Re-render this section
   })

   ### observer for omclick
   observeEvent(input$js.omclick, {
      if(debugON) {
         cat(paste0("Click on ", input$js.omclick, "\n"))
      }
      uid = str_split(input$js.omclick, "_")
      id = uid[[1]][1]       # We don't care about the value of uid[[1]][3]; it's just there
      n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
      s = pH$order[as.numeric(n)]
      switch(id,
         "edit" = {
            if(editMe) {
               S$modal_title <<- "Whoops."
               S$modal_text <<- HTML("<p>You have to <b>Cancel</b> or <b>Save</b> the open edit dialog before opening another one.</p>")
               rv$modal_warning <- rv$modal_warning + 1
            } else {
               editMe <<- TRUE
               rv[[s]] = rv[[s]] + 1
            }
         },
         "cancel" = {
            editMe <<- FALSE
            rv[[s]] = rv[[s]] + 1
         },
         "save" = {
            editMe <<- FALSE
            js$getEdit(paste0(s, "Ed"))
         },
         "sub" = {
            rv$menuActive = n
         },
         "activate" = {
            prj = projectGet("**", WHERE=tibble(c("projectID", "=", S$PRJ$projectID)))
            prj$status[2] = 1
            recSave(prj)
            js$redirect(paste0("?Protocol&prj=", S$PRJ$projectID))   # Rerender page
         },
         # "view" = {
         #    S$modal_title <<- "Under Construction."
         #    S$modal_text <<- HTML("<p>Sorry, this feature isn't available yet.</p>")
         #    rv$modal_warning <- rv$modal_warning + 1
         # },
         message(paste0("In input$js.omclick observer, no handler for ", id, "."))
      )
   }, ignoreNULL = TRUE, ignoreInit = TRUE)
}

