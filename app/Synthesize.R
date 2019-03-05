### open-meta.app Synthesize.R
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

# load the inputMeta.R code (also used by other pages)
source("inputMeta.R", local=TRUE)
source("forest.robu2.R", local=TRUE)

rv$menuActive = 1    # Start out on first sub-menu
rv$limnSetup = 0
rv$Forest = 0

S$editFORM <- FALSE
S$NUMs$analysisNUM <- 0
S$noAnalysis <- TRUE

S$R <- recGet(S$db, "result",
           c("resultID", "catalogID", "studyNUM", "armNUM", "P", "I", "C", "O", "TS", "info", "esType", "nC", "nI", "es", "v", "ci.lo", "ci.hi"),
           tibble(d = c("deleted", "=", "0")))
S$noResults <- ifelse(S$R$resultID[1]==0, TRUE, FALSE)
S$M <- tibble()
S$R2 <- tibble()
S$Rx <- tibble()   # Table for robumeta
S$Rp <- tibble()   # Table for publication


if(S$P$Msg=="") {
   output$uiMeat <- renderUI({c(rv$limn); isolate({
      if(rv$limn && S$P$Msg=="") {
         if(S$editFORM && S$P$Modify) {
            return(tagList(
               bs4("r", align="hc",
                  bs4("c10",
                  bs4("r", id="editForm")
               ))
            ))
         } else {
            switch(as.character(rv$menuActive),
               "1" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="Setup")
                     ))
                  ))
               },
               "2" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="Prisma")
                     ))
                  ))
               },
               "3" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="Sequential")
                     ))
                  ))
               },
               "4" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="Forest")
                     ))
                  ))
               },
               "5" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="Bias")
                     ))
                  ))
               },
               # "6" = {
               #    return(tagList(
               #       bs4("r", align="hc",
               #          bs4("c10",
               #          bs4("r", id="pageMenu"),
               #          bs4("r", id="Subgroup")
               #       ))
               #    ))
               # },
               "6" = {
                  return(tagList(
                     bs4("r", align="hc",
                        bs4("c10",
                        bs4("r", id="pageMenu"),
                        bs4("r", id="MetaRegression")
                     ))
                  ))
               }
            )
         }
      }
   })})
}

output$pageMenu <- renderUI({c(rv$menuActive, rv$limn); isolate({
   if(S$hideMenus) { return("") }
   return(
      tagList(
         bs4("c12",
         bs4("md", id="sub", n=1:6, active=rv$menuActive, text=c("Analyze", "PRISMA diagram", "Sequential Analysis", "Forest Plot", "Bias Plots", "Meta-Regression")),
            bs4("dx", style="height:1.5rem")
         )
      )
   )
})})

output$editForm <- renderUI({c(rv$limn); isolate({
   db <- ifelse(str_sub(S$editFORMname,1,5)=="Form-", "om$prime", S$db)
   S$IN$FORM <<- imGetFORM(S$editFORMname, db)                    # S$editFORMname is the only input here
   S$IN$FORM <<- imGetFORMvalues(S$IN$FORM)                       # Heavily uses inputMeta.R functions
   return(tagList(                                                # Save will use rv$imGetFORMData, which
      imForm2HTML(S$IN$FORM),                                     #   needs S$IN$FORM, which is set up here
      bs4("c12", class="text-right",
         bs4("btn", uid=paste0("cancelForm_", S$editFORMname), q="b", class="mr-3", "Cancel"),
         bs4("btn", uid=paste0("saveForm_", S$editFORMname), q="b", class="mr-3", "Save")
      )
   ))
})})

output$Setup <- renderUI({c(rv$limn, rv$limnSetup); isolate({
#   print(paste0("Running output$Setup; analysisNUM: ", S$NUMs$analysisNUM))
   if(S$noResults) { return(HTML0(("<h5>Nothing to display - extraction must be complete on at least one study.</h5>"))) }
   A <- recGet(S$db, "analysis",
           c("analysisID", "name", "type", "P", "I", "C", "O", "TS", "comment"),
           tibble(d = c("deleted", "=", "0")))
   if(S$P$Modify) {
      newAnyBtn <- tagList(                                          # New Any/Edit Any buttons
         bs4("c12", bs4("btn", uid="addForm_Form-Analysis", q="g", class="mr-3", "Add a New Analysis")))
      editAnyBtn <- tagList(
         bs4("c12", class="text-right", bs4("btn", uid="editForm_Form-Analysis", q="g", class="mr-3", "Edit Analysis")))
   } else {
      newAnyBtn <- editAnyBtn <- ""
   }
   if(S$NUMs$analysisNUM==0) {    # Show picker
      S$noAnalysis <<- TRUE                                          # Flag for other submenus
      ID = "viewAny"                                                 # This allows multiple pickRs on a single page
      TABLE = "analysis"                                             # The table the pickR data will come from
      WHERE=tibble(d=c("deleted", "=", "0"))
      FilterF = whereFilter                                          # typically whereFilter
      HeadlineF = THRUb                                              # typically THRUb
      SELECT = "name"                           # These are the table fields needed to build the pickR
      if(S$P$Modify) {                                               # View or Edit depends on permissions
        ButtonData <- list(edit=list(id=paste0("viewAny"), q="b", class="mr-2", label="View Analysis"),
                           delete=list(id=paste0("deleteX"), q="r", class="mr-2", label="Delete Analysis"))
      } else {
        ButtonData <- list(view=list(id=paste0("viewAny"), q="b", label="View"))
      }
      ButtonF = stdButtons                                           # use just the function name; no quotes, no ()
      FixDataF = THRU
      FormatF = prf_analysis
      NOtext = "No Analyses have been created yet."
      activePage = ifelse(is.null(S$PKR[[ID]]$activePage), 1, S$PKR[[ID]]$activePage)
      itemsPerPage = S$PKR$itemsPerPage                              # Modifiable pickR-by-pickR
      scroll = FALSE                                                 # Modifiable pickR-by-pickR
      results <- pickR(ID, S$db, TABLE, WHERE, FilterF, HeadlineF, SELECT, ButtonData, ButtonF,
                    FixDataF, FormatF, NOtext, activePage, itemsPerPage, scroll)
      return(tagList(
         newAnyBtn,
         results
      ))
   }
   if(S$NUMs$analysisNUM>0) {    # Show selected analysis
      S$noAnalysis <<- FALSE                                          # Flag for other submenus
      FORM <- imGetFORM("Form-Analysis", "om$prime")
      FORM <- imGetFORMvalues(FORM)
      FORM$disabled <- TRUE                                          # Disable the FORM until editing
      S$Any$FORM <<- FORM
      otherAnyBtn <- tagList(
         bs4("c12", class="pl-0 pb-2", bs4("btn", uid="viewAny_0", q="b", class="mr-3", "Select a Different Analysis")))
      results <- HTML0('<pre><span style="font-size: 1rem;">', analyze(), '</span></pre>')
      return(tagList(
         otherAnyBtn,
         imForm2HTML(FORM),
         editAnyBtn,
         results
      ))
   }
})})

# Standard pickR formatting functions
prf_analysis = function(r) {                        # Standard function for one column of data and one row of buttons
   return(paste0(
'<div class="row">
   <div class="col-4">', r[[1,]], '</div>
   <div class="col-5 text-right">', r[[2,]], '</div>',
#   <div class="col-5></div>',
   bs4('c12', bs4('hr0', class="py-2")), '
</div>', collapse = ''))
}

output$Prisma <- renderUI({c(rv$menuActive, rv$limn); isolate({
   return(HTML0("<p>PRISMA diagram upcoming...</p>")
   )
})})

output$Sequential <- renderUI({c(rv$menuActive, rv$limn); isolate({
   if(S$noResults) { return(HTML0(("<h5>Nothing to display - extraction must be complete on at least one study.</h5>"))) }
   if(S$noAnalysis) { return(HTML0(("<h5>Nothing to display - no analysis selected in <i>Analyze</i>.</h5>"))) }
   return(HTML0("<p>Sequential Analysis upcoming...</p>")
   )
})})

output$Forest <- renderUI({c(rv$menuActive, rv$limn); isolate({
   if(S$noResults) {
      rv$Forest <- 0
      return(HTML0(("<h5>Nothing to display - extraction must be complete on at least one study.</h5>")))
   }
   if(S$noAnalysis) {
      rv$Forest <- 0
      return(HTML0(("<h5>Nothing to display - no analysis selected in <i>Analyze</i>.</h5>")))
   }
   rv$Forest <- rv$Forest + 1
   if(is.null(S$M$data)) {
      return("")
   } else {
      h =  150 + (length(unique(S$M$data$studyNUM)) * 45) + (length(S$M$data$es) * 12)
      return(tagList(
         plotOutput("FPlot", height = h),
         if(S$P$Modify) {
            bs4("c12", class="text-right",
                downloadButton(outputId = "downloadData", class="m-3 btn-primary", label = "Download Data"),
                downloadButton(outputId = "downloadModel", class="m-3 btn-primary", label = "Download Model"),
                downloadButton(outputId = "downloadForest", class="m-3 btn-primary", label = "Download Plot")
            )
         }
      ))
   }
})})

output$FPlot <- renderPlot({
  #    View(S$M$data)
  #    View(S$M$data.full)
   if(rv$Forest>0) {      # See output$Forest; this disables output$FPlot when there's nothing to display
      forest.robu2(S$M, es.lab = "XName", study.lab = "studyName")   # NOTE: the model, S$M; not the results, S$R
   }
})

# download Handler for the Forest Plot
output$downloadForest <- downloadHandler(
 filename = function() {
   fname <- recGet(S$db, "analysis", "name", tibble(d = c("analysisID", "=", S$NUMs$analysisNUM)))
   fname <- paste0("Forest Plot for ", fname[1,1], ".png")
   return(fname)
 },
 content = function(file) {
   h =  150 + (length(unique(S$M$data$studyNUM)) * 45) + (length(S$M$data$es) * 12)
   png(file, width=2400, height=h*2.1, units="px", res=150)
   forest.robu2(S$M, es.lab = "XName", study.lab = "studyName")
   dev.off()
 }
)

# download Handler for the Model
output$downloadModel <- downloadHandler(
 filename = function() {
   fname <- recGet(S$db, "analysis", "name", tibble(d = c("analysisID", "=", S$NUMs$analysisNUM)))
   fname <- paste0("Model for ", fname[1,1], ".RDS")
   return(fname)
 },
 content = function(file) {
    saveRDS(S$M, file=file)
 }
)

# download Handler for the Data
output$downloadData <- downloadHandler(
 filename = function() {
   fname <- recGet(S$db, "analysis", "name", tibble(d = c("analysisID", "=", S$NUMs$analysisNUM)))
   fname <- paste0("Data for ", fname[1,1], ".csv")
   return(fname)
 },
 content = function(file, data=S$Rp) {

#    S$Rp <<- R %>% select(studyName, Y, BiasFund, BiasHiding, BiasStaff, BiasSubj, BiasAttr,
#                            I, O, TS, info, nC, nI)
    studies <- unique(data$studyName)
    blanks <- rep("", length(studies))
    R <- tibble(study=blanks, biasnames=blanks, bias=blanks, O=blanks, I=blanks, TS=blanks, nC=blanks, nI=blanks)
    for(i in 1:length(studies)) {
       S <- data %>% filter(studyName == studies[i])
       R$study[i] <- S$studyName[1]
       R$biasnames[i] <- paste0("Industry funding:\nRandomization and allocation concealment:\n",
                                "Blinding of research personnel:\nBlinding of participants:\n",
                                "Level of attrition and exclusions:")
       R$bias[i] <- paste0(S$BiasFund[1], "\n", S$BiasHiding[1], "\n", S$BiasStaff[1], "\n",
                           S$BiasSubj[1], "\n", S$BiasAttr[1])
       R$O[i]  <- paste0(S$O, collapse="\n")
       R$I[i]  <- paste0(S$Dose, collapse="\n")
       R$TS[i] <- paste0(S$TimeSpan, collapse="\n")
       R$nC[i] <- paste0(S$nC, collapse="\n")
       R$nI[i] <- paste0(S$nI, collapse="\n")

    }
    write_csv(R, path=file)
    # write.csv(S$R, file=file, row.names=FALSE)
 }
)

      # return(ggplot(data=S$R2,
      #    aes(x = XName, y = es, ymin = ci.lo, ymax = ci.hi ))+
      #    geom_pointrange(aes(col=XName))+
      #    geom_hline(aes(fill=XName),yintercept =1, linetype=2)+
      #    xlab('Study')+ ylab("Effect Size (95% Confidence Interval)")+
      #    geom_errorbar(aes(ymin=ci.lo, ymax=ci.hi,col=XName),width=0.5,cex=1)+
      #    facet_wrap(~studyName,strip.position="left",nrow=nrow(S$R2),scales = "free_y") +
      #    theme(plot.title=element_text(size=16,face="bold"),
      #       axis.text.y=element_blank(),
      #       axis.ticks.y=element_blank(),
      #       axis.text.x=element_text(face="bold"),
      #       axis.title=element_text(size=12,face="bold"),
      #    strip.text.y = element_text(hjust=0,vjust = 1,angle=180,face="bold"))+
      #    coord_flip()
      #    )

# https://sakaluk.wordpress.com/2016/02/16/7-make-it-pretty-plots-for-meta-analysis/
   # apatheme=theme_bw()+
   #   theme(panel.grid.major=element_blank(),
   #         panel.grid.minor=element_blank(),
   #         panel.border=element_blank(),
   #         axis.line=element_line(),
   #    #     text=element_text(family='Sans'),
   #         legend.position='none')
   # return(ggplot(S$R2, aes(y=studyName, x=es, xmin=ci.lo, xmax=ci.hi, shape = tester))+
   #      #Add data points and color them black
   #      geom_point(color = 'black')+
   #      #Add 'special' points for the summary estimates, by making them diamond shaped
   #  #    geom_point(data=subset(S$R2, tester=='Summary'), color='black', shape=18, size=4)+
   #      #add the CI error bars
   #      geom_errorbarh(height=.1)+
   #      #Specify the limits of the x-axis and relabel it to something more meaningful
   #      scale_x_continuous(limits=c(-2,2), name='Standardized Mean Difference (d)')+
   #      #Give y-axis a meaningful label
   #      ylab('Reference')+
   #      #Add a vertical dashed line indicating an effect size of zero, for reference
   #      geom_vline(xintercept=0, color='black', linetype='dashed')+
   #      #Create sub-plots (i.e., facets) based on levels of setting
   #      #And allow them to have their own unique axes (so authors don't redundantly repeat)
   #      facet_grid(setting~., scales= 'free', space='free')+
   #      #Apply my APA theme
   #      apatheme)

   # reverses the factor level ordering for labels after coord_flip()
#   S$R2$studyName <- factor(S$R2$studyName, levels=rev(S$R2$studyName))
   # S$R2 <- arrange(S$R2, es)
   # View(S$R2)
   # return(ggplot(data=S$R2, aes(x=XName, y=es, ymin=ci.lo, ymax=ci.hi)) +
   #         geom_pointrange() +
   #         geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
   #         coord_flip() +  # flip coordinates (puts labels on y axis)
   #         xlab("Study") + ylab("Effect Sizes (95% CI)") +
   #         apatheme)
   # }
#})

output$Bias <- renderUI({c(rv$menuActive, rv$limn); isolate({
   if(S$noResults) { return(HTML0(("<h5>Nothing to display - extraction must be complete on at least one study.</h5>"))) }
   if(S$noAnalysis) { return(HTML0(("<h5>Nothing to display - no analysis selected in <i>Analyze</i>.</h5>"))) }
   return(HTML0("<p>Bias Plots upcoming...</p>")
   )
})})

# output$Subgroup <- renderUI({c(rv$menuActive, rv$limn); isolate({
#    if(S$noResults) { return(HTML0(("<h5>Nothing to display - extraction must be complete on at least one study.</h5>"))) }
#    if(S$noAnalysis) { return(HTML0(("<h5>Nothing to display - no analysis selected in <i>Analyze</i>.</h5>"))) }
#    return(HTML0("<p>Subgroup Analysis upcoming...</p>")
#    )
# })})

output$MetaRegression <- renderUI({c(rv$menuActive, rv$limn); isolate({
   if(S$noResults) { return(HTML0(("<h5>Nothing to display - extraction must be complete on at least one study.</h5>"))) }
   if(S$noAnalysis) { return(HTML0(("<h5>Nothing to display - no analysis selected in <i>Analyze</i>.</h5>"))) }
   return(HTML0("<p>Meta-regression upcoming...</p>")
   )
})})

### observer for omclick
observeEvent(input$js.omclick, {
   if(A$debugON) {
      cat(paste0("Click on ", input$js.omclick, "\n"))
   }
   uid = str_split(input$js.omclick, "_")
   id = uid[[1]][1]        # We don't care about the value of uid[[1]][3]; it's just there
   n  = uid[[1]][2]        #   to guarantee Shiny.onInputChange sees something new and returns it.
   switch(id,
      "sub" = {
         rv$menuActive = n
         rv$limn = rv$limn + 1
      },
      "viewAny" = {
         S$NUMs$analysisNUM <<- n
         S$hideMenus <<- FALSE
         rv$limnSetup <- rv$limnSetup + 1
      },
      "deleteX" = {
         if(S$P$Modify) {
            r <- recGet(S$db, "analysis", c("**"), tibble(c("analysisID", "=", n)))
            r$deleted[2] <- 1
            r <- recSave(r, S$db)
            rv$limnSetup = rv$limnSetup + 1
         }
      },
      "addForm" = {
         if(S$P$Modify) {
            S$editFORM <<- TRUE
            S$editFORMname <<- n
            r <- recGet(S$db, "analysis", c("analysisID"), tibble(c("deleted", ">=", 0)))
            S$NUMs$analysisNUM <<- max(r$analysisID) + 1
            S$hideMenus <<- TRUE
            rv$limn <- rv$limn + 1
         }
      },
      "editForm" = {
         if(S$P$Modify) {
            S$editFORM <<- TRUE
            S$editFORMname <<- n
            S$hideMenus <<- TRUE
            rv$limn <- rv$limn + 1
         }
      },
      "cancelForm" = {
         S$editFORM <<- FALSE
         S$hideMenus <<- FALSE
         rv$limn <- rv$limn + 1
      },
      "saveForm" = {
         if(S$P$Modify) {
            msg=""
               n <- str_trim(stripHTML(as.character(input[["AnalysisName"]]))) == ""
               P <- is.null(input[["AnalysisP"]])
               I <- is.null(input[["AnalysisI"]])
               C <- is.null(input[["AnalysisC"]])
               O <- is.null(input[["AnalysisO"]])
               TS <- is.null(input[["AnalysisTS"]])
               if(n) {
                  msg = paste0(msg, "<li>Analysis name can't be blank</li>")
               }
               if(P) {
                  msg = paste0(msg, "<li>You must check at least one Participant Group</i></li>")
               }
               if(I) {
                  msg = paste0(msg, "<li>You must check at least one Intervention</i></li>")
               }
               if(C) {
                  msg = paste0(msg, "<li>You must check at least one Comparison</i></li>")
               }
               if(O) {
                  msg = paste0(msg, "<li>You must check at least one Outcome</i></li>")
               }
               if(TS) {
                  msg = paste0(msg, "<li>You must check at least one Time Span</i></li>")
               }
            if(msg!="") {
               S$modal_title <<- "Whoops"
               S$modal_text <<- HTML("<p>Can't save this analysis because:<ul>", msg, "</ul></p>")
               rv$modal_warning <- rv$modal_warning + 1
            } else {
               S$editFORM <<- FALSE
               S$hideMenus <<- FALSE
               rv$imGetFORMData <- rv$imGetFORMData + 1
               rv$limnSetup <- rv$limnSetup+1
            }
         }
      },
      "download" = {
         if(n=="forest") {
         }
      },
      message(paste0("In input$js.omclick observer, no handler for ", id, "."))
   )
}, ignoreNULL = TRUE, ignoreInit = TRUE)

analyze <- function(model="C") {
   R <- S$R %>%
      filter(P %in% unlist(str_split(S$Any$FORM$value[S$Any$FORM$column=="P"], ";"))) %>%
      filter(I %in% unlist(str_split(S$Any$FORM$value[S$Any$FORM$column=="I"], ";"))) %>%
      filter(C %in% unlist(str_split(S$Any$FORM$value[S$Any$FORM$column=="C"], ";"))) %>%
      filter(O %in% unlist(str_split(S$Any$FORM$value[S$Any$FORM$column=="O"], ";"))) %>%
      filter(TS %in% unlist(str_split(S$Any$FORM$value[S$Any$FORM$column=="TS"], ";")))
   if(length(unique(R$studyNUM)) < 4) {
      S$M <- tibble()
      r <- "Not enough studies yet for a Dependent Effects analysis."
      return(r)
   }
   # Get studyNames for R from extract table
   r <- recGet(S$db, "extract", c("studyNUM", "value"),
            tibble(s = c("studyNUM", " IN ", paste0("(", paste0(unique(R$studyNUM), collapse=","), ")")),
                   n = c("name", "=", "Trial")))
   studyName <- r$value
   names(studyName) <- as.character(r$studyNUM)
   R$studyName <- studyName[as.character(R$studyNUM)]

   # Get Industry Funding Bias
   r <- recGet(S$db, "extract", c("studyNUM", "value"),
            tibble(s = c("studyNUM", " IN ", paste0("(", paste0(unique(R$studyNUM), collapse=","), ")")),
                   n = c("name", "=", "TrialBiasFund")))
   BiasFund <- r$value
   names(BiasFund) <- as.character(r$studyNUM)
   R$BiasFund <- BiasFund[as.character(R$studyNUM)]

   # Get  Randomization and allocation concealment Bias
   r <- recGet(S$db, "extract", c("studyNUM", "value"),
            tibble(s = c("studyNUM", " IN ", paste0("(", paste0(unique(R$studyNUM), collapse=","), ")")),
                   n = c("name", "=", "TrialBiasHiding")))
   BiasHiding <- r$value
   names(BiasHiding) <- as.character(r$studyNUM)
   R$BiasHiding <- BiasHiding[as.character(R$studyNUM)]

   # Get Blinding of research personnel Bias
   r <- recGet(S$db, "extract", c("studyNUM", "value"),
            tibble(s = c("studyNUM", " IN ", paste0("(", paste0(unique(R$studyNUM), collapse=","), ")")),
                   n = c("name", "=", "TrialBiasStaff")))
   BiasStaff <- r$value
   names(BiasStaff) <- as.character(r$studyNUM)
   R$BiasStaff <- BiasStaff[as.character(R$studyNUM)]

   # Get Blinding of participants Bias
   r <- recGet(S$db, "extract", c("studyNUM", "value"),
            tibble(s = c("studyNUM", " IN ", paste0("(", paste0(unique(R$studyNUM), collapse=","), ")")),
                   n = c("name", "=", "TrialBiasSubj")))
   BiasSubj <- r$value
   names(BiasSubj) <- as.character(r$studyNUM)
   R$BiasSubj <- BiasSubj[as.character(R$studyNUM)]

   # Get Level of attrition and exclusions Bias
   r <- recGet(S$db, "extract", c("studyNUM", "value"),
            tibble(s = c("studyNUM", " IN ", paste0("(", paste0(unique(R$studyNUM), collapse=","), ")")),
                   n = c("name", "=", "TrialBiasAttr")))
   BiasAttr <- r$value
   names(BiasAttr) <- as.character(r$studyNUM)
   R$BiasAttr <- BiasAttr[as.character(R$studyNUM)]

   # Construct XName (what's different about this result in terms of PICOTS)
   keepP <- length(unique(R$P))>1
   keepI <- length(unique(R$I))>1
   keepC <- length(unique(R$C))>1
   keepO <- length(unique(R$O))>1
   keepTS <- length(unique(R$TS))>1
   R$XName <- rep("", nrow(R))
   for(i in 1:nrow(R)) {
      R$XName[i] <- paste0(c(R$P[i][keepP], R$I[i][keepI], R$C[i][keepC], R$O[i][keepO], R$TS[i][keepTS]), collapse="-")
   }
   # To sensibly sort the forest plot, we need the authors and publication years
   ay = recGet(S$db, "catalog", c("catalogID", "author", "Y"), tibble(x = c("catalogID", " IN ", paste0("(", paste0(unique(R$catalogID), collapse=","), ")"))))
   R$Y <-R$author <- rep("", nrow(R))
   for(cid in R$catalogID) {
      R[R$catalogID==cid, "Y"] <- ay[ay$catalogID==cid, "Y"]
      R[R$catalogID==cid, "author"] <- ay[ay$catalogID==cid, "author"]
   }
   R <- arrange(R, Y, author, XName)
 #  View(R)
   # R2 drops some columns from R
   R2 <- tibble(studyName = R$studyName, XName = R$XName, studyNUM=R$studyNUM,
                es = as.numeric(R$es), v = as.numeric(R$v), ci.lo = as.numeric(R$ci.lo), ci.hi=as.numeric(R$ci.hi))
#   View(R2)
   M <- robu(es ~ 1,
       data = R2,
       modelweights = "CORR",
#       studynum = studyNUM,
       studynum = as.factor(studyName),
       var.eff.size = v,
       small = TRUE)
      # },
      # "Cluster-robust hierarchical effects" = {
      #    M <- robu(es ~ 1,
      #        data = R2,
      #        modelweights = "HIER",
      #        studynum = studyNUM,
      #        var.eff.size = v,
      #        small = TRUE)
      #    Sens <- ""
      # }
   # )
#   M$study_orig_id <- 1:nrow(M$data)     # This is a hack to get forest.robu to put things in the arranged order
   digits=3
   effect <- M$reg_table
   effect$labels <- "ES of combined studies"
   colnames(effect) <- c("", "Est","SE", "t", "df", "p", "95% CI.L","95% CI.U", "Sig")
   if(is.nan(effect$df) || as.numeric(effect$df)<4) {
      S$M <- tibble()
      r <- "Not enough studies yet for a Dependent Effects analysis."
   } else {
      Sens <- paste0("\nSensitivity Analysis\n", tib2tab(sensitivity(M)))
      Sens <- str_replace(Sens, "X.Intercept.", "Effect Size")
      # switch(S$Any$FORM$value[S$Any$FORM$column=="type"],
      #    "Cluster-robust correlated effects" = {
      r <- paste0("\n\nDependent Effects Model\n(Robust variance estimation for correlated effects with small sample corrections)\n",
      "\nNumber of studies = ", M$N, "\n",
      "Number of outcomes = ", sum(M$k), " (min = ", min(M$k), ", mean = ", format(mean(M$k), digits = 3), ", median = ", stats::median(M$k), ", max = ", max(M$k), ")\n\n",
      "Rho = ", format(M$mod_info$rho, digits = digits), "\n",
      "I.sq = ", format(M$mod_info$I.2, digits = digits), "\n",
      "Tau.sq = ", format(M$mod_info$tau.sq, digits = digits), "\n\n",
      tib2tab(effect),
      "---\n",
      "Signif. codes: < .01 *** < .05 ** < .10 *\n",
      "---\n",
      Sens)
      #    },
      #    "Cluster-robust hierarchical effects" = {
      #       r <- paste0(paste0(M$mod_label, collapse=" "), "\n",
      #       "\nNumber of clusters = ", M$N, "\n",
      #       "Number of outcomes = ", sum(M$k), " (min = ", min(M$k), ", mean = ", format(mean(M$k), digits = 3), ", median = ", stats::median(M$k), ", max = ", max(M$k), ")\n\n",
      #       "Omega.sq = ", format(M$mod_info$omega.sq, digits = digits), "\n",
      #       "Tau.sq = ", format(M$mod_info$tau.sq, digits = digits), "\n\n",
      #       tib2tab(effect),
      #       "---\n",
      #       "Signif. codes: < .01 *** < .05 ** < .10 *\n",
      #       "---\n",
      #       Sens)
      #    }
      # )
     # View(M)
      S$M <<- M
      S$R2 <<- R2
      # These are for the data download csv and meta regression
      R$Dose      <- as.numeric(sapply(str_split(R$I, " "), function(x) x[1]))
      R$TimeSpan  <- as.numeric(sapply(str_split(R$TS, " "), function(x) x[1]))
      R$O      <- as.factor(R$O)
      R$Y      <- as.numeric(R$Y)
      R$nC     <- as.numeric(R$nC)
      R$nI     <- as.numeric(R$nI)
      R$es     <- as.numeric(R$es)
      R$v      <- as.numeric(R$v)
      R$ci.lo  <- as.numeric(R$ci.lo)
      R$ci.hi  <- as.numeric(R$ci.hi)
      # Table for meta-regression
      S$Rx <<- R %>% select(studyName, Y, Dose, O, TimeSpan, info, nC, nI, es, v, ci.lo, ci.hi)
      # Table for data download
         # for multiple lines in a cell, put quotes around the whole thing.
      S$Rp <<- R %>% select(studyName, Y, BiasFund, BiasHiding, BiasStaff, BiasSubj, BiasAttr,
                            Dose, O, TimeSpan, info, nC, nI)
   }
   return(r)
}

tib2tab <- function(tib, digits=3) {
   tib <- format(tib, trim = TRUE, digits = digits, nsmall=3, scientific = FALSE)
   return(tags$table(
            tags$thead(
               tags$tr(lapply(colnames(tib), function(x) tags$th(style="padding:0 6px 0 6px;", x)))),
            tags$tbody(
               apply(tib,1, function(x) {
                     tags$tr(lapply(x, function(y) {
                        tags$td(style="padding:0 6px 0 6px;", format(y, digits = digits))
                     }))
               })
            ))
   )
}

