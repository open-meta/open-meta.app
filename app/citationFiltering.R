### open-meta.app citationFiltering.R
### Tom Weishaar - v0.1 - October 2018

### This code is needed by both Review.R and Extract.R

# Filtering Globals
S$FIL$FORM <- imGetFORM("Form-filterCites", "om$prime")
# S$PKR$filteredIDs used by prevCite() and nextCite()

citesFilter <- function(DB, TABLE, tableID, WHERE) {   # these aren't actually used in this filter, but they are passed...
   extract <- ifelse(WHERE=="extract", TRUE, FALSE)
   f <- list()
   for(i in 1:nrow(S$FIL$FORM)) {                      # get user's entries
      S$FIL$FORM$value[i] <<- str_trim(stripHTML(input[[S$FIL$FORM$id[i]]]))
      f[[S$FIL$FORM$column[i]]] <- S$FIL$FORM$value[i]
   }
   f$allRnot  <- ifelse(is.null(input$allRnot), "all", input$allRnot)   # These are globals so we remember
   f$notRev   <- ifelse(is.null(input$notRev), TRUE, input$notRev)      #   the settings from search to search
   f$s1Fail   <- ifelse(is.null(input$s1Fail), TRUE, input$s1Fail)
   f$s1Pass   <- ifelse(is.null(input$s1Pass), TRUE, input$s1Pass)
   if(f$author!="")  { WHERE$author=c("author", "LIKE", paste0("%", author, "%"))}
   if(f$year!="")    { WHERE$year=c("Y", "LIKE", paste0("%", year, "%"))}
   if(f$journal!="") { WHERE$journal=c("journal", "LIKE", paste0("%", journal, "%"))}
   chex <- sum(c(f$notRev, f$s1Fail, f$s1Pass))
   if(chex==0) {
      f$notRev <- TRUE       # If nothing is checked, nothing would be returned. Since there are no edge
      f$s1Fail <- TRUE       #   cases where this makes sense, the only possibility is that the user wanted
      f$s1Pass <- TRUE       #   everything, which is what we're changing the checkboxes to.
   }
   WHERE = tibble(dupOf=c("dupOf", "=", 0))                                  # Set up WHERE
   if(extract) {
      WHERE$rBest = c("reviewBest", ">", 1)
   } else {
      if(f$allRnot=="all") {                                             # Do this after QUERY for My Reviews
         if(chex==1) {                                                       # If chex=0 or 3, there's nothing to filter
            if(f$notRev) { WHERE$rBest = c("reviewBest", "=", 0) }
            if(f$s1Fail) { WHERE$rBest = c("reviewBest", "=", 1) }
            if(f$s1Pass) { WHERE$rBest = c("reviewBest", ">", 1) }       # could be 2-3-4
         }
         if(chex==2) {
            if(f$notRev && f$s1Fail) { WHERE$rBest = c("reviewBest", "<=", 1) } # Everything but any kind of pass
            if(f$notRev && f$s1Pass) { WHERE$rBest = c("reviewBest", "!=", 1) } # Everything but Stage 1 fails
            if(f$s1Fail && f$s1Pass) { WHERE$rBest = c("reviewBest", ">", 0)  } # Everything but no review
         }
      }
   }
### Special SQL Request to OR title and abstact
   wherePairs = wherez(WHERE, shiny.pool)
   if(f$abstract!="") {                                               # all this for an OR
      qabs = dbQuoteString(shiny.pool, paste0("%", f$abstract, "%"))
      wherePairs = paste0(wherePairs, " AND (`abstract` LIKE ", qabs, " OR `title` LIKE ", qabs, ")")
   }
   selects = dbQuoteIdentifier(shiny.pool, c("catalogID", "reviewBest", "Y", "author"))
   selects = paste0(selects, collapse=",")
   QUERY = paste0("SELECT ", selects, " FROM ", dbt(S$db, "catalog", shiny.pool), " WHERE ", wherePairs, ";")
   r <- as.tibble(dbGetQuery(shiny.pool, QUERY))                      # perform raw SQL Query
   r <- r %>% arrange(Y, author) %>% select(catalogID, reviewBest)    # We sort twice, once here for entire vector of filteredIDs
###
   if(f$allRnot=="my") {                                              # Skip this for All Reviews
      myReviews <- recGet(S$db, "review", c("catalogID", "decision"), tibble(c("verUser", "=", S$U$userName)))
      myFailIDs <- as.integer(myReviews$catalogID[myReviews$decision==1])
      myPassIDs <- as.integer(myReviews$catalogID[myReviews$decision>1])  # Can be 2-3-4
      filteredIDs <- as.integer(r$catalogID)
      if(chex==1) {
         if(f$notRev) { Keepers <- !(filteredIDs %in% c(myFailIDs, myPassIDs)) }
         if(f$s1Fail) { Keepers <- filteredIDs %in% c(myFailIDs) }
         if(f$s1Pass) { Keepers <- filteredIDs %in% c(myPassIDs) }
      }
      if(chex==2) {
         if(f$notRev && f$s1Fail) { Keepers <- !(filteredIDs %in% c(myPassIDs)) }
         if(f$notRev && f$s1Pass) { Keepers <- !(filteredIDs %in% c(myFailIDs)) }
         if(f$s1Fail && f$s1Pass) { Keepers <- (filteredIDs %in% c(myFailIDs, myPassIDs)) }
      }
      if(chex==0 || chex==3) {
         Keepers <- TRUE
      }
      r <- r[Keepers,]
   }
   S$PKR$filteredIDs <<- as.integer(r$catalogID)                    # prevCite(), nextCite() needs this
   return(r)
}

citesHead <- function(r) {
   return(
      HTML0("<span style='font-size: 1.15rem; color:#fff;'>",
         format(length(r$reviewBest), big.mark = ","), " results; ",
         format(sum(r$reviewBest==0), big.mark = ","), " not reviewed; ",
         format(sum(r$reviewBest==1), big.mark = ","), " failed; ",
         format(sum(r$reviewBest==2), big.mark = ","), " passed</span><br>")
   )
}
