### om_skeleton mysql-startup.R
### Tom Weishaar - Nov 2017 - v0.1
### Skeleton for multi-page, multi-user web site in Shiny, with user authentication

### FUNCTIONS

### Very-low level functions - you'll never need to call these directly.

#  This function builds a 2-row (empty) tibble based on the given definition.
newRec = function(table, dict=table.definition.list) {
   dict.def <- dict[[table]]
   r = list()
   for(i in 1:nrow(dict.def)) {
      switch(dict.def$Type[i],                        # see https://dev.mysql.com/doc/refman/5.5/en/create-table.html
         BIT = {r[dict.def$Name[i]] <- 0L},
         TINYINT = {r[dict.def$Name[i]] <- 0L},
         SMALLINT = {r[dict.def$Name[i]] <- 0L},
         MEDIUMINT = {r[dict.def$Name[i]] <- 0L},
         INT = {r[dict.def$Name[i]] <- 0L},
         BIGINT = {r[dict.def$Name[i]] <- 0L},
         REAL = {r[dict.def$Name[i]] <- 0},
         DOUBLE = {r[dict.def$Name[i]] <- 0},
         FLOAT = {r[dict.def$Name[i]] <- 0},
         DECIMAL = {r[dict.def$Name[i]] <- 0},
         NUMERIC = {r[dict.def$Name[i]] <- 0},
         CHAR = {r[dict.def$Name[i]] <- ""},
         VARCHAR = {r[dict.def$Name[i]] <- ""},
         BINARY = {r[dict.def$Name[i]] <- ""},
         VARBINARY = {r[dict.def$Name[i]] <- ""},
         TINYBLOB = {r[dict.def$Name[i]] <- as.character(NA)},      # In SQL, can't have a default value
         BLOB = {r[dict.def$Name[i]] <- as.character(NA)},          # In SQL, can't have a default value
         MEDIUMBLOB = {r[dict.def$Name[i]] <- as.character(NA)},    # In SQL, can't have a default value
         LONGBLOB = {r[dict.def$Name[i]] <- as.character(NA)},      # In SQL, can't have a default value
         TINYTEXT = {r[dict.def$Name[i]] <- as.character(NA)},      # In SQL, can't have a default value
         TEXT = {r[dict.def$Name[i]] <- as.character(NA)},          # In SQL, can't have a default value
         MEDIUMTEXT = {r[dict.def$Name[i]] <- as.character(NA)},    # In SQL, can't have a default value
         LONGTEXT = {r[dict.def$Name[i]] <- as.character(NA)},      # In SQL, can't have a default value
         ENUM = {r[dict.def$Name[i]] <- ""},
         SET = {r[dict.def$Name[i]] <- ""},
         DATE = {stop("Use sTime(), which is a string")},
         TIME = {stop("Use sTime(), which is a string")},
         TIMESTAMP = {stop("Use sTime(), which is a string")},
         DATETIME = {stop("Use sTime(), which is a string")},
         YEAR = {stop("Use sTime(), which is a string")},
         # DATE = {r[dict.def$Name[i]] <- as_datetime(NA)},
         # TIME = {r[dict.def$Name[i]] <- as_datetime(NA)},
         # TIMESTAMP = {r[dict.def$Name[i]] <- as_datetime(NA)},
         # DATETIME = {r[dict.def$Name[i]] <- as_datetime(NA)},
         # YEAR = {r[dict.def$Name[i]] <- as_datetime(NA)},
         stop("Undefined MySQL data type in newRec()")
      )
   }
   r = as_tibble(r)
   r = rbind(r, r)          # make two rows
   naVec = is.na(r[1,])     # find the NAs
   r[2,naVec] = ""          # in row 2, make the NAs ""; this will force an update on the first save.
   return(r)
}

# forms `db`.`table`
dbt = function(db, table, pool=shiny.pool) {
   return(paste0(dbQuoteIdentifier(pool, db), ".", dbQuoteIdentifier(pool, table)))
}
# Turns a tibble into quoted, scrubbed, and separated name=value SET pairs for recSave()
QsetQ = function(Qset, pool) {
   q = character(0)
   n = colnames(Qset)
   for(i in 1:(ncol(Qset))) {
      q[i] = paste0(dbQuoteIdentifier(pool, n[i]), "=", dbQuoteString(pool, as.character(Qset[2,i])))
   }
   return(paste0(q, collapse = ", "))        # First paste creates the pairs, second the SET string
}
# properly converts WHERE tibble into wherePairs for Queries
wherez = function(WHERE, pool) {   # WHERE is a tibble
   w = ""
   for(i in 1:ncol(WHERE)) {                                  # Need all this to keep from putting quotes on numbers
      if(WHERE[[2,1]]!=" IN " && suppressWarnings(is.na(as.numeric(WHERE[[3,i]])))) { # An NA means it's a string
         v = dbQuoteString(pool, WHERE[[3,i]])
      } else {
         v = WHERE[[3,i]]
      }
      w[i] = paste0(dbQuoteIdentifier(pool, WHERE[[1,i]]), WHERE[[2,i]], v)
   }
   return(paste0(w, collapse = " AND "))
}

# This is still a low-level function; use the functions that begin with a table name, eg. userGet()
#    SELECT is a vector of field names
#    WHERE is a 3-row tibble of fields, operators, and values
recGet = function(db, table, SELECT, WHERE, pool=shiny.pool) {
# Do some routing
   return2 = FALSE
   if(SELECT[1]=="") {                                             # return a blank record for editing
      return(newRec(table))                                        #    (all columns, 2 duplicate rows)
   }
   if(SELECT[1]=="*") {                                            # return all columns, 1 row per record
      SELECT="*"                                                   # trim select vector to length 1
   }
   if(SELECT[1]=="**") {                                           # return a record for editing
      return2 = TRUE
      SELECT="*"                                                   #    (all columns, 2 duplicate rows)
   }
# Do some error checking
   if(debugON) {
      validFields = table.definition.list[[table]]$Name               # Valid fields for this table
      if(SELECT[1]!="*") {
         selectErrors = !(SELECT %in% validFields)                    # Any invalid fields in SELECT?
      } else {
         selectErrors = FALSE
      }
      whereErrors = !(as.character(WHERE[1,]) %in% validFields)                     # Any invalid fields in WHERE?
      opErrors = !(as.character(WHERE[2,]) %in% c("=", "!=", "<", ">", "<=", ">=", " IN ")) # Any invalid operators in WHERE?
      if(any(c(selectErrors, whereErrors, opErrors))) {
         msg = "In reCGet():\n"
         if(any(selectErrors)) {
            msg = paste0(msg, "   SELECT has unknown variables: ", paste0(SELECT[selectErrors], collapse=", "), "\n")
         }
         if(any(whereErrors)) {
            msg = paste0(msg, "   WHERE has unknown variables: ", paste0(as.character(WHERE[1,])[whereErrors], collapse=", "), "\n")
         }
         if(any(opErrors)) {
            msg = paste0(msg, "   WHERE has unknown operators: ", paste0(as.character(WHERE[2,])[opErrors], collapse=", "), "\n")
         }
         stop(msg)
      }
   }
# Turn SELECT into selects
   if(SELECT[1]!="*") {                                    # don't quote "*"
      selects = dbQuoteIdentifier(pool, SELECT)            #   but quote everything else (SELECT is a vector)
      selects = paste0(selects, collapse=",")              #   then collapse the vector into a single string
   } else {
      selects = "*"
   }
# Turn WHERE into wheres
   if(WHERE[2,1]!=" IN ") {                                   # Skip this with IN, assuming deleted check already happened
      if(!("deleted" %in% as.character(WHERE[1,]))) {         # if no "deleted" field, add one to get active records only
         WHERE = cbind(WHERE, tibble(c("deleted", "=", "0"))) #   (To get deleted or ALL records, you need to add your own
      }                                                       #    deleted column to the WHERE tibble.)
   }
   wherePairs = wherez(WHERE, pool)
# Execute QUERY
   QUERY = paste0("SELECT ", selects, " FROM ", dbt(db, table, pool), " WHERE ", wherePairs, ";")
print(QUERY)
   dbLink <- poolCheckout(pool)                            # get a dbLink from the pool
   on.exit(poolReturn(dbLink), add = TRUE)                 # return it when done, even if there's an error
   r = dbGetQuery(dbLink, QUERY)                           # perform SQL Query
# Return Data or New Empty Record
   if(nrow(r)>0)  {                                        # r is number of rows found
      r = as_tibble(r)                                     #    length>0, return tibble, not dataframe
      if(return2) {                                        #    but if it's one row and the request was
         if(nrow(r)!=1) { stop("Your query found more than 1 row.")}    # This shouldn't happen after debugging.
         r[2,]=r[1,]                                       #    for two, make it so.
      }
   } else {                                                # Else, nothing was found. so
      r = newRec(table)                                    #    return a new, empty record without an error,
      if(!return2) {                                       #    but note that newRec returns two rows;
         r = r[2,]                                         #    if there's only supposed to be one row,
      }                                                    #    return rwo 2 as row 1 could have NAs.
      if(SELECT[1]!="*") {                                 # Just the columns the user asked for, though
         r = r[SELECT]
      }
   }
   return(r)
}

 # recGet("om$prime", "user", SELECT="")
 # recGet("om$prime", "page", "*", WHERE=tibble(c("pageName", "=", "profile")))
 # recGet("om$prime", "user", "**", WHERE=tibble(c("userName", "=", "admin")))
 # recGet("om$prime", "user", c("userID", "userName", "sessionID"), WHERE=tibble(c("userName", "=", "admin")))

### High-level functions

# Our table constraints are that the first field of each table be an integer, auto-increment, primary key
#    and be named `table`ID. (We use this to discover the table name and refer to the row.) In addition,
#    all tables must have the verNum(integer), verUser(string), verTime(time), clash(0/1). and clashFacts(text)
#    fields. These fields are sacred to recSave(), don't edit or delete them yourself.

### Save a record (code only supports saving one record at a time)
# When saving a record, you only need to provide a SET, which is a tibble with 2 rows. Row 1 has the
#   data before editing, row 2 the edited data. Typically the SET includes all record columns, but
#   in any case the first column MUST be `table`ID and there must be verNum and verUser columns. The code decides
#   whether to INSERT (new record) or UPDATE (existing record) based on whether the ID is zero or not.
#
# Before either an INSERT or UPDATE, we create a Qset variable from SET that drops fields that haven't
#   changed.
#
# If there is an update clash, we save all the non-clashing fields, set the clash variable, and store what
#   clashed in clashFacts. The record cannot be saved again until the clash is cleared (clash is reset to 0).
#
# At the end we SELECT and pass back all the columns of the record as it currently exists on the server,
#   except in the case of an existing update clash, when the SET is returned.

recSaveR <- function(SET, verUser="Admin", db="om$prime", pool=shiny.pool) {
   modalMsg=list(title="", text="")                                        # return these to server for activation
 # Using the name of the ID field, determine the table to save this in
   setFields = colnames(SET)                                               # Names of columns to be saved
   table = str_sub(setFields[1], 1, nchar(setFields[1])-2)                 # Name of the table
 # Do some error checking before save
   sacredFields = c("verNum", "verUser", "verTime", "clash", "clashFacts") # Never-drop columns
   if(debugON) {
      validFields = table.definition.list[[table]]$Name                    # Names of valid fields for this table
      if(str_sub(setFields[1], nchar(setFields[1])-1, -1)!="ID") {
         stop("In recSave(), the first field of the SET must be `table`ID.")
      }
      fieldErrors = !(setFields %in% validFields)                          # Are there invalid fields in the SET?
      if(any(fieldErrors)) {
         stop(paste0("In recSave(), the SET has unknown variables: ", paste0(setFields[fieldErrors], collapse=", "), "."))
      }
      sacredFieldErrors = !(sacredFields %in% setFields)                   # Are any sacred fields missing?
      if(any(sacredFieldErrors)) {
         stop(paste0("In recSave(), these sacredFields are missing: ", paste0(sacredFields[sacredFieldErrors], collapse=", "), "."))
      }
      if(nrow(SET)!=2) {                                                   # Exactly 2 rows?
         stop("In recSave, the SET doesn't have exactly 2 rows.")
      }
      if(SET$deleted[1] && SET$deleted[2]) {                               # Need to check both or can't delete/undelete
         stop("In recSave(), saving of deleted records isn't allowed. If necessary, activate-edit-delete")
      }
   }
   # if(SET$clash[2]) {                                                      # Is there an existing update clash?
   #    modalMsg$title <- "Database Issue!"
   #    modalMsg$text <- "Sorry, update clash must be resolved before this record can be updated again."
   #    return(list(r=SET, modalMsg=modalMsg))
   # }
 # deal with versioning issues
   SET$verNum[2] = SET$verNum[1] + 1               # inc version number in row 2
   SET$verUser[2] = verUser                        # verUser is a function parameter
   SET$verTime[2] = sTime()
   if(SET$clash[1]==1 && SET$clash[2]==0) {        # If clash has been cleared, erase clashFacts
      SET$clashFacts[2] <- ""
   }
 # Convert SET tibble into minimized Qset
   # Anything that's true here will get dropped
   dropSet = as.logical(is.na(SET[2,]))            # Drop columns where row 2 is NA; never insert NA/NULL into db
   dropSet = dropSet | naf(SET[1,]==SET[2,])       # Drop any columns that are unchanged but keep if row 1 is NA
   dropSet = dropSet & !(colnames(SET) %in% sacredFields)   # Force-keep the sacred columns to see what they are on the server.
   Qset = SET[, !dropSet]                          # flip logical vector to drop what was TRUE
   dbError = FALSE
 # pool checkout/checkin
   dbLink <- poolCheckout(pool)                 # get a dbLink from the pool
   on.exit(poolReturn(dbLink), add = TRUE)      # return dbLink when done, even if there's an error
   if(SET[[2,1]]==0) {                          # INSERT - `table`ID==0, so this is a new col
 # INSERT
      QUERY = paste0("INSERT INTO ", dbt(db, table, pool), " SET ", QsetQ(Qset, pool), ";")
      r = dbExecute(dbLink, QUERY)              # r is an integer
      if(r==1) {                                # number of rows updated
         r = dbGetQuery(dbLink, "SELECT LAST_INSERT_ID()")    # r is a tibble!
         SET[[2,1]] = as.integer(r[[1,1]])                    # Save r as.integer
      } else {
         dbError = TRUE
         modalMsg$title <- "Database Issue!"
         modalMsg$text <- "Sorry, there was an error while trying to insert that record into the database."
      }
   } else {                                     # UPDATE - `table`ID != 0, so this col exists
 # UPDATE
      wherePairs = wherez(tibble(c(setFields[1], "=", SET[[2,1]])), pool) # For this we need only a rowID-based WHERE
      selects = paste0(dbQuoteIdentifier(pool, colnames(Qset)), collapse=",")  # collapse vector to single, quoted string
      sQUERY = paste0("SELECT ", selects, " FROM ", dbt(db, table, pool), " WHERE ", wherePairs, ";")
      uQUERY = paste0("UPDATE ", dbt(db, table, pool), " SET ", QsetQ(Qset, pool), " WHERE ", wherePairs, ";")
#print(uQUERY)
 # SQL Transaction Start: See https://cran.r-project.org/web/packages/pool/pool.pdf
      r = dbBegin(dbLink)   #, "START TRANSACTION;")     # Transaction because there are multiple steps here
      Qset[3,] = dbGetQuery(dbLink, sQUERY)              # Get what's on the server right now for the Qset fields
      if(Qset[[3,"verNum"]] == Qset[[1,"verNum"]]) {         # if verNums match...
         r = dbExecute(dbLink, uQUERY)                       #    no intervening updates; let's do it!
         if(r==1) {                                          # was 1 row updated?
            r = dbCommit(dbLink)  #, "COMMIT;")              #    Yes, finish transaction
    # SQL Transaction End
         } else {                                            # Shouldn't really happen, but..
            r = dbRollback(dbLink)  #, "ROLLBACK;");         #    release Transaction lock...
    # SQL Transaction End
            dbError = TRUE
            modalMsg$title <- "Database Issue!"
            modalMsg$text <- "Sorry, there was an error while trying to update that record in the database."
         }
      } else {                                     # Now the hard part - verNums did not match
# At this point Qset is a 3-row tibble containing the columns that were edited plus the sacredFields.
#   Row 1 is the original data, row 2 is the edited data, and row 3 is the data on the server, which we now
#   know is more recent than row 1. What we want to do is:
#      * Drop any columns where rows 2 and 3 are the same (the server already has the edit).
#      * In the remaining columns, if rows 1 and 3 are the same, row 2 has something better.
#      *    if rows 1 and 2 are the same, row 3 has something better.
#      * Finally, in what's left, all three are different and we have an update conflict.

            # Anything that's true here will get dropped
         sacredFieldsVec = (colnames(Qset) %in% sacredFields) # which columns are sacred?
         dropSet = as.vector(Qset[2,]==Qset[3,])              # drop row 2 == row 3
         dropSet = dropSet & !sacredFieldsVec                 # Force-keep the sacred columns
         Qset = Qset[, !dropSet]                              # Flip and drop
         if(ncol(Qset)>sum(sacredFieldsVec)) {                # Anything left besides sacred columns?
            msg=rep("",ncol(Qset))                            # Create msg vector
            cell=rep("",ncol(Qset))                           # Create cell vector
            Qnames = colnames(Qset)                           # Get names of remaining columns
            for(i in 1:ncol(Qset)) {                          # Not lapply() because we need column name
               if(Qset[1,i]!=Qset[3,i] &&                     # Note the clash in this column
                  Qnames[i]!="verNum" &&                      #   but don't mention verNum/verUser clashes here
                  Qnames[i]!="verUser") {
                  msg[i] =  paste0("<li>In ", Qnames[i], " you entered <b>", Qset[2,i],
                                   "</b> but ", Qset[3,"verUser"], " said <b>", Qset[3,i], "</b></li>")
                  cell[i] = paste0("<li>In ", Qnames[i], " ", Qset[2,"verUser"], " said <b>", Qset[2,i],
                                   "</b> but ", Qset[3,"verUser"], " said <b>", Qset[3,i], "</b></li>")
               }
            }
            msg = paste0(msg, collapse="")                    # Collapse messages; nchar will = 0 if no clashes
            if(nchar(msg)>0) {                                # Any conflicts?
               if(nchar(Qset[3,"clashFacts"])>0) {            # Just in case there was an existing clash
                  Pclash <- Qset[3,"clashFacts"]              # Keep what was there
               } else {
                  Pclash = ""
               }
               Qset[2,"clash"] <- 1                           # Set the clash flag
               Qset[2,"clashFacts"] <- paste0(Pclash,         # Remember the clashFacts
                        "<p>At ", sTime(), " an update by ", Qset[2,"verUser"], " clashed with a ",
                        Qset[3,"verTime"], " update by ", Qset[3,"verUser"],
                        " in the following ways:<ul>", paste0(cell, collapse = ""),
                        "</ul>Clashing fields now have the more recent update, but these may be incorrect.</p>")
            }
            uQUERY = paste0("UPDATE ", dbt(db, table, pool), " SET ", QsetQ(Qset, pool), " WHERE ", wherePairs, ";")
            r = dbExecute(dbLink, uQUERY)
            if(r==1) {                                         # was 1 row updated?
               r = dbCommit(dbLink)  #, "COMMIT;")             #    Yes, finish transaction
    # SQL Transaction End
               if(nchar(msg)) {                                # Skip this if conflict was only verNum or verUser
                  modalMsg$title <- "Database Issue!"
                  modalMsg$text <- paste0(Qset$verUser[3], " was editing this record at the ",
                        "same time you were and there were the following conflicts:<ul>",
                        paste0(msg, collapse=""), "</ul>Your changes overwrote ", Qset$verUser[3],
                        "'s changes, but this may be incorrect.")
               }
            } else {
               r = dbRollback(dbLink)  #, "ROLLBACK;");         #    release Transaction lock...
    # SQL Transaction End
               dbError = TRUE
               modalMsg$title <- "Database Issue!"
               modalMsg$text <- "Sorry, there was an error while trying to update that record in the database."
            } # end of clash handling update
         } # else no actual clashes after dropping columns where Qset[2,]==Qset[3,]
      } # end of clash handling
   } # end of UPDATE section
   if(dbError) {
      r = SET
   } else {
      WHERE = tibble(c(setFields[1], "=", SET[[2,1]])) # Need to redo this in case we INSERTed.
      r = recGet(db, table, "**", WHERE, pool)         # Return what's on the server to get accurate verTime, etc.
   }
   return(list(r=r, modalMsg=modalMsg))
}

# u = userGet("userName", "Admin")
# u[[1,"nameFirst"]] = "Tom"
# recSave(u, "admin")
# u = userGet()
# u$userName = "TomW"
# u$regDate = sTime()
# u$sessionID = u$userName   # generate_id() would be better if it works here
# recSave(u, "Admin")

### These are the functions for getting a tibble from a table. There is special function for each table.
# When getting records, there are three possibilities.
#   * You want a new record for editing, which should include all columns and two duplicate rows.
#      * In this case, use SELECT="" (that's also the default for no parameters at all).
#      * Returns a new empty record with all columns and 2 duplicate rows.
#   * You want an existing record for editing, which should include all columns and two duplicate rows.
#      * In this case, use SELECT = "**" and WHERE to describe the desired record.
#      * Returns the exisiting record with all columns and 2 duplicate rows.
#   * You want records with selected columns, but will not edit the data.
#      * In this case, SELECT is a string vector of desired columns, but just "*" is all columns
#      * WHERE describes the desired records, however:
#         * no WHERE returns all active records
#         * to get deleted records, you MUST include "deleted=0" (for deleted only) or "deleted>=0"
#         *   (for all records) in WHERE; otherwise the code defaults to Active records only, no Deleted.
#      * Returns a tibble of SELECTed columns and 1 row for each selected record.
#      * If nothing is found, returns an empty row with selected fields.

# Key fields are ID, userName, and sessionID
userGet = function(SELECT="", WHERE=tibble(c("userID", ">", "0")), pool=shiny.pool) {
   return(recGet("om$prime", "user", SELECT, WHERE, pool=pool))
}

# WHERE=tibble(userName=c("=", "admin"), email=c("=", "tom@omega-ratio.com"))
# SELECT=c("userID", "userName", "email")

# Key fields are ID and pageName
pageGet = function(SELECT="", WHERE=tibble(c("pageID", ">", "0")), pool=shiny.pool) {
   return(recGet("om$prime", "page", SELECT, WHERE, pool=pool))
}

# Key fields are ID and projectName
projectGet = function(SELECT="", WHERE=tibble(c("projectID", ">", "0")), pool=shiny.pool) {
   return(recGet("om$prime", "project", SELECT, WHERE, pool=pool))
}

# Key fields are ID and the userName/projectID combo
membershipGet = function(SELECT="", WHERE=tibble(c("membershipID", ">", "0")), pool=shiny.pool) {
   return(recGet("om$prime", "membership", SELECT, WHERE, pool=pool))
}

# Key field is settingsID
settingsGet = function(SELECT="", WHERE=tibble(c("settingsID", ">", "0")), pool=shiny.pool) {
   return(recGet("om$prime", "settings", SELECT, WHERE, pool=pool))
}

# userGet()
# userGet("*")  # This one has bad syntax (no WHERE) and will throw an error
# userGet("*", tibble(c("userID", "=", 1)))
# userGet("**", tibble(c("userID", "=", 1)))
# userGet(c("userID", "email"), tibble(c("userID", "=", 1)))



### end functions

# Load the list of table defintions. This is created by the STAND ALONE file.
table.definition.list = readRDS(file="table.definition.list.RDS")

