# This is in a separate file because it's rarely used, but when it is, it's in two wildly different places
#    One is when initializing a new setup (sql-initialization.R)
#    The other is when initializing a new project (prjNew.R)


#  This function creates the SQL command to create a table, based on the given defintion
createTable = function(db, table, dict=table.definition.list) {
   dict.def <- dict[[table]]
   r = paste0("CREATE TABLE `", db, "`.`", table, "` (")
   for(i in 1:nrow(dict.def)) {
      name = paste0("`", dict.def$Name[i], "` ")
      type = paste0(dict.def$Type[i])
      if(dict.def$Size[i]!="") {
         type = paste0(type, "(", dict.def$Size[i], ")")
      }
      nosign = ""
      if(dict.def$NoSign[i]=="T") nosign = " UNSIGNED"
      nonull = " NOT NULL"                                     # Don't allow NULLs anywhere
      # nonull = ""
      # if(dict.def$NoNull[i]=="T") nonull = " NOT NULL"
      default=""                                               # This has to be defined ahead of time for switch()
      switch(dict.def$Default[i],
         AUTOINC = default <- " AUTO_INCREMENT",
         s = default <- " DEFAULT ''",
         z = default <- " DEFAULT 0",
         x = default <- "",
         UPDATE = stop("Use sTime() and bTime() and store times in DB as strings."),
         { stop("Undefined default in createTable()") }
      )
      key = ""
      if(dict.def$Key[i]!="" && dict.def$Key[i]!="COMBO") key = paste0(" ", dict.def$Key[i])
      r = paste0(r, name, type, nosign, nonull, default, key, ", ")
   }
   if(any(dict.def$Key=="COMBO")) {
      keys = dict.def$Name[dict.def$Key=="COMBO"]
      k=""
      for(i in 1:length(keys)) {
         k = paste0(k, "`", keys[i], "`,")
      }
      k = str_sub(k,1,nchar(k)-1)
      r = paste0(r, "UNIQUE (", k, "), ")
   }
   return(paste0(str_sub(r,1,nchar(r)-2),");"))
}

# createTable("user", table.definition.list)
