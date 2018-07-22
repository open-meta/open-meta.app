### open-meta STAND-ALONE-table-definitions.R
### Tom Weishaar - Jan 2018 - v0.2
### This is a stand-alone file for creating a list of all table defintions.
###    This list is saved to a file and loaded during app startup.
###    The list is used to create SQL tables and dataframes that match, so they can be easily converted back and forth.

# Constraints:
# The first field in each table must concatenate the table name and "ID", as in "pageID", be AUTOINC and PRIMARY KEY.
# F1  = tibble(Name="pageID",     Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
# The (last) six fields in each table must be:
# Fn  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
# Fn  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
# Fn  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
# Fn  = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
# Fn  = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
# Fn  = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")

# In general, the system handles update conficts by setting clash and recording the actual conflicts in clashFacts.

# DATES and TIMES are always in UTC as 23-character strings in the format: "2018-01-01 02:46:53 UTC".
#   These strings are always created in R and any time math is done in R.

# TINYINT UNSIGNED    0 - 255
# SMALLINT UNSIGNED   0 - 65,535          BLOB or TEXT
# MEDIUMINT UNSIGNED  0 - 16,777,215      MEDIUMBLOB or MEDIUMTEXT
# INT UNSIGNED        0 - 4,294,967,295   LONGBLOB OR LONGTEXT

# Default: AUTOINC, s=string, z=numeric, x=no default (requred for TEXT, BLOB, etc.)

library(tibble)

### om$prime tables
# page
F1  = tibble(Name="pageID",     Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="pageName",   Type="VARCHAR",   Size="20",  NoSign="",  NoNull="T", Default="s",       Key="UNIQUE")
F3  = tibble(Name="spReq",      Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F4  = tibble(Name="pageType",   Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F5  = tibble(Name="pageText",   Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F6  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F7  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F8  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F9  = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F10 = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F11 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
page=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11)

# user
F1  = tibble(Name="userID",     Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="userName",   Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="UNIQUE")
F3  = tibble(Name="hashedPW",   Type="CHAR",      Size="60",  NoSign="",  NoNull="T", Default="s",       Key="")
F4  = tibble(Name="email",      Type="VARCHAR",   Size="254", NoSign="",  NoNull="T", Default="s",       Key="")
F5  = tibble(Name="emailOK",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F6  = tibble(Name="sPowers",    Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F7  = tibble(Name="sessionID",  Type="CHAR",      Size="16",  NoSign="",  NoNull="T", Default="s",       Key="UNIQUE")
F8  = tibble(Name="regDate",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F9  = tibble(Name="evDate",     Type="VARCHAR",   Size="23",  NoSign="",  NoNull="",  Default="s",       Key="")
F10 = tibble(Name="loginDate",  Type="VARCHAR",   Size="23",  NoSign="",  NoNull="",  Default="s",       Key="")
F11 = tibble(Name="loginTries", Type="TINYINT",   Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F12 = tibble(Name="namePrefix", Type="VARCHAR",   Size="15",  NoSign="",  NoNull="",  Default="s",       Key="")
F13 = tibble(Name="nameFirst",  Type="VARCHAR",   Size="50",  NoSign="",  NoNull="",  Default="s",       Key="")
F14 = tibble(Name="nameMiddle", Type="VARCHAR",   Size="50",  NoSign="",  NoNull="",  Default="s",       Key="")
F15 = tibble(Name="nameLast",   Type="VARCHAR",   Size="50",  NoSign="",  NoNull="",  Default="s",       Key="")
F16 = tibble(Name="nameSuffix", Type="VARCHAR",   Size="15",  NoSign="",  NoNull="",  Default="s",       Key="")
F17 = tibble(Name="workingOn",  Type="MEDIUMINT", Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F18 = tibble(Name="rperPage",   Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F19 = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F20 = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F21 = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F22 = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F23 = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F24 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
user=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21,F22,F23,F24)

# project
F1  = tibble(Name="projectID",  Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="projectName",Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="UNIQUE")
F3  = tibble(Name="status",     Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F4  = tibble(Name="privacy",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F5  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F6  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F7  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F8  = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F9  = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F10 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
project=rbind(F1,F2,F3, F4,F5,F6,F7,F8,F9,F10)

# membership
F1  = tibble(Name="membershipID",Type="INT",      Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="userID",     Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="z",       Key="COMBO")
F3  = tibble(Name="projectID",  Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="z",       Key="COMBO")
F4  = tibble(Name="role",       Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F5  = tibble(Name="contact",    Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F6  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F7  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F8  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F9  = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F10 = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F11 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
membership=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11)

# protoHelp
F1  = tibble(Name="protoHelpID",Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="order",      Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="UNIQUE")
F3  = tibble(Name="title",      Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="UNIQUE")
F4  = tibble(Name="helpText",   Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F5  = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F6  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F7  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F8  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F9  = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F10 = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F11 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
protoHelp=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11)

### om$nnn tables

# protocol
F1  = tibble(Name="protocolID", Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="order",      Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="UNIQUE")
F3  = tibble(Name="text",       Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F4  = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F5  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F6  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F7  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F8  = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F9  = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F10 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
protocol=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10)

# settings
F1  = tibble(Name="settingsID", Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="name",       Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F3  = tibble(Name="value",      Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F4  = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F5  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F6  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F7  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F8  = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F9  = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F10 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
settings=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10)

# search
F1  = tibble(Name="searchID",   Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="searchName", Type="VARCHAR",   Size="254", NoSign="",  NoNull="",  Default="s",       Key="")
F3  = tibble(Name="status",     Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F4  = tibble(Name="database",   Type="VARCHAR",   Size="60",  NoSign="",  NoNull="",  Default="s",       Key="")
F5  = tibble(Name="otherDB",    Type="VARCHAR",   Size="60",  NoSign="",  NoNull="",  Default="s",       Key="")
F6  = tibble(Name="beginDate",  Type="VARCHAR",   Size="23",  NoSign="",  NoNull="",  Default="s",       Key="")
F7  = tibble(Name="endDate",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="",  Default="s",       Key="")
F8  = tibble(Name="terms",      Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F9  = tibble(Name="query",      Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F10 = tibble(Name="CFchosen",   Type="VARCHAR",   Size="30",  NoSign="",  NoNull="",  Default="s",       Key="")
F11 = tibble(Name="CFactual",   Type="VARCHAR",   Size="30",  NoSign="",  NoNull="",  Default="s",       Key="")
F12 = tibble(Name="citeCount",  Type="INT",       Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F13 = tibble(Name="absCount",   Type="VARCHAR",   Size="20",  NoSign="",  NoNull="",  Default="s",       Key="")
F14 = tibble(Name="pmidCount",  Type="VARCHAR",   Size="20",  NoSign="",  NoNull="",  Default="s",       Key="")
F15 = tibble(Name="doiCount",   Type="VARCHAR",   Size="20",  NoSign="",  NoNull="",  Default="s",       Key="")
F16 = tibble(Name="createDate", Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F17 = tibble(Name="procDate",   Type="VARCHAR",   Size="23",  NoSign="",  NoNull="",  Default="s",       Key="")
F18 = tibble(Name="updateDate", Type="VARCHAR",   Size="23",  NoSign="",  NoNull="",  Default="s",       Key="")
F19 = tibble(Name="updateID",   Type="SMALLINT",  Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F20 = tibble(Name="updateOfID", Type="SMALLINT",  Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F21 = tibble(Name="fileName",   Type="VARCHAR",   Size="254", NoSign="",  NoNull="",  Default="s",       Key="")
F22 = tibble(Name="fileSize",   Type="INT",       Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F23 = tibble(Name="fileType",   Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F24 = tibble(Name="fileTime",   Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F25 = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F26 = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F27 = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F28 = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F29 = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F30 = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F31 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
search=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,
                                        F20,F21,F22,F23,F24,F25,F26,F27,F28,F29,
                                        F30,F31)

# cite - this is a special table format for temporarily savings cites; NO SQL.CORE fields
F1  = tibble(Name="citeID",     Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="type",       Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F3  = tibble(Name="title",      Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F4  = tibble(Name="author",     Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F5  = tibble(Name="journal",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F6  = tibble(Name="Y",          Type="VARCHAR",   Size="4",   NoSign="",  NoNull="T", Default="s",       Key="")
F7  = tibble(Name="V",          Type="VARCHAR",   Size="10",  NoSign="",  NoNull="T", Default="s",       Key="")
F8  = tibble(Name="N",          Type="VARCHAR",   Size="10",  NoSign="",  NoNull="T", Default="s",       Key="")
F9  = tibble(Name="startP",     Type="VARCHAR",   Size="10",  NoSign="",  NoNull="T", Default="s",       Key="")
F10 = tibble(Name="endP",       Type="VARCHAR",   Size="10",  NoSign="",  NoNull="T", Default="s",       Key="")
F11 = tibble(Name="issn",       Type="VARCHAR",   Size="20",  NoSign="",  NoNull="T", Default="s",       Key="")
F12 = tibble(Name="abstract",   Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F13 = tibble(Name="pmid",       Type="VARCHAR",   Size="15",  NoSign="",  NoNull="T", Default="s",       Key="")
F14 = tibble(Name="pmcid",      Type="VARCHAR",   Size="15",  NoSign="",  NoNull="T", Default="s",       Key="")
F15 = tibble(Name="doi",        Type="VARCHAR",   Size="200", NoSign="",  NoNull="T", Default="s",       Key="")
F16 = tibble(Name="issnvnp",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F17 = tibble(Name="stdTitle",   Type="VARCHAR",   Size="250", NoSign="",  NoNull="T", Default="s",       Key="")
F18 = tibble(Name="stdAuthor",  Type="VARCHAR",   Size="250", NoSign="",  NoNull="T", Default="s",       Key="")
F19 = tibble(Name="stdAbstract",Type="VARCHAR",   Size="250", NoSign="",  NoNull="T", Default="s",       Key="")
cite=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19)

# prj$hits
# buildR <- function(nrecs=1) {                              # standard tibble for hits table
# #   "" = rep("", nrecs)
#    Rids = Riddle(nrecs)
#    return(tibble(Rid = Rids, Sid="", L="", pmid="", pmidOK=FALSE, dupOf="", ptype="", journal="", Y="", V="", N="", P="",
#             title="", authors="", doi="", pmcid = "", comments="", abstract="", nrev=0, rev="1"))
# }

#review
F1  = tibble(Name="RID",        Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="BID",        Type="MEDIUMINT", Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F2  = tibble(Name="searchName", Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="UNIQUE")
F3  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F4  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F5  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F6  = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
review=rbind(F1,F2,F3,F4,F5,F6)

# prj$reviews = tibble(Rid=gl$reviewRid,
#               time=now(),
#               decision=input$stage_1_review,
#               comment=input$review_comment,
#               reviewer=reviewer,
#               rowNum = thisRow + 1)

#trial
F1  = tibble(Name="TID",        Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F3  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F4  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F5  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F6  = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
trial=rbind(F1,F2,F3,F4,F5,F6)

#arm
F1  = tibble(Name="AID",        Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F3  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F4  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F5  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F6  = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
arm=rbind(F1,F2,F3,F4,F5,F6)

#cull
F1  = tibble(Name="CID",        Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F3  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F4  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F5  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F6  = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
cull=rbind(F1,F2,F3,F4,F5,F6)







### Combine into a list and save
table.definition.list = list(
   page=page, user=user, project=project, membership=membership, protoHelp=protoHelp, protocol=protocol,
   settings=settings, search=search, cite=cite
)
saveRDS(table.definition.list, file="app/table.definition.list.RDS")
##################################################################################

rm(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,F20,F21,F22,F23,F24,F25,F26,F27,F28,F29) #,F30,F31)
rm(arm, cite, cull, page, project, review, search, trial, user, membership, protocol, protoHelp,
   settings)
