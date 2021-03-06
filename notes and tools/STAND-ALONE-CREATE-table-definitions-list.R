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

# protohelp
F1  = tibble(Name="protohelpID",Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
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
protohelp=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11)

### om$nnn tables

# comment
F1  = tibble(Name="commentID",  Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="item",       Type="VARCHAR",   Size="50",  NoSign="",  NoNull="T", Default="s",       Key="") # (table of item)
F3  = tibble(Name="itemID",     Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="z",       Key="") # (tableID of item)
F4  = tibble(Name="text",       Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F5  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F6  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F7  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F8  = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F9  = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F10 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
comment=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10)

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
F13 = tibble(Name="absCount",   Type="INT",       Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F14 = tibble(Name="pmidCount",  Type="INT",       Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F15 = tibble(Name="doiCount",   Type="INT",       Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
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
F2  = tibble(Name="searchID",   Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F3  = tibble(Name="type",       Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
F4  = tibble(Name="title",      Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
F5  = tibble(Name="author",     Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
F6  = tibble(Name="journal",    Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
F7  = tibble(Name="Y",          Type="VARCHAR",   Size="4",   NoSign="",  NoNull="T", Default="s",       Key="")
F8  = tibble(Name="V",          Type="VARCHAR",   Size="10",  NoSign="",  NoNull="T", Default="s",       Key="")
F9  = tibble(Name="N",          Type="VARCHAR",   Size="10",  NoSign="",  NoNull="T", Default="s",       Key="")
F10 = tibble(Name="startP",     Type="VARCHAR",   Size="10",  NoSign="",  NoNull="T", Default="s",       Key="")
F11 = tibble(Name="endP",       Type="VARCHAR",   Size="10",  NoSign="",  NoNull="T", Default="s",       Key="")
F12 = tibble(Name="issn",       Type="VARCHAR",   Size="20",  NoSign="",  NoNull="T", Default="s",       Key="")
F13 = tibble(Name="abstract",   Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
F14 = tibble(Name="pmid",       Type="VARCHAR",   Size="15",  NoSign="",  NoNull="T", Default="s",       Key="")
F15 = tibble(Name="pmcid",      Type="VARCHAR",   Size="15",  NoSign="",  NoNull="T", Default="s",       Key="")
F16 = tibble(Name="doi",        Type="VARCHAR",   Size="200", NoSign="",  NoNull="T", Default="s",       Key="")
F17 = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
F18 = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
cite=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18) #,F19)

# catalog - this is the working list of all citations
F1  = tibble(Name="catalogID",  Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="searchID",   Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F3  = tibble(Name="citeID",     Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F4  = tibble(Name="hasDup",     Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F5  = tibble(Name="dupOf",      Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F6  = tibble(Name="dupDM",      Type="VARCHAR",   Size="20",  NoSign="",  NoNull="T", Default="s",       Key="")  # DM: detection method
F7  = tibble(Name="type",       Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
F8  = tibble(Name="title",      Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
F9  = tibble(Name="author",     Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
F10 = tibble(Name="journal",    Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
F11 = tibble(Name="Y",          Type="VARCHAR",   Size="4",   NoSign="",  NoNull="T", Default="s",       Key="")
F12 = tibble(Name="V",          Type="VARCHAR",   Size="10",  NoSign="",  NoNull="T", Default="s",       Key="")
F13 = tibble(Name="N",          Type="VARCHAR",   Size="10",  NoSign="",  NoNull="T", Default="s",       Key="")
F14 = tibble(Name="startP",     Type="VARCHAR",   Size="10",  NoSign="",  NoNull="T", Default="s",       Key="")
F15 = tibble(Name="endP",       Type="VARCHAR",   Size="10",  NoSign="",  NoNull="T", Default="s",       Key="")
F16 = tibble(Name="issn",       Type="VARCHAR",   Size="20",  NoSign="",  NoNull="T", Default="s",       Key="")
F17 = tibble(Name="abstract",   Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
F18 = tibble(Name="pmid",       Type="VARCHAR",   Size="15",  NoSign="",  NoNull="T", Default="s",       Key="")
F19 = tibble(Name="pmcid",      Type="VARCHAR",   Size="15",  NoSign="",  NoNull="T", Default="s",       Key="")
F20 = tibble(Name="doi",        Type="VARCHAR",   Size="200", NoSign="",  NoNull="T", Default="s",       Key="")
F21 = tibble(Name="issnvnp",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F22 = tibble(Name="stdTitle",   Type="VARCHAR",   Size="250", NoSign="",  NoNull="T", Default="s",       Key="")
F23 = tibble(Name="stdAuthor",  Type="VARCHAR",   Size="250", NoSign="",  NoNull="T", Default="s",       Key="")
F24 = tibble(Name="stdAbstract",Type="VARCHAR",   Size="250", NoSign="",  NoNull="T", Default="s",       Key="")
F25 = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="T", Default="x",       Key="")
F26 = tibble(Name="reviewCount",Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="") # 0-No Review
F27 = tibble(Name="reviewBest", Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="") # 1-Stage 1 Fail
F28 = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="") # 2-Stage 1 Pass
F29 = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="") # 3-Extraction Fail
F30 = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="") # 4-Extraction Pass
F31 = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F32 = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F33 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
catalog=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,
                                         F20,F21,F22,F23,F24,F25,F26,F27,F28,F29,
                                         F30,F31,F32,F33)
#review
F1  = tibble(Name="reviewID",   Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="catalogID",  Type="MEDIUMINT", Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F3  = tibble(Name="decision",   Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="") # 0-Not Reviewed; 1-S1 Fail; 2-S1 Pass; 3-S2 Fail; 4-S2 Pass
F4  = tibble(Name="detail",     Type="TEXT"   ,   Size="",    NoSign="",  NoNull="",  Default="x",       Key="") # checkbox results
F5  = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F6  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F7  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="") # reviewer
F8  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="") # time of review
F9  = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F10 = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F11 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
review=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11)

#pico
F1  = tibble(Name="picoID",     Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="picoNUM" ,   Type="MEDIUMINT", Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F3  = tibble(Name="name",       Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F4  = tibble(Name="value",      Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F5  = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F6  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F7  = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F8  = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F9  = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F10 = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F11 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
pico=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11)

#extract
F1  = tibble(Name="extractID",  Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="catalogID",  Type="MEDIUMINT", Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F3  = tibble(Name="studyNUM",   Type="MEDIUMINT", Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F4  = tibble(Name="armNUM",     Type="MEDIUMINT", Size="",    NoSign="T", NoNull="",  Default="z",       Key="")
F5  = tibble(Name="outcome",    Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F6  = tibble(Name="name",       Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F7  = tibble(Name="value",      Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F8  = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F9  = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F10 = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F11 = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F12 = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F13 = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F14 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
extract=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14)

#ids
F1  = tibble(Name="idsID",     Type="VARCHAR",   Size="70",  NoSign="F", NoNull="T", Default="s",       Key="PRIMARY KEY")
F2  = tibble(Name="form",      Type="VARCHAR",   Size="50",  NoSign="F", NoNull="T", Default="s",       Key="")
F3  = tibble(Name="idAsName",  Type="VARCHAR",   Size="15",  NoSign="F", NoNull="T", Default="s",       Key="")
ids=rbind(F1,F2,F3)

#result
F1  = tibble(Name="resultID",   Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="extractID",  Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F3  = tibble(Name="catalogID",  Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F4  = tibble(Name="studyNUM",   Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F5  = tibble(Name="armNUM",     Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F6  = tibble(Name="P",          Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F7  = tibble(Name="I",          Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F8  = tibble(Name="C",          Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F9  = tibble(Name="O",          Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F10 = tibble(Name="TS",         Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F11 = tibble(Name="info",       Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F12 = tibble(Name="esType",     Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F13 = tibble(Name="nC",         Type="VARCHAR",   Size="15",  NoSign="F", NoNull="T", Default="s",       Key="")
F14 = tibble(Name="nI",         Type="VARCHAR",   Size="15",  NoSign="F", NoNull="T", Default="s",       Key="")
F15 = tibble(Name="es",         Type="VARCHAR",   Size="15",  NoSign="F", NoNull="T", Default="s",       Key="")
F16 = tibble(Name="v",          Type="VARCHAR",   Size="15",  NoSign="F", NoNull="T", Default="s",       Key="")
F17 = tibble(Name="se",         Type="VARCHAR",   Size="15",  NoSign="F", NoNull="T", Default="s",       Key="")
F18 = tibble(Name="ci.lo",      Type="VARCHAR",   Size="15",  NoSign="F", NoNull="T", Default="s",       Key="")
F19 = tibble(Name="ci.hi",      Type="VARCHAR",   Size="15",  NoSign="F", NoNull="T", Default="s",       Key="")
F20 = tibble(Name="weight",     Type="VARCHAR",   Size="15",  NoSign="F", NoNull="T", Default="s",       Key="")
F21 = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F22 = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F23 = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F24 = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F25 = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F26 = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F27 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
result=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,
                                        F20,F21,F22,F23,F24,F25,F26,F27)
#analysis
F1  = tibble(Name="analysisID", Type="MEDIUMINT", Size="",    NoSign="T", NoNull="T", Default="AUTOINC", Key="PRIMARY KEY")
F2  = tibble(Name="name",       Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F3  = tibble(Name="type",       Type="VARCHAR",   Size="254", NoSign="F", NoNull="T", Default="s",       Key="")
F4  = tibble(Name="P",          Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F5  = tibble(Name="I",          Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F6  = tibble(Name="C",          Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F7  = tibble(Name="O",          Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F8  = tibble(Name="TS",         Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F9  = tibble(Name="comment",    Type="TEXT",      Size="",    NoSign="",  NoNull="",  Default="x",       Key="")
F10 = tibble(Name="verNum",     Type="SMALLINT",  Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F11 = tibble(Name="verUser",    Type="VARCHAR",   Size="40",  NoSign="",  NoNull="T", Default="s",       Key="")
F12 = tibble(Name="verTime",    Type="VARCHAR",   Size="23",  NoSign="",  NoNull="T", Default="s",       Key="")
F13 = tibble(Name="clash",      Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
F14 = tibble(Name="clashFacts", Type="TEXT",      Size="",    NoSign="",  NoNull="T" ,Default="x",       Key="")
F15 = tibble(Name="deleted",    Type="TINYINT",   Size="",    NoSign="T", NoNull="T", Default="z",       Key="")
analysis=rbind(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15)

   #    extract id, catalogid, studyNUM, armNUM, participantgroup, intervention, comparison, outcome, timespan, d, v.

### Combine into a list and save
table.definition.list = list(
   page=page, user=user, project=project, membership=membership, protohelp=protohelp,
   comment=comment, protocol=protocol, settings=settings, search=search, cite=cite,
   catalog=catalog, review=review, extract=extract, pico=pico, ids=ids, result=result,
   analysis=analysis
)
saveRDS(table.definition.list, file="app/table.definition.list.RDS")
##################################################################################

rm(F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,F11,F12,F13,F14,F15,F16,F17,F18,F19,
                              F20,F21,F22,F23,F24,F25,F26,F27,F28,F29,
                              F30,F31,F32,F33)
rm(cite, page, project, catalog, review, search, user, membership, protocol, protohelp,
   settings, comment, extract, pico, ids, result, analysis)
