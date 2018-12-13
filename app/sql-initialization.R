### open-meta.app sql-initialization.R
### Tom Weishaar - Nov 2017 - v0.1

### Code that is only loaded if there is a need to build the initial om$prime database

# Also creates "admin" and "shiny" users.

DB.Initialization = function(burnItAllDown=FALSE) {
   # caller must set up dbLink
   start.time <- Sys.time()
   on.exit({
      cat("### DB Initialization ###\n")
      print(Sys.time() - start.time)
   })

   # TEST CODE - deletes users and database so the rest of the code is tested
   if(burnItAllDown) {
      SQL.Users <- dbGetQuery(dbLink, "SELECT `user` FROM `mysql`.`user`;")
      if("admin" %in% SQL.Users$user) {
         r = dbExecute(dbLink, "DROP USER 'admin'@'localhost';")
      }
      if("shiny" %in% SQL.Users$user) {
         r = dbExecute(dbLink, "DROP USER 'shiny'@'localhost';")
      }
      r = dbExecute(dbLink, "FLUSH PRIVILEGES;")

      SQL.DBs <- dbGetQuery(dbLink, "SHOW DATABASES")           # get all DataBases
      ourDBs = SQL.DBs[str_sub(SQL.DBs[[1]], 1, 3)=="om$", 1]   # limit this to ours! They begin with om$.
      for(dbname in ourDBs) {
         r = dbExecute(dbLink, paste0("DROP DATABASE IF EXISTS `", dbname, "`;"))
      }
   }

   SQL.Users <- dbGetQuery(dbLink, "SELECT `user` FROM `mysql`.`user`;")

   # If no admin, create it...
   if(!"admin" %in% SQL.Users$user) {
      r = dbExecute(dbLink, paste0("CREATE USER 'admin'@'localhost' IDENTIFIED BY '", SQL.Passwords[2], "';"))
      r = dbExecute(dbLink, "GRANT USAGE ON *.* TO 'admin'@'localhost';")
      r = dbExecute(dbLink, "GRANT EXECUTE, PROCESS, SELECT, SHOW DATABASES, SHOW VIEW, ALTER, ALTER ROUTINE, CREATE, CREATE ROUTINE, CREATE TABLESPACE, CREATE TEMPORARY TABLES, CREATE VIEW, DELETE, DROP, EVENT, INDEX, INSERT, REFERENCES, TRIGGER, UPDATE  ON *.* TO 'admin'@'localhost';")
      r = dbExecute(dbLink, "FLUSH PRIVILEGES;")
   }

   # If no shiny, create it...
   if(!"shiny" %in% SQL.Users$user) {
      r = dbExecute(dbLink, paste0("CREATE USER 'shiny'@'localhost' IDENTIFIED BY '", SQL.Passwords[3], "';"))
      r = dbExecute(dbLink, "GRANT USAGE ON *.* TO 'shiny'@'localhost';")
      r = dbExecute(dbLink, "GRANT EXECUTE, PROCESS, SELECT, SHOW DATABASES, SHOW VIEW, ALTER, ALTER ROUTINE, CREATE, CREATE ROUTINE, CREATE TABLESPACE, CREATE TEMPORARY TABLES, CREATE VIEW, DELETE, DROP, EVENT, INDEX, INSERT, REFERENCES, TRIGGER, UPDATE  ON *.* TO 'shiny'@'localhost';")
      r = dbExecute(dbLink, "FLUSH PRIVILEGES;")
   }

   # update SQL.Users after changes
   SQL.Users <- dbGetQuery(dbLink, "SELECT `user` FROM `mysql`.`user`;")

   # New school init...
      # .sql dump files created with mysqldump or HeidiSQL
      # omPrimeBaseDB.sql includes:
         # membership table with Admin and TomW as PI and Observer on project 1
         # page table with all current pages
         # project table with 1 project
         # protohelp table
         # user table with Admin and TomW
      # omPrimeAuxDB.sql includes:
         # settings with uploadMaxCites and forms
         # ids with utf8 ids for those forms
      # omPrj1BaseDB.sql has these tables
         # catalog
         # cite1
         # cite2
         # comment
         # protocol
         # review
         # search
      # omPrj1AuxDB.sql has these tables
         # extract
         # pico
         # settings
         # ids


   filenames <- c("../Database/omPrimeBaseDB.sql", "../Database/omPrimeAuxDB.sql", "../Database/omPrj1BaseDB.sql", "../Database/omPrj1AuxDB.sql")
   for(filename in filenames) {
      if(file.exists(filename)) {                             # see: http://www.open-meta.org/technology/how-to-source-a-mysqldump-file-with-syntax-statements/
         sql <- stri_read_lines(filename, encoding="utf8")    # read file into sql character vector
         cmd <- ""                                            # initialize command
         for(line in sql) {                                   # loop through sql vector, one line at a time
            if(line!="" && str_sub(line,1,3)!="-- ") {        # skip blank and comment lines
               cmd <- paste0(cmd, line)                       # add current line to command
               if(str_sub(cmd,-1,-1)==";") {                  # if command ends with ";", execute it
                  r <- dbExecute(dbLink, cmd)                 #    else add more lines to cmd
                  cmd <- ""                                   # start over with next command
               }
            }
         }
      }
   }

# Add a page to om$prime.page
      # pg = pageGet(pool=root.pool)
      # pg$spReq[2] = 0
      # pg$pageType[2] = 0
      # pg$pageText[2] = ""
      # pg$deleted[2] = 0
      #
      # pg$spReq[2] = 500
      #
      # pg$pageName[2] = "adminForms"
      # r = recSaveR(pg, pool=root.pool)
      #



   return("Initialization of database is complete...")















   # Nothing below here runs unless you comment out the return() above

   # initialize sample project
   # Only run this if there's no .sql file above to create the initial project
   source("sql-init-project.R", local=TRUE)
   r = initProject("1", "Effect of daily vitamin D<sub>3</sub> supplementation on human health and performance", root.pool)
   return("Initialization of database is complete...")

   # And nothing below here runs either unless you comment out the return() above, too.

### Don't delete the following code. It may still be needed for new installs when there's not an sql dump file!!
   # Old school init....

   # does om$prime database exist?
   SQL.DBs <- dbGetQuery(dbLink, "SHOW DATABASES")
   if(!"om$prime" %in% SQL.DBs$Database) {
      r = dbExecute(dbLink, "CREATE DATABASE `om$prime`;")
      r = dbExecute(dbLink, createTable("om$prime", "page"))
      r = dbExecute(dbLink, createTable("om$prime", "user"))
      r = dbExecute(dbLink, createTable("om$prime", "project"))
      r = dbExecute(dbLink, createTable("om$prime", "membership"))
      r = dbExecute(dbLink, createTable("om$prime", "protohelp"))
      r = dbExecute(dbLink, createTable("om$prime", "settings"))
      r = dbExecute(dbLink, createTable("om$prime", "ids"))
      # inititialise user table
      u = userGet(pool=root.pool)
      u$userName[2] = "Admin"
      u$hashedPW[2] = hashpw(admin_password)
      u$email[2] = admin_email_address
      u$emailOK[2] = 1
      u$sPowers[2] = 900
      u$sessionID[2] = u$userName[2]              # needs to be unique, which means not blank!
      u$regDate[2] = sTime()
      u$evDate[2] = sTime()
      u$loginTries[2] = 0
      u$workingOn[2] = 0
      u$deleted[2] = 0
      r = recSaveR(u, pool=root.pool)

      # make a bunch of user records for testing purposes
      if(FALSE) {
         for(i in str_to_upper(letters[1:5])) {
            u$userName[2] = paste("Admin", i)
            u$hashedPW[2] = hashpw(i)
            u$sessionID[2] = u$userName[2]              # needs to be unique, which means not blank!
            u$email[2] = paste0(i,"@omega-ratio.com")
            r = recSaveR(u, pool=root.pool)
         }
      }

      # inititialise settings table
      s = settingsGet(pool=root.pool)
      # s$name[2] = "uploadMaxMB"
      # s$value[2] = "16"
      s$name[2] = "uploadMaxCites"
      s$value[2] = "3500"
      s$comment[2] = paste0("<p>This is the maximum number of citations that can be uploaded at one time. You ",
                            "can increase this, perhaps up to 5,000, but it's better to encourage users to ",
                            "split their citations up into multiple, smaller searches.</p>",
                            "<p><br></p>",
                            "<p>Shiny has a default upload size of 5 MB that our code increases to 16 MB. MySQL ",
                            "has a <i>max_allowed_packet</i> that defaults to 4 MB; our code increases this ",
                            "one to 16 MB as well. These changes are near the top of the app.R file and are ",
                            "required by the cite upload and table save in Search.R.</p>",
                            "<p><br></p>",
                            "<p>An average citation uses about 2.75 KB, so 16 MB should be plenty for up to 5,000 ",
                            "citations. In any case, this setting is app-specific, not session-specific, ",
                            "so it applies to everyone.</p>")
      r = recSaveR(s, pool=root.pool)

      # inititialise project table
      prj = projectGet(pool=root.pool)
#      prj$projectName[2] = paste0(sample(words,12), collapse=" ")
      prj$projectName[2] = "Effect of daily vitamin D<sub>3</sub> supplementation on human health and performance"
      PROJECTNAME = prj$projectName[2]
      prj$status[2] = 1
      prj$deleted[2] = 0
      r=recSaveR(prj, pool=root.pool)
      # make a bunch of project records for testing purposes
      # if(TRUE) {
      #    for(i in str_to_upper(letters)[1:10]) {
      #       prj$projectName[2] = paste0("Project ", i, " - ", paste0(sample(words,12), collapse=" "))
      #       r = recSaveR(prj, pool=root.pool)
      #    }
      # }

      # initialize membership table
      membership = membershipGet(pool=root.pool)
      membership$userID[2] = 1
      membership$projectID[2] = 1
      membership$role[2] <- "Principal Investigator"
      membership$contact[2] <- 1
      r=recSaveR(membership, pool=root.pool)

      # make a bunch of membership records for testing purposes
      # if(TRUE) {
      #    users = userGet("userID", pool=pool)$userID
      #    for(i in 2:27) {
      #       membership$projectID[2] = i
      #       for(u in sample(users, 5)) {
      #          membership$userID[2] = u
      #          r = recSaveR(membership, pool=root.pool)
      #       }
      #    }
      # }

      # inititialise page table
      pg = pageGet(pool=root.pool)
      pg$spReq[2] = 0
      pg$pageType[2] = 0
      pg$pageText[2] = ""
      pg$deleted[2] = 0

      pg$pageName[2] = "login"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "profile"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "lostpassword"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "Help"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "prjNew"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "prjMy"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "prjActive"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "prjFinished"
      r = recSaveR(pg, pool=root.pool)

      pg$spReq[2] = 500

      pg$pageName[2] = "adminCP"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "adminUsers"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "adminPages"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "adminProjects"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "adminUserPrj"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "adminPrjUser"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "adminForms"
      r = recSaveR(pg, pool=root.pool)

      pg$spReq[2] = 1   # Need to be logged in for this one to send email to PI

      pg$pageName[2] = "prjAdmin"
      r = recSaveR(pg, pool=root.pool)

      pg$spReq[2] = 0   # Don't need to be logged in to view

      pg$pageName[2] = "Protocol"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "Join"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "Search"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "SearchList"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "SearchAnalysis"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "Review"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "ReviewAnalysis"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "Extract"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "Synthesize"
      r = recSaveR(pg, pool=root.pool)

      pg$pageName[2] = "Publish"
      r = recSaveR(pg, pool=root.pool)
   }

saveP = function(order, title, helpText) {
   SET = newRec("protohelp")
   SET$order[2] = order
   SET$title[2] = title
   SET$helpText[2] = helpText
   return(recSaveR(SET, pool=root.pool))
}

      # inititialise protohelp table
   ## A is Overall instructions
order="A"
title="Overall instructions for writing your project's protocol"
helpText='
<p>A systematic review is a special type of literature review. Systematic reviews, which can be done with or without meta-analysis, <em><b>control researcher bias</b></em> (<a href="http://journals.sagepub.com/doi/pdf/10.1177/0163278702025001003" target="_blank" rel="noopener">Chalmers, Hedges, &amp; Cooper, 2002</a>).</p>
<p>Instead of allowing a research group to cherry-pick literature that the research group agrees with, a systematic review requires the group to explicitly state the strategy it will use to discover all of the studies that are relevant to its research question. The research group must also explicitly state the criteria it will use to decide which studies it will include in and which it will exclude from its analysis.</p>
<p>Before your project can be activated, you must detail your strategy and criteria here in the protocol section of your project. Once you have activated your project, the only sections of the protocol you will be able to update are <b><i>Amendments</i></b> and <b><i>Publications</i></b>.</p>
<p>In addition to discouraging research groups that want to promote a preconceived result and encouraging groups that want to find out how things actually work, your written protocol will also be very helpful for attracting people to help you and for publishing your results in a peer-reviewed journal. To help authors improve their reporting of systematic reviews and meta-analyses, academic journals use a standard called <a href="http://www.prisma-statement.org/">PRISMA</a> (<em>Preferred Reporting Items for Systematic Reviews and Meta-Analyses: the PRISMA statement</em>, <a href="https://www.bmj.com/content/339/bmj.b2535.full?view=long&amp;pmid=19622551">Moher et al, 2009</a>).</p>
<p>Our instructions for the protocol you will write here are based on a PRISMA extension called <a href="https://www.bmj.com/content/bmj/349/bmj.g7647.full.pdf">PRISMA-P</a> (<i>Preferred reporting items for systematic review and meta-analysis protocols, <a href="https://www.bmj.com/content/bmj/349/bmj.g7647.full.pdf" target="_blank" rel="noopener">Shamseer et al., 2015</a>).</i></p>
<p>As you edit each section, instructions will appear to help you understand what that section is for. Italicized sentences are exact quotes from Shamseer et al. The more familiar you are with PRISMA and PRISMA-P, the easier it will be to complete your project\'s protocol, to complete your project, and to get your project published.</p>
'
r = saveP(order, title, helpText)

   ## Aa is "Amendments"
order="Aa"
title="Amendments"
helpText='As you execute your protocol, you will likely discover that some of the promises you made are impossible to keep. In this section you take back your original promises and describe what your team actually did. When you publish papers on this project, you will likely only be required to provide a link to your original protocol, but will be asked to discuss your protocol amendments in some detail, so make good notes here on what you had to change. The system will automatically note the author and date of all amendments and list them here.
'
r = saveP(order, title, helpText)

   ## Ab
order="Ab"
title="Publications"
helpText='For each paper you publish about this project, add a citation here. If the paper is available online, please include a link to it.
'
r = saveP(order, title, helpText)

   ## B is "Administrative Information Section"
order="B"
title="Administrative Information"
helpText=''
r = saveP(order, title, helpText)

   ## B is "Administrative Information Section"
order="Ba"
title="Project Name"
helpText='<p>You can edit your project name if you like.</p>'
r = saveP(order, title, helpText)

order="Bb"
title="Protocol Title"
helpText='
<p>The Protocol Title should begin with the name of the project, but then specify that this is the <i>protocol</i> for the project. If the project is an update of a previous systematic review, make sure it is identified as such.</p>
'
r = saveP(order, title, helpText)

order="Bc"
title="Protocol Registration"
helpText='
<p>If the project\'s protocol has been registered elsewhere, provide the name of the registry (such as <a href="http://www.crd.york.ac.uk/PROSPERO", target="_blank">PROSPERO</a>), registration number, and link.</p>
'
r = saveP(order, title, helpText)

order="Bd"
title="Protocol's Authors"
helpText='
<p>Provide the names of the authors of this protocol. You can also include institutional affiliation, but e-mail addresses can be harvested by spammers. If people want to contact you, they can do so through Open-Meta.</p>
'
r = saveP(order, title, helpText)

order="Be"
title="Funding Sources"
helpText='
<p>Identify any funding sources for this project. If there aren\'t any, say <i>none</i>.</p>
'
r = saveP(order, title, helpText)

order="Bf"
title="Conflicts of Interest"
helpText='
<p>Do any of the protocol\'s authors have a conflict of interest? If so, please describe it. If not, say <i>none</i>.</p>
'
r = saveP(order, title, helpText)

order="Bg"
title="Other Authorship Notes"
helpText='
<p>Any other information you want to provide about the authors. For example, if this project is part of a dissertation, mention that and the names and institutions of the author\'s advisors. Even if you have nothing to add, say so; don\'t leave this or any other items here blank.</p>
'
r = saveP(order, title, helpText)

   ## C is "Introduction"
order="C"
title="Project Rationale and Aims"
helpText=''
r = saveP(order, title, helpText)

order="Ca"
title="Rationale"
helpText='
<p>Describe the rationale for your study. In a sense this is your <i>literature review</i>, but stay in the clouds here - don\'t try to review the articles that will become the subject of your project. On the other hand, it <b><i>is</i></b> necessary to discuss any relevant prior systematic reviews and what you perceive to be their limitations. It is also appropriate to discuss prior non-systematic reviews. If you have a theoretical model explaining why your study will be indispensable, discuss it here. Why are you doing this?</p>
'
r = saveP(order, title, helpText)

order="Cb"
title="Specific Aims (Research Questions)"
helpText='
<p>Provide an explicit statement of the question(s) your study will address. Include operational definitions of all ambiguous terms (for example: "a <i>beneficial effect</i> is an effect size favorable to the intervention with a 95% confidence interval that does not include the no-effect value"). Briefly describe how you will limit the universe of studies that you will examine; but just include a quick summary here, you\'ll present your detailed restraints in the next section.</p>
'
r = saveP(order, title, helpText)

   ## D is "Methods"
order="D"
title="Project Methodology"
helpText=''
r = saveP(order, title, helpText)

order="Da"
title="Eligibility Criteria"
helpText=''
r = saveP(order, title, helpText)

order="Daa"
title="Report Characteristics"
helpText='
<p><b><i>To successfully lead a crowd-sourced project, you need to provide very specific eligibility criteria. Put your best effort into the six <i>eligibility</i> parts of this protocol.</i></b> These six sections together describe the restraints your team will use to limit the universe of all possible studies to those studies you will include in and exclude from your project.</p>
<p>In this first section, discuss what kinds of reports will meet your criteria based on factors such as publication year or language of the report. Will you include only published reports? Only peer-reviewed reports? What about theses, dissertations, commentaries, letters, or editorials? Will you include reports that otherwise meet your criteria but that weren\'t found by your literature search?</p>
'
r = saveP(order, title, helpText)

order="Dab"
title="Study Designs"
helpText='
<p>In this section, limit your discussion to what study designs are acceptable for your project. Randomized-controlled trials with individual (rather than cluster) randomization are best for meta-analysis; other designs might work if you can come up with enough of them and a convincing effect size for each study. However, unless your team includes expertise in the interpretation of a design\'s results, you\'d best exclude studies with that design unless you won\'t be doing a meta-analysis. In that case your options are more inclusive, but you must describe (later in this protocol) an alternative method for combining the results of the studies you include in your project.</p>
'
r = saveP(order, title, helpText)

order="Dac"
title="Study Characteristics - Participants"
helpText='
<p>Considering the participants in a study, describe your project\'s study inclusion and exclusion criteria. For example, you might limit the universe of studies you will examine to those with human participants of a certain age in a specific geographic locale. Other commonly-used limitations for participants are gender, health conditions, and living setting. Be very specific: for example, if you are only interested in adults, will you accept a study that included children but reported results for adults separately?</p>
'
r = saveP(order, title, helpText)

order="Dad"
title="Study Characteristics - Intervention"
helpText='
<p>Describe the experimental intervention of the studies you will include in your project. If you accept non-experimental designs, how will you create the two groups you\'ll need to calculate an effect size for the study (for example, will you compare the status of some variable you think is important in the quintiles with the lowest and highest scores on some Outcome variable)? Does it matter how the intervention is administered (for example, a drug could be taken by mouth, by injection, or by skin cream in different studies; will you accept them all?)</p>
'
r = saveP(order, title, helpText)

order="Dae"
title="Study Characteristics - Comparisons"
helpText='
<p>The Open-Meta app supports combining studies that compare a control group to an intervention group or that compare a baseline or pre-treatment (pre-test) score with a post-treatment (post-test) score. The best data to work with, however, includes both control and intervention group data and both baseline and post-treatment scores on the outcomes of interest. Will your meta-analysis accept studies without a control group or without baseline data? If you require a control group, also describe the types of control groups your project will accept. For example, will your project include a study with a no-intervention control group or only studies in which the control group received a placebo? What about a a control group that received a placebo with a standard-of-care dose of the intervention rather than a zero dose? Typically, comparison groups must be identical except for the intervention - will you include studies with specific types of non-identical groups?</p>
'
r = saveP(order, title, helpText)

order="Daf"
title="Study Characteristics - Outcomes"
helpText='
<p>Describe the type and timing of the Outcome measurements in the studies you will include in your project. If you are interested in multiple Outcomes, will you include studies that report some but not all of your Outcomes, or must the study include all Outcomes to be eligible for inclusion in your project? Is there a required time delay between the beginning of the intervention and a valid Outcome measurement? If a study measures an Outcome at several different time points, will you consider each time point a separate Arm of the study or only use one Outcome measurement? Which one? Is an Outcome measured after the intervention has been completed (for example, after the particpants have stopped taking a drug) a valid or invalid measurement?</p>
'
r = saveP(order, title, helpText)

order="Db"
title="Information Sources"
helpText='
<p>What information sources will you use to discover studies to include in your project? Typical projects rely primarily on one or more library bibliographic databases, some of which are publicly available, such as <a href="https://www.ncbi.nlm.nih.gov/pubmed/", target="_blank">PubMed</a>. Other information sources include reference lists and contacting the authors of included studies. Particularly important are the studies included in prior systematic reviews similar to your project.</p>
'
r = saveP(order, title, helpText)

order="Dc"
title="Search Strategy"
helpText='
<p>In terms of at least one bibliographic database, what search strategy will you use? The best way to answer this question is to do some trial searches using several different databases so that you know what kinds of indexing terms and filters are available and useful for your project. You should report an estimate of how many studies your search will find. An excellent search strategy casts a net that\'s big enough to find all the studies relevant to your project but small enough to avoid catching more studies than your team has the capacity to review. (If you use multiple databases, the Open-Meta software will automatically identify duplicate citations for you.)</p>
'
r = saveP(order, title, helpText)

order="Dd"
title="Data Collection and Management Protocols"
helpText=''
r = saveP(order, title, helpText)

order="Dda"
title="Data Management"
helpText='
<p>In general, once you\'ve uploaded citations from electronic databases, all your other data management is handled by the Open-Meta software, so this is where we get a little recognition. Since the "subjects" of your project are research studies, rather than individuals, your Institutional Review Board should issue you an exemption letter, and you can also mention that here.</p>
'
r = saveP(order, title, helpText)

order="Ddb"
title="Stage 1 Review Process"
helpText='
<p>In Open-Meta\'s Stage 1 Review, your reviewers scan study titles and abstracts to determine which studies are obviously not suitable for your project. How many reviewers will you have? What percentage of your studies will be scanned by multiple reviewers? Typically, if any single reviewer marks a study as passing Stage 1 Review, it is included in Stage 2. Is that how your team will do it? Will you categorize reasons why studies don\'t pass Stage 1 Review? What categories will you use?</p>
'
r = saveP(order, title, helpText)

order="Ddc"
title="Stage 2 Review and Data Extraction Process"
helpText='
<p>In Open-Meta\'s Stage 2 Review process, your reviewers extract the data required to determine an effect size for the intervention your project is studying. This more thorough review will also find some studies that don\'t meet your project\'s inclusion criteria. In this section, discuss your process for Stage 2 review. How many reviewers will you have and how will they check each other\'s work?</p>
'
r = saveP(order, title, helpText)

order="De"
title="Data Items"
helpText='<p>Open-Meta structures its universe around research Trials, each of which may be descibed in one or more research Reports. Each Report provides data on one or more of the Trial\'s Outcomes. Each outcome can have one or more Trial Outcome Arms with data for the group considered the Control Group and for the group considered the Intervention Group.</p>
<p>In general, for each Outcome you\'ll collect the name of the Outcome and how it was measured. For each of that Outcome\'s Arms you\'ll collect the number of particpants and the outcome measurement in terms of both central tendency and variance (3 items). These items will be collected for both the control and intervention groups (3 * 2 = 6 items) at both baseline and outcome measurement (6 * 2 = 12 items). For each Outcome Arm you\'ll also record what\'s unique (different dosage? different time interval?) about that Arm (12 + 1 = 13 items minimum per Outcome Arm).</p>
<p>If the Outcome is measured multiple times after baseline and you want to record this, each time is a separate Arm. Now multiply that by the number of relevant Outcomes reported. Yes, this is a lot of work, but at least Open-Meta is here to help you keep it all straight.</p>
<p>You will also collect bias information on each Trial, but there\'s a separate section for that information below.</p>
<p>Briefly document this here and if you plan to collect additional information about the groups in each Arm (for example, average body weight or percent female), mention that here.</p>
'
r = saveP(order, title, helpText)

order="Df"
title="Outcomes and Prioritization"
helpText='
<p>As mentioned in the previous item, each of your Trials will report on one or more Outcomes. An example of an Outcome is a measure of human physical or mental health or performance. In this section, discuss the data you will collect from the studies in your project in terms of the primary Outcomes you\'re interested in. Do you have any secondary Outcomes?</p>
'
r = saveP(order, title, helpText)

order="Dg"
title="Risk of bias in individual Trials"
helpText='
<p>Open-Meta allows you to rate each Trial for six types of Trial bias. Each rating can be Unclear, Low Risk, or High Risk. The six types of bias are Funding Bias, Randomiation and Allocation Concealment, Blinding of Participants, Blinding of Research Personnel and Outcome Assessors, and Level of Attrition and Exclusions.</p>
<p>In this section discuss how your team will determine its ratings for each Trial. Will a single person rate all the Trials? Different persons? Multiple persons? How will you know they are adequately trained to make these ratings?</p>
'
r = saveP(order, title, helpText)

order="Dh"
title="Data Synthesis"
helpText=''
r = saveP(order, title, helpText)

order="Dha"
title="Criteria for Synthesis"
helpText='
<p><i>Diversity in study populations, interventions, outcomes, or trial conduct may mean that including some studies in a meta-analysis, or even conducting meta-analyses at all, will be impossible. Authors should describe, with reference to the PICO criteria, the conditions that should be present before they will proceed with statistical synthesis. Thus authors might consider whether to include trials with differing formulations or doses of the experimental treatment, studies using differing versions of a technology (such as a device), studies with different age profiles in the sample population, or studies with different follow-up times</i>, (<a href="https://www.bmj.com/content/bmj/349/bmj.g7647.full.pdf" target="_blank" rel="noopener">Shamseer et al., 2015</a>).</p>
'
r = saveP(order, title, helpText)

order="Dhb"
title="Data Reduction and Consistency Measures"
helpText='
<p>If appropriate, state that you will synthesize your data using meta-analysis. Because one Trial may provide multiple Outcomes or even multiple Arms for a single Outcome (for example, Outcome Arms could have different doses or durations or both), Open-Meta defaults to robust variance estimation (Hedges, Tipton, and Johnson. 2010. Robust variance estimation in meta-regression with dependent effect size estimates. Res Synth Methods, 1(1), 39-65.). Unlike random effects meta-analysis, robust variance meta-analysis allows inclusion of all Outcome Arms from a Trial, but requires more Trials for stable results. Random effect methods, on the other hand, require selecting just one Outcome Arm per Trial or averaging data over each Trial, reducing the number of data points for the analysis from the number of Outcome Arms to the number of Trials. If the number of studies is too limited to use the robust variance method, Open-Meta uses random effects methods.</p>
<p>State how will you measure heterogeneity between studies and the level of heterogeneity (example: <i>I-squared > 50% or p < .05</i>) at which you would do a secondary analysis to try to uncover reasons for the dissimilarity in the Trials.</p>
'
r = saveP(order, title, helpText)

order="Dhc"
title="Secondary Analyses"
helpText='
<p><i>Investigating possible causes of between-study variability or exploring the robustness of meta-analyses by using subgroup analysis or meta-regression may be desirable. If authors plan such analyses, they should state this and specify the covariates anticipated for the analyses (such as disease type or severity, or treatment dose). For subgroup analyses, authors should describe how they will partition the covariate into subgroups (for example, what will constitute mild or severe disease, low or high treatment dose). Whether they plan a fixed or random effects approach and how they will evaluate residual heterogeneity should also be stated.</i></p>
<p><i>If any sensitivity analyses are intended - such as including or excluding small studies, studies with high risk of bias, industry funded studies, or outlier studies - authors should describe their plan for doing so</i>, (<a href="https://www.bmj.com/content/bmj/349/bmj.g7647.full.pdf" target="_blank" rel="noopener">Shamseer et al., 2015</a>).</p>
'
r = saveP(order, title, helpText)

order="Dhd"
title="Alternative Analyses"
helpText='
<p>State the extent to which your project will include a qualitative or narrative summary. If you don\'t plan to use meta-analysis, go into detail here about how you will combine Trial results to arrive at a conclusion.</p>
'
r = saveP(order, title, helpText)

order="Di"
title="Meta-biases"
helpText='
<p>Empirical evidence suggests that a meta-analysis itself, as differentiated from the trials it summarizes, can be biased. The two primary forms of this bias are publication bias and outcome reporting bias. Publication bias results from the likelihood that trials with statistically significant results are more likely to be published. Outcome reporting bias results when researchers report only the significant results from a trial and leave unsignificant outcomes unreported. Discuss how you will guard against these types of bias.</p>
'
r = saveP(order, title, helpText)

order="Dj"
title="Confidence in Cumulative Evidence"
helpText='
<p><i>Authors should describe which approach they plan on using to summarize the confidence they have in the resulting body of evidence, ideally using an established and validated approach. The description should include a plan for assessing the risk of bias across studies, inconsistency, imprecision, indirectness, publication bias, and factors that increase the confidence in an effect (such as large effects, dose effect relations, and issues around opposing bias and confounding not explaining an effect or lack thereof) for each outcome that is included in the PICO.</i> <a href="https://mayoclinic.pure.elsevier.com/en/publications/grade-guidelines-11-making-an-overall-rating-of-confidence-in-eff">The Grading of Recommendations Assessment, Development and Evaluation (GRADE) approach</a> <i>is increasingly recommended,</i> (<a href="https://www.bmj.com/content/bmj/349/bmj.g7647.full.pdf" target="_blank" rel="noopener">Shamseer et al., 2015</a>).</p>
'
r = saveP(order, title, helpText)

order="E"
title="Activate Project"
helpText='
<p>When all of the edit buttons are green (after you\'ve entered <i>something</i> for every item, even <b>Not Applicable</b>), an <i>Activate Project</i> button will appear below. But don\'t click it until you\'re entirely happy with your protocol. Until you click <i>Activate Project</i> you can still edit your protocol (no matter whether the button is purple or green). But once your project is active, you\'ll only be able to edit the top two sections, <b><i>Amendments</i></b> and <b><i>Publications</i></b>.</p>
'
r = saveP(order, title, helpText)

   return("Initialization of database is complete...")
}



