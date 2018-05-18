# open-meta.app
R-Shiny-SQL-based web app for crowd-sourced systematic reviews and meta-analyses.

The vision for this app is that it will support the entire process of creating a systematic review and meta-analysis, from writing a project protocol based on the PRISMA-P guidelines, to uploading citations from academic databases, through stage 1 review, data extraction, and statistical analysis, to downloading graphics, tables, and bibliographies for publication. It should be both useful in itself as a tool and useful for teaching best practices in reviewing and synthesizing research.


Although this project is dependent on a large number of R packages, it isn't itself a package - it's a multi-user app that requires an internet host running Shiny Server. It also requires a compatible SQL database and an email connection. You can see what the current version looks like, is capable of, and where it's going at [dev.open-meta.org/app](http://dev.open-meta.org/app).

However, if you are interested in helping with the code, you can run a development version on your own computer. In addition to the standard dependencies on R and Shiny, you'll also need a database like [MySQL](https://dev.mysql.com/downloads/mysql/) or [MariaDB](https://downloads.mariadb.org/).

### To get started...
...load the *credentials.R* file and fill in a password for the app's **Admin** superuser account and three passwords for the database. The first of these needs to be the actual root password to your database, which you'll create when you set the database up. The other two passwords will be used when the app initializes the database with two additional accounts, but you don't need to set up those two accounts in advance.

You can leave the email setup blank; in this case, when the app sends emails the subject and body will appear on-screen in a modal dialog so you can see what happened, but no email will actually be sent.

After saving the *credentials.R* file, load the *app.R* file into RStudio and click the *Run App* button.

During development you can make changes to most pages and then just reload that page to see your changes rather than restarting the app. This applies to all files except *app.R* itself and the files it loads as global functions: *credentials.R, sql-core.R, sql-initialization.R, and bs4.R*. 

### Helpful development tools
If you get involved in this project, there are three development tools I find extremely useful:

* **Printing to the console**. When *debugON* is TRUE, you can follow what the code is doing in your console. When I'm trying to work out a bug I add additional print() statements to the code to find out whether a section of code ran or to discover the value of variables at specific points in the code.
* **Google's Chrome browser** has an amazing window that's really helpful for finding and fixing HTML, CSS, and JavaScript errors. To open it, right click on an element on your page and select the **Inspect** option.
* **[HeidiSQL](https://www.heidisql.com/)** is both cool and indispensible for seeing what was actually saved in your SQL database and to make changes directly to the database for testing purposes.

### Package notes
Right now the app uses the following packages:

* shiny
* tidyverse
* pool
* RMariaDB
* mailR
* bcrypt     (password encryption)
* DT         (javascript tables)
* RefManageR (BibTeX library)

I don't expect to write any statistical software to calculate effect sizes or do meta-analysis but to use existing packages. There is a terrific list of existing R packages related to meta-analysis, maintained by Michael Dewey, at [CRAN Task View: MetaAnalysis](https://cran.r-project.org/web/views/MetaAnalysis.html).

***

This project is part of a dissertation for a doctoral degree. Typically dissertations have to be completely done by the student, but since this is an open-source project, my advisors have encouraged me to share the code before the project is completed and to be open to pull requests. I have a project notebook at [www.open-meta.org](http://www.open-meta.org) that includes some excruciating detail about setting up an Amazon Lightsail instance to run the app in the cloud as well as an introductory [Shiny tutorial](http://www.open-meta.org/technology/a-totally-different-read-me-first-shiny-tutorial/), if that package for web-enabling R is new to you.
