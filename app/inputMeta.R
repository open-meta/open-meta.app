### open-meta.app input-meta.R
### Tom Weishaar - Sep 2018 - v0.1

# This file provides support functions for creating, editing, and saving FORMs (used by adminForms.R and prjAdmin.R)
#    Additional functions support opening and displaying a FORM and saving its data (used by Extract.R etc.)
#    At the moment, the code supports text, quill, numeric, select (dropdown), radio button, and checkbox inputs.
#       A FORM is a tibble. Each row describes an input.
#       A FORMrow describes one input in the FORM.
#       A "form" is a tibble that describes one input. You can also think of it as a FORM that collects the data for a FORMrow.
#       Input "forms" are loaded into memory by input-meta.R.
#       FORMs are JSONized and stored in the the om$prime or project settings table.
#       Data collected by a FORM is stored in either a standard table or a name-value table (required for project-custom data).
#       Each FORMrow includes "table" and "column" fields describing where that data item should be stored.
#          (For name-value tables, the FORMrow's "name" field equates to the table's "name" field and "column" is not used.)

# A function with "Form" in the name can handle either a FORM or a form tibble.
# Some FORMs can be customized by project administrators to add inputs for whatever data they want to collect.

# Some initializations
S$IN$inputType <- 1
rv$imGetFORMData <- 0
#S$IN$FORMrowform <- imGetBlankform(S$IN$codeTypes[S$IN$inputType])

S$IN$userTypes <- c("Simple text", "Text editor", "Numeric", "Select (dropdown)", "Radio buttons", "Checkboxes")
S$IN$codeTypes <- c("text", "quill", "number", "select", "radio", "checkbox")

# Load the shared imGetBlankFORMrow function
source("inputMetaAux.R", local=TRUE)  # This will create a default one-row tibble with the columns of a FORMrow

# Load the list of form defintions into memory.
# This list is created by the STAND-ALONE-CREATE-Blankforms-list.R file,
imBlankforms <- readRDS(file="Blankforms.RDS")

# These forms are essentially FORMs describing the inputs needed to collect the data for one input, except they are memory-based
#   and never saved by inputMeta.R. To edit them, use STAND-ALONE-CREATE-Blankforms-list.R
imGetBlankform <- function(type) {            # type refers to the type of input. See S$IN$codeTypes
   return(imBlankforms[[type]])
}

# Runs when Project->Members & Settings->Customized Inputs->type is selected; also called by Extract.R etc.
# Load a FORM from the settings file if there is one; otherwise load a FORM with the right columns but no rows.
#    Result on return is typically saved to S$IN$FORM. Also sets S$IN$FORMname global under the assumption that
#    only one FORM is ever open at a time.
imGetFORM = function(FORMname) {
   S$IN$FORMname <<- FORMname                     # imSaveform2FORMrow() will need this
   r <- recGet(S$db, "settings", "value", tibble(c("name", "=", S$IN$FORMname)))
   if(r$value=="") {                              # Did recGet return anything?
      f <- imGetBlankFORMrow("blank")[-1,]        # No, it's a new form. Build a zero-row tibble with default input parameters
   } else {
      f <- as.tibble(fromJSON(r$value))           # Yes, it's an existing form. UnJSONize the value as it's a tibble
   }
   return(f)
}

# This handles the top of the customization page, where inputs are added and edited.
# A button changes it from a "Add and input" mode to an "Editing input" mode.
# In editing mode, there are two sections and the embedded second section is reactive to
#    the input type selector in the first section.
imModifyInputs <- function() {
   if(S$IN$flag$showAddInputButton) {             # Show button or inputs?
      return(tagList(
         bs4("btn", id="addInput", q="g", "Add customized input"),
         bs4("hr")
      ))
   } else {
      ir = imGetBlankFORMrow("select")            # This builds the type-of-input selector
      ir$id = "inputType"
      ir$value = S$IN$userTypes[S$IN$inputType]
      ir$options = paste0(S$IN$userTypes, collapse=";")
      ir$label = "Type of Input"
      ir$width = "25%"
      ir$inline = FALSE
      if(S$IN$flag$editingForm) {                 # Disable the type selector when editing (rather than creating) a FORMrow
         ir$disabled = TRUE
      }
      return(tagList(
         imForm2HTML(ir),                         # This displays the type-of-input selector
         output$modifyAnInput <- renderUI(imModifyAnInput()),         # This displays the form for collecting info about that input
         HTML('<div class="text-right mt-3">'),
         bs4("btn", id="cancelInput", n=1, q="b", "Cancel"),
         bs4("btn", id="saveInput", n=1, q="b", "Save"),
         HTML('</div>'),
         bs4("hr")
      ))
   }
}

# REACTIVE - This is called each time its embedded input$inputType selector is changed because it's called
#    by a render function inside imModifyInputs(). It displays the form for creating/editing a FORMrow
imModifyAnInput <- function() {
   r = ""
   if(S$IN$flag$editingForm || is.null(input$inputType)) {
      r = imForm2HTML(S$IN$FORMrowform)           # S$IN$FORMrowform is set up by click observer for "addInput" and "editMe"
   } else {
      S$IN$inputType <<- which(S$IN$userTypes %in% input$inputType)   # Get current inputType selector setting
      # Save current values and insert in form?
      r = imForm2HTML(imGetBlankform(S$IN$codeTypes[S$IN$inputType])) # Display a form of that type
   }
   return(HTML(r))
}

# This handles the bottom of the page where current inputs are displayed
# REACTIVE to input$view, the embedded "Look and Feel" vs "Action Buttons" radio buttons
imShowInputs <- function() {
   S$IN$view <<- ifelse(is.null(input$view), S$IN$view, input$view)
   ir = imGetBlankFORMrow("radio")
   ir$id = "view"
   ir$name = "view"
   ir$value = S$IN$view
   ir$options = "Look and Feel; Action Buttons"
   ir$label = "Show"
   showWhatBut = ""
   if(S$IN$flag$showAddInputButton) {             # only show the Look and Feel/Action Buttons choice when NOT editing
      showWhatBut = imForm2HTML(ir)
   }
   if(S$IN$view=="Look and Feel") {
      r <- tagList(
         HTML0("<h5>Current Look</h5>"),
         showWhatBut,
         bs4("hr0", class="pb-3"),
         imForm2HTML(S$IN$FORM),
         bs4("hr0", class="pb-4")
      )
   } else {
      m = ""
      nr = nrow(S$IN$FORM)
      if(nr > 0) {                                                    # A new FORM will have no rows
         for(i in 1:nr) {
            if(!S$IN$flag$showAddInputButton || !S$P$Modify ||        # Don't show buttons if modifying an input,
                        (S$IN$FORM$locked[i] && !S$P$SA)) {           #    without permission, or if locked and not Sys Admin
               m = paste0(m, imForm2HTML(S$IN$FORM[i,]))
            } else {
               uq = dq = "b"
               if(i==1)  { uq = c("b", "X") }                         # Disable first moveup button
               if(i==nr) { dq = c("b", "X") }                         #    and last movedown button
               m = paste0(m, imForm2HTML(S$IN$FORM[i,]),
                  bs4("btn", id="upMe", n=i, q=uq, class="mb-2", "Move up"),
                  bs4("btn", id="editMe", n=i, q="g", class="mb-2", "Edit"),
                  bs4("btn", id="downMe", n=i, q=dq, class="mb-2", "Move down"),
                  bs4("btn", id="deleteMe", n=i, q="r", class="mb-2", style="float:right;", "Delete"),
                  bs4("hr", class="py-3")
                  )
            }
         }
      }
      r <- tagList(
         HTML0("<h5>Current Look</h5>"),
         imForm2HTML(ir),
         bs4("hr0", class="pb-3"),
         HTML0(m),
         bs4("hr0", class="pb-4")
      )
   }
   return(r)
}

# Used to edit a FORM. Loads a form with values from a FORMrow.
# Accepts one row of a FORM as a parameter, then
#    a.) gets a blank form corresponding the FORMrow's input type
#    b.) loops through the form's inputs, collecting values from the FORMrow's relevant parameters.
#    c.) returns the form
imFORMrow2form <- function(FORMrow) {
   form <- imGetBlankform(FORMrow$type)           # Get the correct kind of input form
   for(i in 1:nrow(form)) {                       # Loop through the form
      id <- form$id[i]
      if(id=="other" || id=="direction") {        # These ids need special handling, as they are not columns in FORMrow
         if(id=="direction") {
            v <- ifelse(FORMrow$inline, "across the page", "in a column")
         } else {
            v <- c("sameline"[FORMrow$sameline], "disabled"[FORMrow$disabled], "locked"[FORMrow$locked])  # a vector
         }
         form[i, "value"] <- paste0(v, collapse=";")
      } else {
         if(length(FORMrow[[form$id[i]]]>0)) {    # Needed to prevent a crash if you modify an input form
            form[i, "value"] <- FORMrow[[form$id[i]]]                 # form$id[i] is a column name in FORMrow
         }
      }
   }
   return(form)
}

# This converts either a FORM or a form to an HTML form
imForm2HTML <- function(Form) {
   r = '<form><div class="form-row">'             # This div will be closed by the first input; just a hack to give
   if(nrow(Form)>0) {                             #   each input the chance to close the previous row and start a new one.
      for(i in 1:nrow(Form)) {
         thisr = imFormRow2HTML(Form[i,])
         r = paste0(r, thisr)
      }
   }
   return(HTML0(r, "</div></form>"))              # This /div closes the last row of the form
}

# Here we arrive at the heart of the matter.
# This is the code that creates a single HTML input based on a row from a Form.
#   Called repeatedly, once for each input in a Form.
imFormRow2HTML = function(tr) {
# Initialize variables that will be pasted together or are otherwise needed
   options = tr$options
   value = tr$value
   if(any(c("select", "radio", "checkbox") %in% tr$type)) {
      options = str_trim(unlist(str_split(options, ";")))             # vectorize and trim
      value = str_trim(unlist(str_split(value, ";")))                 # vectorize and trim
   }
   if(tr$type=="checkbox") {
      if(S$P$SA && str_sub(tr$id,1,2)!="id") {                        # if id doesn't start with "id" its a form, not a FORM
         options = c(options, "locked")                               # System Admin also gets the locked option
         if(tr$locked){
            value = c(value, "locked")
         }
      }
   }
   type = paste0(' type="', tr$type, '"')
   id = paste0(' id="', tr$id, '"')
   label = paste0('<label class="mb-2" for="', tr$id, '">', tr$label, '</label>\n')
   width = switch(tr$width,                                           # These are bs4 column widths (n/12)
      "25%" = "3",
      "33%" = "4",
      "50%" = "6",
      "66%" = "8",
      "75%" = "9",
      "100%" = "12",                                                  # Final line provides support for standard 1 to 12 widths.
      ifelse(tr$width=="", "12", tr$width)                            #   Default for no width provided is 12 (100%)
   )
   class = ifelse(tr$inline, " inline", "")
   valueP = ifelse(nchar(value[1])>0, paste0(' value="', value, '"'), '')
   placeholder = ifelse(tr$placeholder=="", "", paste0(' placeholder="', tr$placeholder, '"'))
   min =       ifelse(tr$min=="", "", paste0(" min=", tr$min))        # These are numbers, so no quotes like placeholder
   max =       ifelse(tr$max=="", "", paste0(" max=", tr$max))
   step =      ifelse(tr$step=="", "", paste0(" step=", tr$step))
   maxlength = ifelse(tr$maxlength=="", "", paste0(" maxlength=", tr$maxlength))
   disabled =  ifelse(tr$disabled, " disabled", "")                   # Same as readonly but doesn't allow a click to select
   optionsCode = ""                                                   # Initialize for checkbox-radio-select
   ariaD = paste0(' ariaDescribedby="aria', tr$id,'"')

   if(tr$id=="name" && S$IN$flag$editingForm) {                # No user, even SysAdmin, can change the "name" field when editing a form.
      disabled = " disabled"                                   #   To allow this, we'd also have to muck around in the "ids" table
   }                                                           #   User will need to delete that input and start over.

### row-col-label                                              # imForm2HTML() starts with '<form><div class="form-row">'
   r=paste0('', '</div><div class="form-row">'[!tr$sameline])  #    and ends with '</div></form>. If the first FORMrow has the expected
   r=paste0(r, '<div class="form-group col-', width, '">')     #    sameline=FALSE, this code will create a blank and invisible row. On
                                                               #    the other hand, if sameline=TRUE in the first input by mistake, this
                                                               #    code forgives that error and the first input goes in the row that's
                                                               #    already open. After the first row, sameline=FALSE closes the existing
                                                               #    row and starts a new one; sameline=TRUE does not. In any case, each
                                                               #    input gets its own column with a width determined by the "Width of
                                                               #    this input" % setting converted to columns above.
### text-password-number
   if(any(c("text", "password", "number") %in% tr$type)) {
      class = paste0(' class="form-control ', class, '"')
      r <- paste0(r, label, '<input', type, id, class, valueP, placeholder, min, max, step, maxlength, disabled, ariaD, '>\n')
   }

### select
   if("select" %in% tr$type) {
      s = options %in% value                      # a T/F vector
      for(i in 1:length(options)) {
         optionsCode <- paste0(optionsCode, '<option', ' selected'[s[i]],'>', options[i], '</option>')
      }

r <- paste0(r, label,
'
<select id="', tr$id, '" class="form-control ', class ,'"', disabled, ariaD, '>', optionsCode, '</select>
')
   }

### checkbox/radio
   # Disable all checkboxes, but only unchecked radio buttons (for visual clarity)
   if(any(c("checkbox", "radio") %in% tr$type)) {
      if(options[1]!="") {                        # if there are no options at all, the options vector will have a blank first option
         for(i in 1:length(options)) {
            optionsCode = paste0(optionsCode,
'      <div class="form-check', ' form-check-inline'[tr$inline], '">
         <input class="form-check-input" type="', tr$type, '" name="', tr$id ,'" id="', paste0(tr$id,i),
            '" value="', options[i], '"', ' checked="checked"'[options[i] %in% value],
            disabled[tr$type=="checkbox" || (tr$type=="radio" && !(options[i] %in% value))], '/>
         <label class="form-check-label" for="', paste0(tr$id,i) ,'">', options[i], '</label>
      </div>')
         }
r <- paste0(r, '
<div id="', tr$id, '" class="shiny-input-', tr$type, 'group shiny-input-container', '-inline'[tr$inline], '"', ariaD, '>
   ', label, '
   <div class="shiny-options-group">',
optionsCode, '
   </div>
</div>
')
      }
   }

### quill
   if("quill" %in% tr$type) {
      r <- paste0(r, label,
               as.character(bs4("quill", id=tr$id, value=value, placeholder=tr$placeholder,
                  ariaD=ariaD, disabled=tr$disabled)))                # as.character to convert from tagList()
   }

### helptext and return
   helptext <- ifelse(tr$helptext=="", "",
      paste0('<small class="form-text text-muted" id="aria', tr$id, '">', tr$helptext, '</small>'))
   return(paste0(r, helptext, "</div>"))          # Close column. (This function should return text. Caller will HTML it.)
}

# Called before saving a form to a FORMrow to make sure everything is copacetic.
#    Makes sure the short label is unique.
#    Checks for all required fields.
#    For select-radio-checkbox make sure value is in options
imFormValidates <- function() {
   msg=""
   if(!S$IN$flag$editingForm) {                                                # Only need to validate the name for new inputs
      name <- str_trim(stripHTML(as.character(input[["name"]])))               # trim and strip HTML from input$name
      if(name=="") {
         msg="<li>The Short name for this input can't be blank.</li>"
      } else {
         dbLink <- poolCheckout(shiny.pool)                                    # get a dbLink from the pool
         on.exit(poolReturn(dbLink), add = TRUE)                               # Have to use dbLink because ids is a non-standard table
         r = dbGetQuery(dbLink, paste0("SELECT idsID FROM `", S$db, "`.`ids` WHERE idAsName='", name, "';"))    # dbExecute returns # of rows
         s = dbGetQuery(dbLink, paste0("SELECT idsID FROM `om$prime`.`ids` WHERE idAsName='", name, "';"))      # check both id tables
         if(nrow(r)!=0 || nrow(s)!=0) {                                        # dbGetQuery, returns a table with no rows if not found
            msg = "<li>Short names must be unique and that name is already taken.</li>"
         }
      }
   }
   table <- str_trim(stripHTML(as.character(input[["table"]])))
   column <- str_trim(stripHTML(as.character(input[["column"]])))
   label <- str_trim(stripHTML(as.character(input[["label"]])))
   if(table=="") {
      msg = paste0(msg,"<li>The Data table for this input can't be blank.</li>")
   }
   if(column=="") {
      msg = paste0(msg,"<li>The Data column for this input can't be blank.</li>")
   }
   if(label=="") {
      msg = paste0(msg,"<li>The Long name for this input can't be blank.</li>")
   }
   formtype <- S$IN$codeTypes[S$IN$inputType]
   if(formtype %in% c("select", "radio", "checkbox")) {
      options <- str_trim(stripHTML(as.character(input[["options"]])))         # trim and strip HTML from input$options
      options <- ifelse(str_sub(options,-1,-1)==";", str_sub(options,1,-2), options) # remove trailing ";"
      options <- str_trim(unlist(str_split(options, ";")))                     # vectorize and retrim
      value <- str_trim(stripHTML(as.character(input[["value"]])))             # trim and strip HTML from input$value
      value <- ifelse(str_sub(value,-1,-1)==";", str_sub(value,1,-2), value)   # remove trailing ";"
      value <- str_trim(unlist(str_split(value, ";")))                         # vectorize and retrim
   }
   if(formtype=="select") {
      if("" %in% options) {
         msg = paste0(msg,"<li>You have a blank dropdown option.</li>")
      }
      # if length of value == 0, the selector will default to the first option; not recommended but allowed
      if(length(value)==1 && value!="" && !(value %in% options)) {
         msg = paste0(msg,"<li>The selected option, <i>", value, "</i>, isn't an option name.</li>")
      }
      if(length(value)>1) {
         msg = paste0(msg,"<li>You can't have more than one selected option.</li>")
      }
   }
   if(formtype=="radio") {
      if("" %in% options) {
         msg = paste0(msg,"<li>You have a blank radio button name.</li>")
      }
      if(length(options)<2) {
         msg = paste0(msg,"<li>You must provide at least two radio button names. Separate them with a semicolon.</li>")
      }
      if("" %in% value) {
         msg = paste0(msg,"<li>You must have a selected radio button.</li>")
      }
      if(length(value)==1) {
         if(!(value %in% options)) {
            msg = paste0(msg,"<li>The selected radio button, <i>", value, "</i>, isn't a radio button name.</li>")
         }
      }
      if(length(value)>1) {
         msg = paste0(msg,"<li>You can't have more than one selected radio button.</li>")
      }
   }
   if(formtype=="checkbox") {
      if(value!="") {     # Continue only if something is checked. It's ok if nothing is checked.
         for(s in value) {
            if(!s %in% options) {
               msg = paste0(msg,"<li>Checked checkbox <i>", s, "</i> isn't a checkbox name.</li>")
            }
         }
      }
   }
   if(msg=="") {
      return(TRUE)
   } else {
      S$modal_title <<- "Whoops"
      S$modal_text <<- HTML("<p>Can't save form because:<ul>", msg, "</ul></p>")
      rv$modal_warning <- rv$modal_warning + 1
      return(FALSE)
   }
}

# Used to save an input described in a form to a FORM after the form validates.
#    Put a form's values in a pre-existing FORMrow, put that in a FORM, and save the FORM
#       to the settings file (see below for saving FORM DATA!)
#    In general, the row ids of a form match the column names in a FORMrow
#       However, form id "direction" collects data that needs to be converted and stored as T-F in
#         FORMrow column "inline" while form id "other" collects data that needs to be converted
#         and stored as T-F in FORMrow columns "sameline", "disabled", and "locked". Note that we're
#         pulling the form's values out of input[[id]].
imSaveform2FORMrow <- function() {
   FORMrow <- S$IN$FORMrow                    # Get edited row (pre-exisiting or blank; set up by addInput or editMe)
   formtype <- S$IN$codeTypes[S$IN$inputType] # Note that if we are editing a FORMrow, the inputType can't be changed,
   FORMrow$type <- formtype                   #   but if it's a new form it can, so we can't depend on FORMrow's $type
   if(!S$IN$flag$editingForm) {               # For new forms, change form$name into a utf8 id and save it in the ids table
      dbLink <- poolCheckout(shiny.pool)                         # get a dbLink from the pool
      on.exit(poolReturn(dbLink), add = TRUE)                    # return it when done, even if there's an error
      name <- str_trim(stripHTML(as.character(input[["name"]]))) # trim and strip HTML from input$name
      id <- paste0("id", paste0(utf8ToInt(name), collapse=""))   # create valid id syntax for both R and JavaScript
      FORMrow$id <- id                                           # save the new id back into the FORMrow
      r = dbExecute(dbLink, paste0("INSERT INTO `", S$db, "`.`ids` VALUES ('",
                                   id, "', '", S$IN$FORMname, "', '", name, "');"))
      ## if(r!=1), insert failed; however, imFormValidates() checked for uniqueness just milliseconds ago
   }
   form <- imGetBlankform(formtype)                              # Get the form for this type of FORMrow; we need its ids
   for(id in form$id) {                                          # Loop through the form ids; some, but not all, are columns in FORMrow
      rawI <- str_trim(stripHTML(input[[id]]))                   # trim and strip HTML from input$
      if(id=="options" && formtype %in% c("select", "radio", "checkbox")) {   # Special handling for Select, Radio, and Checkbox options
         rawI <- ifelse(str_sub(rawI,-1,-1)==";", str_sub(rawI,1,-2), rawI)   # remove a trailing ";" from option list
      }
      if(id %in% c("direction", "other")) {                      # Special handling for ids "direction" and "other"
         if(id=="direction") {
            FORMrow$inline <- "across the page" %in% rawI        # Input is character or character vector (checkbox with multiple checks)
         } else {
            FORMrow$sameline <- "sameline" %in% rawI             # FORMrow table has logical vectors
            FORMrow$disabled <- "disabled" %in% rawI
            FORMrow$locked <- "locked" %in% rawI
         }
      } else {
         FORMrow[1,id] <- ifelse(is.na(rawI), "", rawI)          # NA happens when min, max, step, or maxlength are empty. stripHTML()
      }                                                          #   converts as.character(NA) to "", but returns numeric as is
   }
   if(nrow(S$IN$FORM)==0) {                                      # To rbind or not to rbind, that is the question.
      S$IN$FORM <<- FORMrow
   } else {
      if(FORMrow$order==999) {                                   # Add an additional row to an existing tibble
         S$IN$FORM <<- rbind(S$IN$FORM, FORMrow)                 # $order=999 means it's a new FORMrow
      } else {
         S$IN$FORM[S$IN$FORM$order==FORMrow$order,] <<- FORMrow  # Update an existing row in an exisiing tibble
      }
   }
   imSaveFORM()
}

# Saves S$IN$FORM into the S$db.settings file. For adminForms.R that's om$prime, for prjAdmin.R it's the project's db
#    This is separated from imSaveform2FORMrow because it's also used after moveup, movedown, and delete
#    Note that this is used to save a FORM, but NOT the data collected by the form. For that, see rv$imGetFORMData below.
imSaveFORM = function() {
   r <- recGet(S$db, "settings", "**", tibble(c("name", "=", S$IN$FORMname)))
   r$name[2] <- S$IN$FORMname                                    # In case this is a new FORM and recGet gave us a blank record
   if(nrow(S$IN$FORM)==0) {
      r$value[2] <- ""                                           # JSON won't save colnames if there are no rows
   } else {
      S$IN$FORM$order <<- 1:nrow(S$IN$FORM)                      # Re-number the $order vector, getting rid of all 999s & .5s
      r$value[2] <- toJSON(S$IN$FORM)                            # FORM is a tibble, so we have to JSONize it
   }
   r = recSave(r, S$db)
}

# This is called by adminForms.R and prjAdmin.R to move FORMrows up or down within a FORM.
imFixOrder <- function() {
   S$IN$FORM <<- S$IN$FORM[order(S$IN$FORM$order),]
   return(imSaveFORM())                                          # imSaveFORM will renumber the $order vector
}

# Get the groupNUM, based on the recID
imID2NUM <- function(ID, TABLE) {
   SELECT = paste0(TABLE,"NUM")
   tableID = paste0(TABLE,"ID")
   if(ID>0) {                                                                 # already exists, get it
      R <- recGet(S$db, TABLE, SELECT, WHERE=tibble(c(tableID, "=", ID)))
      NUM <- as.numeric(R[[SELECT]])
   } else {
      R <- recGet(S$db, TABLE, SELECT, WHERE=tibble(c(tableID, ">", 0)))
      if(nrow(R)!=0) {
         NUM <- max(as.numeric(R[[SELECT]])) + 1                              # It's the max of tableNUM plus 1
      } else {
         NUM <- 1                                                             # But for first one it's 1
      }
   }
   return(NUM)
}

# Run imGetFORMData by incrementing rv$imGetFORMData
# First saves values of inputs into $values column of S$IN$FORM, then into proper type of SQL table
S$IN$flag$imSAVE <- "start"
observeEvent(c(input$js.editorText, rv$imGetFORMData), {
   if(rv$imGetFORMData>0 && nrow(S$IN$FORM)>0 && S$P$Modify) {        # Don't actually run this until we get an increment and then only
      if(S$IN$flag$imSAVE=="start") {                                 #   if there are rows in FORM and the user has modify permission.
         S$IN$flag$imSAVE <<- "finish"                                # First deal with Quill inputs, if any
         QTF <<- S$IN$FORM$type=="quill"                              # TF vector of rows with quill inputs
         S$IN$Qs <<- which(QTF)                                       # Row numbers of quill inputs
         S$IN$Qn <<- sum(QTF)                                         # Number of quill inputs
         S$IN$Qindex <<- 1                                            # Index to quill inputs
         if(S$IN$Qn>0) {
            return(js$getEdit(S$IN$FORM$id[S$IN$Qs[S$IN$Qindex]]))    # This will start us over at top
         }
      }
      if(S$IN$flag$imSAVE=="finish") {
         if(S$IN$Qn>0) {                                              # Are we recovering Quill data?
            S$IN$FORM$value[S$IN$FORM$id==input$js.editorText[1]] <<- # Yes, use the input id to save the
               input$js.editorText[2]                                 #   edited text in $value; Quill
         }                                                            #   itself does the HTML security strip
         S$IN$Qindex <<- S$IN$Qindex + 1                              # Increment quill index
         if(S$IN$Qindex <= S$IN$Qn) {                                 #    and contine until there are no more Quill ids
            return(js$getEdit(S$IN$FORM$id[S$IN$Qs[S$IN$Qindex]]))    # This will start us over at top
         }
         ids = S$IN$FORM$id                                           # now get the regular inputs
         if(S$IN$Qn>0) {
            ids[-S$IN$Qs]                                             # Remove quill ids, if any
         }
         for(i in 1:length(ids)) {
            if(is.null(input[[ids[i]]])) {                            # input[[id]] can be NULL for checkboxes
               rawI <- ""                                             #   where nothing is checked
            } else {
               rawI <- str_trim(stripHTML(as.character(input[[ids[i]]]))) # trim and strip HTML from input$
            }
            S$IN$FORM$value[i] <<- ifelse(is.na(rawI), "", rawI)      # NA happens when a numeric input is empty
         }
      }
      S$IN$flag$imSAVE <<- "start"                                    # Clean up for next run

      # At this point we have a FORM with the new inputs saved in the values column. But actually saving the FORM
      #   is not what we want here. Instead, we now save the FORM values in the appropriate table, which can be
      #   either a name-value table or a standard row-column table. Then we throw the FORM away as saving it
      #   would change the FORM's default values!

      dataTable <- S$IN$FORM[[1,"Table"]]                             # Name of SQL table is stored in FORM$Table
      if(dataTable %in% c("settings", "pico")) {                      # If it's a name-value table with one NUM field
         NUM <- imID2NUM(S$IN$recID, dataTable)                       #    We'll need this for each row, so save in variable
         tableNUM <- paste0(dataTable,"NUM")
         for(i in 1:nrow(S$IN$FORM)) {                                # Save FORM values
            R <-  recGet(S$db, dataTable, SELECT="**",
                     WHERE=tibble(c(paste0(dataTable, "NUM"), "=", NUM), c("name", "=", S$IN$FORM$name[i])))
            R[[tableNUM]][2] = NUM                                    # While edited recs will already have NUM
            R$name[2] = S$IN$FORM$name[i]                             #    and Name, must do this for new rows!
            R$value[2] = S$IN$FORM$value[i]
         }
      }
      if(dataTable=="extract") {                                      # More NUM fields to fill out
          warning("At the bottom of inputMeta.R, the code for filling out the extra NUM fields in an Extract table is incomplete.")
      }
      if(!dataTable %in% c("settings", "extract", "pico")) {          # It's a standard table
         warning("At the bottom of inputMeta.R, the code for saving FORM data into a standard table is incomplete.")
         R <-  recGet(S$db, dataTable, SELECT="**", WHERE=tibble(c(paste0(dataTable, "ID"), "=", S$IN$recID)))
         for(i in 1:nrow(S$IN$FORM)) {
            R[[i, S$IN$FORM$column]] <- S$IN$FORM$value[i]            # Move data from rows to columns
         }
      }
      R <- recSave(R, S$db)                                           # Save it
      rv$limn = rv$limn+1                                             # Re-render
   }
})

