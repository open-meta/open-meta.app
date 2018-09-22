### open-meta.app input-meta.R
### Tom Weishaar - Sep 2018 - v0.1

# This file provides support functions for creating and displaying bs4 inputs, including
#   text, quill, numeric, dropdown selectors, radio buttons, and checkboxes.

# A FORM is a tibble. Each row describes an input.
# A FORM can be customized by project administrators to add inputs for whatever data they want to collect.
# A FORMrow describes one input in the FORM
# FORMs themselves are JSONized and stored in the project's settings table.
# FORM **data** is stored in the an associated table (output-trial-arm-group) in a num-name-value format.

# A "form" is likewise a tibble with a row for each input.
# Each input in a "form" collects data for one FORMrow.

# A function with "Form" in the name can handle either a FORM or a form tibble.

### Create customized inputs

S$IN$userTypes <- c("Simple text", "Text editor", "Numeric", "Select (dropdown)", "Radio buttons", "Checkboxes")
S$IN$codeTypes <- c("text", "quill", "number", "select", "radio", "checkbox")

# Load the shared imGetBlankFORMrow function
source("inputMetaAux.R", local=TRUE)  # This will create a default one-row tibble with the columns of a FORMrow

# Load the list of Blankform defintions.
# This list is created by the STAND-ALONE-CREATE-Blankforms-list.R file,
imBlankforms <- readRDS(file="Blankforms.RDS")

# These forms are essentially FORMs describing the inputs needed to collect the data for one input
imGetBlankform <- function(type) {            # type refers to the type of input. See S$IN$codeTypes
   return(imBlankforms[[type]])
}

# Some initializations
S$IN$inputType <- 1
S$IN$FORMrowform <- imGetBlankform(S$IN$codeTypes[S$IN$inputType])

# Runs when Project->Members & Settings->Customized Inputs->type is selected
# Load S$IN$FORM with a FORM from the settings file if there is one;
#    otherwise load a FORM with the right columns but no rows. Also sets two table-name-related globals.
imGetFORM = function(myTable) {
   S$IN$tableName <<- myTable                              # Need this to save an edited FORM later
   S$IN$settingsName <<- paste0("inputForm-", myTable)     # imSaveform2FORMrow() will need this too
   r <-recGet(S$db, "settings", "value", tibble(c("name", "=", S$IN$settingsName)))
   if(r$value!="") {                                       # did recGet return anything?
      S$IN$FORM <<- as.tibble(fromJSON(r$value))           # Yes, unJSONize as it's a tibble
   } else {
      S$IN$FORM <<- imGetBlankFORMrow("blank")[-1,]        # No, build a zero-row tibble with default input parameters
   }
}

# This handles the top of the customization page, where inputs are added and edited
# Not reactive itself, but an embeddeed second section IS reactive,
#    changing based on the input type selector in the first section
imModifyInputs <- function() {
   if(S$IN$flag$showAddInputButton) {           # Show button or inputs?
      return(tagList(
         bs4("btn", id="addInput", q="g", "Add customized input"),
         bs4("hr")
      ))
   } else {
      ir = imGetBlankFORMrow("select")
      ir$id = "inputType"
      ir$value = S$IN$userTypes[S$IN$inputType]
      ir$options = paste0(S$IN$userTypes, collapse=";")
      ir$label = "Type of Input"
      ir$width = "25%"
      ir$inline = FALSE
      if(S$IN$flag$editingForm) {              # Disable the type selector when editing (rather than creating)
         ir$disabled = TRUE                    #    a FORMrow
      }
      return(tagList(
         imForm2HTML(ir),
         output$modifyAnInput <- renderUI(imModifyAnInput()),
         HTML('<div class="text-right mt-3">'),
         bs4("btn", id="cancelInput", n=1, q="b", "Cancel"),
         bs4("btn", id="saveInput", n=1, q="b", "Save"),
         HTML('</div>'),
         bs4("hr")
      ))
   }
}

# REACTIVE - This runs each time the input$inputType selector is changed
# This is called by a render function innside imModifyInputs() to display the form for creating/editing a FORMrow
imModifyAnInput <- function() {
   r = ""
   if(S$IN$flag$editingForm || is.null(input$inputType)) {
      r = imForm2HTML(S$IN$FORMrowform)   # S$IN$FORMrowform is set up by click observer for "addInput" and "editMe"
   } else {
      S$IN$inputType <<- which(S$IN$userTypes %in% input$inputType)         # Get current inputType selector setting
      # Save current values and insert in form?
      r = imForm2HTML(imGetBlankform(S$IN$codeTypes[S$IN$inputType]))       # Display a form of that type
   }
   return(HTML(r))
}

# This handles the bottom of the page where current inputs are displayed
# REACTIVE to input$view, the embedded "Look and Feel" vs "Action Buttons" radio buttons
imShowInputs <- function() {
#print("Running imShowInputs")
   S$IN$view <<- ifelse(is.null(input$view), S$IN$view, input$view)
   ir = imGetBlankFORMrow("radio")
   ir$id = "view"
   ir$value = S$IN$view
   ir$options = "Look and Feel; Action Buttons"
   ir$label = "Show"
   showWhatBut = ""
   if(S$IN$flag$showAddInputButton) {              # only show the Look and Feel/Action Buttons choice when NOT editing
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
      m=""
      if(nrow(S$IN$FORM)>0) {                                                    # A new FORM will have no rows
         for(i in 1:nrow(S$IN$FORM)) {
            if(!S$IN$flag$showAddInputButton || (S$IN$FORM$locked && !S$P$SA)) { # if locked and not Sys Admin, don't allow changes
               m = paste0(m, imForm2HTML(S$IN$FORM[i,]))                         #    also no buttons if modifying an input
            } else {
               m = paste0(m, imForm2HTML(S$IN$FORM[i,]),
                  bs4("btn", id="upMe", n=i, q="b", class="mb-2", "Move up"),
                  bs4("btn", id="editMe", n=i, q="g", class="mb-2", "Edit"),
                  bs4("btn", id="downMe", n=i, q="b", class="mb-2", "Move down"),
                  bs4("btn", id="deleteMe", n=i, q="r", class="mb-2", style="float:right;", "Delete")
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
# Accepts one for of a FORM as a parameter, then
#    a.) gets a blank form corresponding the FORMrow's input type
#    b.) loops through the form's inputs, collecting values from the FORMrow's relevant parameters.
#    c.) returns the form
imFORMrow2form <- function(FORMrow) {
   form <- imGetBlankform(FORMrow$type)          # Get the correct kind of input form
   for(i in 1:nrow(form)) {                      # Loop through the form
      id <- form$id[i]
      if(id=="other" || id=="direction") {       # These ids need special handling, as they are not columns in FORMrow
         if(id=="direction") {
            v <- ifelse(FORMrow$inline, "across the page", "in a column")
         } else {
            v <- c("sameline"[FORMrow$sameline], "disabled"[FORMrow$disabled], "locked"[FORMrow$locked])  # a vector
         }
         form[i, "value"] <- paste0(v, collapse=";")
      } else {
         form[i, "value"] <- FORMrow[[form$id[i]]]  # form$id[i] is a column name in FORMrow
      }
#      print(paste0("in imFORMrow2form id ", id, " is ", form[i,"value"]))
   }
   return(form)
}

# This converts either a FORM or a form to an HTML form
imForm2HTML <- function(Form) {
   r = "<form><div>"                            # This div will be closed by the first input; just a hack to give
   if(nrow(Form)>0) {                           #   each input the chance to close the previous row and start a new one.
      for(i in 1:nrow(Form)) {
         thisr = imFormRow2HTML(Form[i,])
         r = paste0(r, thisr)
      }
   }
   return(HTML0(r, "</div></form>"))            # This /div closes the last row of the form
}

# Here we arrive at the heart of the matter.
# This is the code that creates a single HTML input based on a row from a Form.
#   Called repeatedly, once for each input in a Form.
imFormRow2HTML = function(tr) {
# Initialize variables that will be pasted together or are otherwise needed
   options = tr$options
   value = tr$value
   if(any(c("select", "radio", "checkbox") %in% tr$type)) {
      options = str_trim(unlist(str_split(options, ";")))       # vectorize and trim
      value = str_trim(unlist(str_split(value, ";")))           # vectorize and trim
   }
   if(tr$type=="checkbox") {
      if(S$P$SA && str_sub(tr$id,1,2)!="id") {                  # if id doesn't start with "id" its a form, not a FORM
         options = c(options, "locked")                         # System Admin also gets the locked option
         if(tr$locked){
            value = c(value, "locked")
         }
      }
   }
   type = paste0(' type="', tr$type, '"')
   id = paste0(' id="', tr$id, '"')
   label = paste0('<label class="mb-2" for="', tr$id, '">', tr$label, '</label>\n')
   width = switch(tr$width,                                    # These are bs4 column widths (n/12)
      "25%" = "3",
      "33%" = "4",
      "50%" = "6",
      "66%" = "8",
      "75%" = "9",
      "100%" = "12",
      "12"               # default width if none provided
   )
   class = ifelse(tr$inline, " inline", "")
   valueP = ifelse(nchar(value[1])>0, paste0(' value="', value, '"'), '')
   placeholder = ifelse(tr$placeholder=="", "", paste0(' placeholder="', tr$placeholder, '"'))
   min =       ifelse(tr$min=="", "", paste0(" min=", tr$min))           # These are numbers, so no quotes like placeholder
   max =       ifelse(tr$max=="", "", paste0(" max=", tr$max))
   step =      ifelse(tr$step=="", "", paste0(" step=", tr$step))
   maxlength = ifelse(tr$maxlength=="", "", paste0(" maxlength=", tr$maxlength))
   disabled =  ifelse(tr$disabled, " disabled", "")  # Same as readonly but doesn't allow a click to select
   optionsCode = ""                       # Initialize for checkbox-radio-select
   ariaD = paste0(' ariaDescribedby="aria', tr$id,'"')

   if(tr$id=="name" && S$IN$flag$editingForm) {disabled = " disabled"}     # can't edit "name" if editing

### row-col-label
   r=paste0('', '</div><div class="form-row">'[!tr$sameline])           # if not sameline, close old row and start new one (caller must start a row)
   r=paste0(r, '<div class="form-group col-', width, '">')             # start new columnn

### text-password-number
   if(any(c("text", "password", "number") %in% tr$type)) {
      class = paste0(' class="form-control ', class, '"')
      r <- paste0(r, label, '<input', type, id, class, valueP, placeholder, min, max, step, maxlength, disabled, ariaD, '>\n')
   }

### select
   if("select" %in% tr$type) {
      s = options %in% value          # a T/F vector
      for(i in 1:length(options)) {
         optionsCode <- paste0(optionsCode, '<option', ' selected'[s[i]],'>', options[i], '</option>')
#         optionsCode <- paste0(optionsCode, '<option value="', options[i], '" ', selected[i],'>', options[i], '</option>')
      }

r <- paste0(r, label,
'
<select id="', tr$id, '" class="form-control ', class ,'"', disabled, ariaD, '>', optionsCode, '</select>
')
   }

### checkbox/radio
   # Disable all checkboxes, but only uncheck radio buttons (for visual clarity)
   if(any(c("checkbox", "radio") %in% tr$type)) {
      if(options[1]!="") {             # if there are no options at all, the options vector will have a blank first option
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
                  ariaD=ariaD, disabled=tr$disabled)))   # as.character to convert from tagList()
   }

### helptext and return
   helptext <- ifelse(tr$helptext=="", "",
      paste0('<small class="form-text text-muted" id="aria', tr$id, '">', tr$helptext, '</small>'))
   return(paste0(r, helptext, "</div>"))        # Close column. (This function should return text. Caller will HTML it.)
}

# Called when trying to Save an input desciption for a FORMrow to make sure everything is copacetic
#    Make sure the short label is unique and the long label exists
#    For select-radio-checkbox make sure value is in options
#    Also check for all required fields.
imFormValidates <- function() {
   msg=""
   if(!S$IN$flag$editingForm) {                            # Only need to validate the name for new inputs
      name <- str_trim(stripHTML(as.character(input[["name"]]))) # trim and strip HTML from input$name
      if(name=="") {
         msg="<li>The short name for this input can't be blank.</li>"
      } else {
         dbLink <- poolCheckout(shiny.pool)                                    # get a dbLink from the pool
         on.exit(poolReturn(dbLink), add = TRUE)                         # return it when done, even if there's an error
         r = dbExecute(dbLink, paste0("SELECT idsID FROM ", S$db, ".ids WHERE idsID='", name, "';"))
         if(r!=0) {
            msg = "<li>Short names must be unique and that name is already taken.</li>"
         }
      }
   }
   label <- str_trim(stripHTML(as.character(input[["label"]]))) # trim and strip HTML from input$name
   if(label=="") {
      msg = paste0(msg,"<li>The label for this input can't be blank.</li>")
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
      if("" %in% options) {
         msg = paste0(msg,"<li>You have a blank checkbox name.</li>")
      }
      if(length(value)>0) {
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

imFixOrder <- function(tib) {
   tib <- tib[order(tib$order),]
   tib$order <- 1:nrow(tib)
   return(tib)
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
   if(!S$IN$flag$editingForm) {               # Not editing a Form (it's a new one)
      dbLink <- poolCheckout(shiny.pool)                         # get a dbLink from the pool
      on.exit(poolReturn(dbLink), add = TRUE)                    # return it when done, even if there's an error
      name <- str_trim(stripHTML(as.character(input[["name"]]))) # trim and strip HTML from input$name
      id <- paste0("id", paste0(utf8ToInt(name), collapse=""))   # create valid id syntax for both R and JavaScript
      FORMrow[1, "id"] <- id                                     # put this id in FORMrow
      r = dbExecute(dbLink, paste0("INSERT INTO `", S$db, "`.`ids` VALUES ('",
                                   id, "', '", S$IN$tableName, "', '", name, "');"))
      ## if(r!=1), insert failed; however, imFormValidates() checked for uniqueness just milliseconds ago
   }
   form <- imGetBlankform(formtype)           # Get the form for this type of FORMrow; we need its ids
   for(id in form$id) {                       # Loop through the form ids; some, but not all, are columns in FORMrow
      if(is.null(input[[id]])) {              # If input[[id]] is NULL, it can only be checkboxes where nothing is checked
         rawI <- ""
      } else {
         rawI <- str_trim(stripHTML(as.character(input[[id]])))  # trim and strip HTML from input$
      }
      # Special handling for ids "direction" and "other"
      if(id=="direction" || id=="other") {
      # Special handling for select-radio-checkbox ids "direction" and "other"
#      if(formtype %in% c("select", "radio", "checkbox") && (id=="direction" || id=="other")) {
         rawI <- ifelse(str_sub(rawI,-1,-1)==";", str_sub(rawI,1,-2), rawI)  # remove trailing ";"
         rawI <- str_trim(unlist(str_split(rawI, ";")))          # vectorize and retrim
      print(id)
      print(rawI)
      print("sameline" %in% rawI)
         if(id=="direction") {
            FORMrow[1, "inline"] <- "across the page" %in% rawI
         } else {
            FORMrow[1, "sameline"] <- "sameline" %in% rawI
            FORMrow[1, "disabled"] <- "disabled" %in% rawI
            FORMrow[1, "locked"] <- "locked" %in% rawI
         }
      } else {
         FORMrow[1,id] <- ifelse(is.na(rawI), "", rawI)          # NA happens when min, max, step, or maxlength are empty
      }
   }
   if(nrow(S$IN$FORM)==0) {                                      # Create a new tibble
      S$IN$FORM <<- FORMrow
   } else {
      if(FORMrow$order==999) {                                   # Add an additional row to an existing tibble
         print(S$IN$FORM)
         print(FORMrow)
         S$IN$FORM <<- rbind(S$IN$FORM, FORMrow)                 # $order=999 means it's a new FORMrow
      } else {
         S$IN$FORM[S$IN$FORM$order==FORMrow$order,] <<- FORMrow  # Update an existing row in an exisiing tibble
      }
   }
   imSaveFORM()
}

# This is separated from imSaveform2FORMrow because it's also used after moveup, movedown, and delete
imSaveFORM = function() {
   r <- recGet(S$db, "settings", "**", tibble(c("name", "=", S$IN$settingsName)))
   r$name[2] <- S$IN$settingsName                            # Set up in imGetForm; don't change it elsewhere!
   if(nrow(S$IN$FORM)>0) {
      S$IN$FORM$order <<- 1:nrow(S$IN$FORM)
      r$value[2] <- toJSON(S$IN$FORM)                        # FORM is a tibble, so we have to JSONize it
   } else {
      r$value[2] <- ""                                       # JSON won't save colnames if there are no rows
   }
   r <- recSave(r, S$db)
}

imSaveFORMData <- function() {

   ### Missing code here

}
