# In inputMeta2 we want to make the following enhancements to inputMeta

# a. widths in rem rather than columns
# b. can set background colors for text/numeric inputs
# c. new types
#      -Spacer: divs that have a width and can optionally have (centered?) text
#      -Hidden: name-value only; store in
#      -Button: need standard bs4Button fields
# d. id to come later
#      -need a symbol, like &, no user ids can start with this.
#      -for ID=&, id is created later depending on the row and column of the input

# This is some sample HTML for new formatting:


# <div class="form-row"><div class="form-group col-12">     --- not sure we even need these

# <div style="display:table;overflow-x:scroll;table-layout:fixed;">

   # <div style="display:table-cell;vertical-align:top;padding:0 .5rem 0 0;width:12rem;">

      # <label class="mb-2" for="name">Short name</label>
      # <input type="text" id="name" class="form-control  inline shiny-bound-input" placeholder="Required..."
         # maxlength="15" ariadescribedby="arianame" style="width:11rem">                    ---note width on input!
      # <small class="form-text text-muted" id="arianame">Used in tables and graphs.<br>
            # Must be unique; can't be changed.<br>15 characters maximum.</small>
   # </div>

# </div></div>                                              --- would drop if first line drops

# FORM entry changes
# Type of input
# Name  Table  Column   Color (background-color:)
# Label
# Default value
# Placeholder text
# Help text


### open-meta.app input-meta.R
### Tom Weishaar - Sep 2018 - v0.1

# This file provides support functions for creating, editing, and saving FORMs (used by adminForms.R and prjAdmin.R)
#    Additional functions support opening and displaying a FORM and saving its data (used by Extract.R etc.)
#    At the moment, the code supports text, quill, numeric, select (dropdown), radio button, and checkbox inputs.
#       A FORM is a tibble. Each row describes an input.
#       A FORMrow describes one input in the FORM.
#       A "form" is a tibble that describes one input. You can also think of it as a FORM that collects the data for a FORMrow.
#       Input "forms" are loaded into memory by input-meta.R.
#       FORMs are JSONized and stored in the om$prime or project "settings" table.
#       Data collected by a FORM is stored in either a standard table or a name-value table (required for project-custom data).
#       Each FORMrow includes "table" and "column" fields describing where that data item should be stored.
#          (For name-value tables, the FORMrow's "name" field equates to the table's "name" field and "column" is not used.)

# A function with "Form" in the name can handle either a FORM or a form tibble.
# Some FORMs can be customized by project administrators to add inputs for whatever data they want to collect.

# Some initializations
S$IN$inputType <- 1
S$IN$inputNUM <- 0

rv$imGetFORMData <- 0

S$IN$userTypes <- c("Simple text", "Text editor", "Numeric", "Select (dropdown)", "Radio buttons", "Checkboxes", "Spacer", "Hidden")
S$IN$codeTypes <- c("text", "quill", "number", "select", "radio", "checkbox", "spacer", "hidden")

source("imGetBlankFORMrow.R", local=TRUE)  # load the shared imGetBlankFORMrow function

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
imGetFORM = function(FORMname, db=S$db) {
   S$IN$FORMname <<- FORMname                     # imSaveform2FORMrow() will need this
   fileName <- paste0("FORMs/", S$IN$FORMname, ".csv")
   if(A$FORMfromDisk && file.exists(fileName)) {
      f <- read_csv(fileName, na=character(), col_types="ccccccccccccccccccclllln")
   } else {
      r <- recGet(db, "settings", "value", tibble(c("name", "=", FORMname)))
      if(r$value=="") {                              # Did recGet return anything?
         f <- imGetBlankFORMrow("blank")[-1,]        # No, it's a new form. Build a zero-row tibble with default input parameters
      } else {
         f <- as.tibble(fromJSON(r$value))           # Yes, it's an existing form. UnJSONize the value as it's a tibble
      }
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
         bs4("btn", id="inputAdd", q="g", "Add customized input"),
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
      if(S$IN$flag$oldInput) {                    # Disable the type selector when editing an existing input
         ir$disabled = TRUE
      }
      return(tagList(
         imForm2HTML(ir),                         # This displays the type-of-input selector
         output$modifyAnInput <- renderUI(imModifyAnInput()),         # This displays the form for collecting info about that input
         HTML('<div class="text-right mt-3">'),
         bs4("btn", id="inputCancel", n=1, q="b", "Cancel"),
         bs4("btn", id="inputSave", n=1, q="b", "Save"),
         HTML('</div>'),
         bs4("hr")
      ))
   }
}

# REACTIVE - This is called each time its embedded input$inputType selector is changed because it's called
#    by a render function inside imModifyInputs(). It displays the form for creating/editing a FORMrow
imModifyAnInput <- function() {
   r = ""
   if(S$IN$flag$oldInput || is.null(input$inputType)) {
      r = imForm2HTML(S$IN$FORMrowform)           # S$IN$FORMrowform is set up by click observer for "inputAdd" and "inputEdit"
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
   ir$inline = TRUE
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
                  "<div>",                                            # Bug here. Why doesn't FORM close before this div??
                  bs4("btn", id="inputUp", n=i, q=uq, class="mb-2", "Move up"),
                  bs4("btn", id="inputEdit", n=i, q="g", class="mb-2", "Edit"),
                  bs4("btn", id="inputDown", n=i, q=dq, class="mb-2", "Move down"),
                  bs4("btn", id="inputDelete", n=i, q="r", class="mb-2", style="float:right;", "Delete"),
                  "</div>",
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
   r = '<form class="f"><div class="fr">'         # This div will be closed by the first input; just a hack to give
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
   if(tr$type=="hidden") { return("") }                               # Nothing to display
# Initialize variables that will be pasted together or are otherwise needed
   options = tr$options
   value = tr$value
   if(any(c("select", "radio", "checkbox") %in% tr$type)) {
      options = str_trim(unlist(str_split(options, ";")))             # vectorize and trim
      value = str_trim(unlist(str_split(value, ";")))                 # vectorize and trim
   }
   if(tr$type=="checkbox") {                                          # If adminForms.R, System Admin also gets
      if(S$PG$pageName=="adminForms" && S$P$SA && tr$id=="other") {   #  the locked option on the "other" checkboxes
         options = c(options, "locked")
         if(tr$locked){
            value = c(value, "locked")
         }
      }
   }
   type = paste0(' type="', tr$type, '"')
   id = paste0(' id="', tr$id, '"')
   label = paste0('<label class="mb-2" for="', tr$id, '">', tr$label, '</label>\n')
   width = switch(tr$width,                                           # These are widths in rems, approximating
      "25%" = "15",                                                   #    bs4 column widths (n/12)
      "33%" = "20",
      "50%" = "30",
      "66%" = "40",
      "75%" = "45",
      "100%" = "60",
      tr$width
   )
   class = ifelse(tr$color=="default", "", tr$color)
   class = ifelse(tr$inline, paste0(class, " inline"), class)
   valueP = ifelse(nchar(value[1])>0, paste0(' value="', value, '"'), '')
   placeholder = ifelse(tr$placeholder=="", "", paste0(' placeholder="', tr$placeholder, '"'))
   min =       ifelse(tr$min=="", "", paste0(" min=", tr$min))        # These are numbers, so no quotes like placeholder
   max =       ifelse(tr$max=="", "", paste0(" max=", tr$max))
   step =      ifelse(tr$step=="", "", paste0(" step=", tr$step))
   maxlength = ifelse(tr$maxlength=="", "", paste0(" maxlength=", tr$maxlength))
   disabled =  ifelse(tr$disabled, " disabled", "")                   # Same as readonly but doesn't allow a click to select
   optionsCode = ""                                                   # Initialize for checkbox-radio-select
   ariaD = paste0(' ariaDescribedby="aria', tr$id,'"')

   # if(tr$id=="name" && S$IN$flag$oldInput) {                # No user, even SysAdmin, can change the "name" field when editing a form.
   #    disabled = " disabled"                                   #   To allow this, we'd also have to muck around in the "ids" table
   # }                                                           #   User will need to delete that input and start over.

### row-col-label                                              # imForm2HTML() starts with '<form><div class="fr">'
   r=paste0('', '</div><div class="fr">'[!tr$sameline])        #    and ends with '</div></form>. If the first FORMrow has the expected
                                                               #    sameline=FALSE, this code will create a blank and invisible row. On
                                                               #    the other hand, if sameline=TRUE in the first input by mistake, this
                                                               #    code forgives that error and the first input goes in the row that's
                                                               #    already open. After the first row, sameline=FALSE closes the existing
                                                               #    row and starts a new one; sameline=TRUE does not.

### open form cell; also do spacer and buttons, which have special class and style handling for the cell
   if(tr$type %in% c("spacer", "button")) {
      align <- switch(tr$placeholder,                          #    Code allows multiple buttons in one row with
         "Left" = "text-left",                                 #    defined row width and button alignment taken
         "Center" = "text-center",                             #    from first button in the row.
         "Right" = "text-right",
         "text-center"
      )
      switch(tr$type,
         "spacer" = {
            height = ifelse(tr$value=="", 1, tr$value)         # 0rem to 1.3rem is 29 px high, maybe because it's display:table-cell?
            r=paste0(r, '<div class="fc mr-2 ', align, ' ', class,
                     '" style="width:', width, 'rem;min-width:', width, 'rem;height:', height, 'rem;">', tr$label)
         },
         "button" = {
            if(tr$sameline) {                                 # If sameline, button goes in fc that's already open
               closingDiv <- "</div>"                         #    but we have to assume no more than two buttons
            } else {                                          #    so we can close the <div>.
               closingDiv <- ""
               r=paste0(r, '<div class="fc mr-2 ', align, ' ',
                     '" style="width:', width, 'rem;min-width:', width, 'rem;">')
            }
            switch(tr$color,
               "default" = { q = "b" },
               "blue" = { q = "b" },
               "green" = { q = "g" },
               "purple" = { q = "i" },
               "yellow" = { q = "y" },
               "red" = { q = "r"}
                )
            switch(tr$btnsize,
               "Extra-small" = { q = c(q,"xs") },
               "Small" = { q = c(q,"s") },
               "Large" = { q = c(q,"l") }
                )
            switch(tr$btnshape,
               "Pill" = { q = c(q, "p") },
               "Square" = { q = c(q, "q") }
               )
            r <- paste0(r,                                     # as.character to convert from tagList()
               as.character(bs4("btn", id=value, class=tr$options, q=q, disabled=tr$disabled, tr$label)), closingDiv)
            return(r)                                          # skip <div> closing code below
         }
      )
   } else {
      r=ifelse(tr$width=="",
         paste0(r, '<div class="fc mr-2">'),
         paste0(r, '<div class="fc mr-2" style="width:', width, 'rem;min-width:', width, 'rem;">'))

   }

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
         if(!tr$inline) {
            cboxWidth = paste0(' " style="width:', width, 'rem;min-width:', width, 'rem;" ')
         } else {
            cboxWidth = ""
         }
r <- paste0(r, '
<div id="', tr$id, '" class="shiny-input-', tr$type, 'group shiny-input-container', '-inline'[tr$inline], '"',
            cboxWidth, ariaD, '>', label, '
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

# Called before saving an Input to a FORMrow to make sure everything is copacetic.
#    Makes sure the short label is unique.
#    Checks for all required fields.
#    For select-radio-checkbox make sure value is in options
imInputValidates <- function() {
   msg=""
   inputCode <- S$IN$codeTypes[S$IN$inputType]
   if(inputCode %in% (c("spacer", "hidden", "button"))) { return(TRUE) }       # No need to validate these...
   oldName <- ""
   if(S$IN$inputNUM>0) { oldName <- S$IN$FORM$name[S$IN$inputNUM] } # Get the old name, if there was one
   name <- str_trim(stripHTML(as.character(input[["name"]])))                  # trim and strip HTML from input$name
   if(name=="") {
      msg="<li>The Short name for this input can't be blank.</li>"
   } else {
      names <- S$IN$FORM$name                                                  # Check names in this form for duplicates
      if(S$IN$inputNUM>0) { names <- names[-S$IN$inputNUM]}                    #    But not this input itself!
      if(name %in% names) {                                                    # make sure name isn't duplicated within this form
         msg = paste0("<li>Another input in this form is already using the Short Name <b>", name, "</b>.</li>")
      }
      if(str_sub(S$IN$FORMname,1,8)=="PrjForm-") {  # if it's prjFORM, need to make sure names aren't duplicated in other forms, either.
         dbLink <- poolCheckout(shiny.pool)                                    # get a dbLink from the pool
         on.exit(poolReturn(dbLink), add = TRUE)                               # Have to use dbLink because ids is a non-standard table
            # NOTE: if coming from adminForms.R, S$db will be om$prime
         r = dbGetQuery(dbLink, paste0("SELECT form FROM `", S$db, "`.`ids` WHERE idAsName='", name, "';"))
         if(nrow(r)>0 && any(r$form != S$IN$FORMname)) {                      # Not interested in THIS form
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
   if(inputCode %in% c("select", "radio", "checkbox")) {
      options <- str_trim(stripHTML(as.character(input[["options"]])))         # trim and strip HTML from input$options
      options <- ifelse(str_sub(options,-1,-1)==";", str_sub(options,1,-2), options) # remove trailing ";"
      options <- str_trim(unlist(str_split(options, ";")))                     # vectorize and retrim
      value <- str_trim(stripHTML(as.character(input[["value"]])))             # trim and strip HTML from input$value
      value <- ifelse(str_sub(value,-1,-1)==";", str_sub(value,1,-2), value)   # remove trailing ";"
      value <- str_trim(unlist(str_split(value, ";")))                         # vectorize and retrim
   }
   if(inputCode=="select") {
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
   if(inputCode=="radio") {
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
   if(inputCode=="checkbox") {
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

# Used to save an input into a FORM after the input form validates, then will save the entire FORM.
#    Puts the input's values in a pre-existing FORMrow, puts that in a pre-existing or new FORM,
#       and saves the FORM to the settings file (see below for saving FORM DATA!)
#    In general, the ids of an input form match the column names in a FORMrow.
#       However, form id "direction" collects data that needs to be converted and stored as T-F in
#         FORMrow column "inline" while form id "other" collects data that needs to be converted
#         and stored as T-F in FORMrow columns "sameline", "disabled", and "locked". Note that we're
#         pulling the form's values out of input[[id]].
#    ids are saved using utf8ToInt() in PrjForm- forms so that users can enter any character as
#       part of the name and we still have a valid JavaScript and R variable name.
imSaveform2FORMrow <- function() {
   FORMrow <- S$IN$FORMrow                 # Get unedited row (pre-exisiting or blank; set up by inputAdd or inputEdit)
   inputCode <- S$IN$codeTypes[S$IN$inputType]  # Note that if we are editing, the inputType can't be changed,
   FORMrow$type <- inputCode                    #    but if it's a new form it can, so we can't depend on FORMrow's $type
      # because the short name can now be changed, we have to do stuff if it's new or changed
   oldName <- ""
   if(S$IN$inputNUM>0) { oldName <- S$IN$FORM$name[S$IN$inputNUM] } # Get the old name, if there was one
   name <- str_trim(stripHTML(as.character(input[["name"]])))       # Trim and strip HTML from the new name
   if(str_sub(S$IN$FORMname,1,8)=="PrjForm-" && oldName != name) {  # For PrjForm- inputs, new or with name change only
      dbLink <- poolCheckout(shiny.pool)                            # get a dbLink from the pool
      on.exit(poolReturn(dbLink), add = TRUE)                       # return it when done, even if there's an error
      if(oldName!="") {                                             # if name change, need to delete old id
         r = dbExecute(dbLink, paste0("DELETE FROM `", S$db, "`.`ids` WHERE idAsName='", oldName, "';"))
      }
      id <- paste0("id", paste0(utf8ToInt(name), collapse=""))   # create valid id syntax for both R and JavaScript
      FORMrow$id <- id                                           # save the new id back into the FORMrow
      r = dbExecute(dbLink, paste0("INSERT INTO `", S$db, "`.`ids` VALUES ('",
                                   id, "', '", S$IN$FORMname, "', '", name, "');")) # if adminForm.R S$db is om$prime
      ### imInputValidates() checked for uniqueness just milliseconds ago
   } else {
      FORMrow$id <- "FORM-default"                               # For Form- forms, skip the ids table and use this for now
   }
   form <- imGetBlankform(inputCode)                             # Get the form for this type of FORMrow; we need its ids
   for(id in form$id) {                                          # Loop through the form ids; some, but not all, are columns in FORMrow
      rawI <- str_trim(stripHTML(input[[id]]))                   # trim and strip HTML from input$
      if(id=="options" && inputCode %in% c("select", "radio", "checkbox")) {   # Special handling for Select, Radio, and Checkbox options
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
   if(FORMrow$id=="FORM-default") {                              # Now we can fix the id on non-PrjForm- foms
      FORMrow$id <- FORMrow$name
   }
   FORMrow$formname <- S$IN$FORMname
   if(nrow(S$IN$FORM)==0) {                                      # To rbind or not to rbind, that is the question.
      S$IN$FORM <<- FORMrow
   } else {
      if(FORMrow$order==999) {                                   # Add an additional row to an existing tibble
         S$IN$FORM <<- rbind(S$IN$FORM, FORMrow)                 # $order=999 means it's a new FORMrow
      } else {
         S$IN$FORM[S$IN$FORM$order==FORMrow$order,] <<- FORMrow  # Update an existing row in an existing tibble
      }
   }
   imSaveFORM()
}

# Saves S$IN$FORM into the S$db.settings file. For adminForms.R that's om$prime, for prjAdmin.R it's the project's db
#    This is separated from imSaveform2FORMrow because it's also used after moveup, movedown, and delete
#    Note that this is used to save a FORM, but NOT the data collected by the form. For that, see rv$imGetFORMData below.
# Always saves to SQL, also saves to disk if A$FORMtoDisk is TRUE.
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
   if(A$FORMtoDisk) {
      write.csv(S$IN$FORM, paste0("FORMs/", S$IN$FORMname, ".csv"), row.names=F)
   }
}

# This is called by adminForms.R and prjAdmin.R to move FORMrows up or down within a FORM.
imFixOrder <- function() {
   S$IN$FORM <<- S$IN$FORM[order(S$IN$FORM$order),]
   return(imSaveFORM())                                          # imSaveFORM will renumber the $order vector
}

# Get the NUM, based on the recID
imID2NUM <- function(ID, TABLE) {
   SELECT = paste0(TABLE,"NUM")
   tableID = paste0(TABLE,"ID")
   if(ID>0) {                                                                 # already exists, get it
      R <- recGet(S$db, TABLE, SELECT, WHERE=tibble(c(tableID, "=", ID)))
      NUM <- as.numeric(R[[SELECT]])
   } else {
      R <- recGet(S$db, TABLE, SELECT, WHERE=tibble(c(tableID, ">", 0), c("deleted", ">=", "0")))  # don't duplicate a
      if(nrow(R)!=0) {                                                                             #   deleted NUM
         NUM <- max(as.numeric(R[[SELECT]])) + 1                              # It's the max of tableNUM plus 1
      } else {
         NUM <- 1                                                             # But for first one it's 1
      }
   }
   return(NUM)
}

# Run imGetFORMData by incrementing rv$imGetFORMData
# This gets data from inputs, not from server; see imGetFORMvalues()
# First saves values of inputs into $values column of S$IN$FORM, then into proper type of SQL table
S$IN$flag$imSAVE <- "start"
observeEvent(c(input$js.editorText, rv$imGetFORMData), {
   if(rv$imGetFORMData>0 && nrow(S$IN$FORM)>0 && S$P$Modify) {        # Don't actually run this until we get an increment and then only
      if(S$IN$flag$imSAVE=="start") {                                 #   if there are rows in FORM and the user has modify permission.
         S$IN$flag$imSAVE <<- "finish"                                # First deal with Quill inputs, if any
         QTF <- S$IN$FORM$type=="quill"                               # TF vector of rows with quill inputs
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
            ids <- ids[-S$IN$Qs]                                      # Remove quill ids, if any
         }
         ids <- ids[ids!=""]                                          # Remove blank ids (spacers, etc.)
         for(id in ids) {
            rawI <- str_trim(stripHTML(as.character(input[[id]])))    # trim and strip HTML from input$; changes NULL to ""
            if(length(rawI)>1) {                                      # collapse vectors (multiple checkboxes)
               rawI <- paste0(rawI, collapse=";")
            }
            S$IN$FORM$value[S$IN$FORM$id==id] <<- ifelse(is.na(rawI), "", rawI) # NA happens when a numeric input is empty
         }
      }
      S$IN$flag$imSAVE <<- "start"                                    # Clean up for next run

      # At this point we have a FORM with the new inputs saved in the values column. But actually saving the FORM
      #   is not what we want here. Instead, we now save the FORM values in the appropriate table, which can be
      #   either a name-value table or a standard row-column table. Then we throw the FORM away as saving it
      #   would change the FORM's default values!
      dataTable <- S$IN$FORM$table[1]                                 # Name of SQL table is stored in FORM$Table
      if(dataTable=="extract") {
         NUMS <- unlist(str_split(S$IN$FORM$column[1], fixed(",")))   # Column field of table lets us know which
         armNUM <- ifelse(NUMS[1]==0, 0, S$NUMs$armNUM)               #    NUMs should always be 0.
         WHERE=tibble(c=c("catalogID", "=", S$NUMs$catalogID),
                      s=c("studyNUM", "=", S$NUMs$studyNUM),
                      a=c("armNUM", "=", armNUM))
         for(i in 1:nrow(S$IN$FORM)) {                                # Save FORM values
            WHERE$n = c("name", "=", S$IN$FORM$name[i])
            R <-  recGet(S$db, dataTable, SELECT="**", WHERE=WHERE)
            R$catalogID[2] <- S$NUMs$catalogID                        # While edited recs will already have NUMs
            R$studyNUM[2] <- S$NUMs$studyNUM                          #    and Name, must do this for new rows!
            R$armNUM[2] <- armNUM
            R$name[2] = S$IN$FORM$name[i]
            R$value[2] = S$IN$FORM$value[i]
            R <- recSave(R, S$db)                                     # There's a save on each loop
         }
      }
      if(dataTable %in% c("settings", "pico")) {
         NUM <- imID2NUM(S$IN$recID, dataTable)                       #    We'll need this for each row, so save in variable
         tableNUM <- paste0(dataTable,"NUM")
         for(i in 1:nrow(S$IN$FORM)) {                                # Save FORM values
            R <-  recGet(S$db, dataTable, SELECT="**",
                     WHERE=tibble(c(paste0(dataTable, "NUM"), "=", NUM), c("name", "=", S$IN$FORM$name[i])))
            R[[tableNUM]][2] = NUM                                    # While edited recs will already have NUM
            R$name[2] = S$IN$FORM$name[i]                             #    and Name, must do this for new rows!
            R$value[2] = S$IN$FORM$value[i]
            R <- recSave(R, S$db)                                     # There's a save on each loop
         }
      }
      if(dataTable %in% "review") {
         status <- S$IN$FORM$value[S$IN$FORM$id=="Stage2Status"]      # Handling Extraction Reviews
         chex <- S$IN$FORM$value[S$IN$FORM$id=="Stage2Detail"]        #  Also see error-checking and save to catalog table
         comment <- S$IN$FORM$value[S$IN$FORM$id=="Stage2Comment"]    #  in Extract.R
         r <- recGet(S$db, "review", "**", tibble(c("catalogID", "=", S$NUMs$catalogID)))
         if(status=="Unreviewed") {                                   # Undo prior review, if one
            r$decision[2] <- 2                                        # 2=Stage 1 Pass
         } else {
            r$decision[2] <- ifelse(status=="Fail - study ineligible", 3, 4)  # 3=Stage 2 Fail; 4=Stage 2 Pass
         }
         r$detail[2] <- toJSON(chex)
         r$comment[2] <- comment
         r <- recSave(r, S$db)
      }
      if(dataTable %in% "analysis") {
         r <- recGet(S$db, "analysis", "**", tibble(c("analysisID", "=", S$NUMs$analysisNUM)))
         cns <- as.character(S$IN$FORM$column)                        # "column" is table column name
         cns <- cns[-which(cns=="")]                                  # delete blanks (spacers)
         for(i in cns) {                                              # Form values to table columns
            r[[2, i]] <- unlist(S$IN$FORM[S$IN$FORM$column==i , "value"])  # unlist pries the values out of the tibble
         }
         r <- recSave(r, S$db)
      }
      if(!dataTable %in% c("extract", "settings", "pico", "review", "analysis")) {          # It's a standard table
         warning("At the bottom of inputMeta.R, the code for saving FORM data into a standard table is incomplete.")
         R <-  recGet(S$db, dataTable, SELECT="**", WHERE=tibble(c(paste0(dataTable, "ID"), "=", S$IN$recID)))
         for(i in 1:nrow(S$IN$FORM)) {
            R[[i, S$IN$FORM$column]] <- S$IN$FORM$value[i]            # Move data from rows to columns
         }
         R <- recSave(R, S$db)                                        # Save it
      }
      rv$limn = rv$limn+1                                             # Re-render
   }
})

imGetFORMvalues <- function (FORM) {
   switch(FORM$table[1],                                              # Get SQL table name from FORM
      "extract" = {                                                   # This section is for an extract table
         NUMS <- unlist(str_split(FORM$column[1], fixed(",")))        # Column field of table lets us know which
         if(length(NUMS)<4) {
            warning(paste0("In this form the length of NUMS is ", length(NUMS)))
         }
         FarmNUM <- ifelse(NUMS[1]==0, 0, S$NUMs$armNUM)
         names <- pull(FORM, name)                                    # Will need to use FORM's names a couple of times
         options <- pull(FORM, options)
         v <- S$extractTBL %>%                                        # Using current NUMs and names in FORM
                  filter(catalogID==S$NUMs$catalogID &                #   get name-value pairs
                         studyNUM==S$NUMs$studyNUM &
                         armNUM==FarmNUM &
                         name %in% names &
                         deleted==0) %>%
                  select(name, value) %>%
                  collect()
         for(i in 1:nrow(FORM)) {
            if(names[i] %in% v$name) {                                # Leave FORM's default value if nothing found on server
               FORM$value[i] <- v$value[v$name %in% names[i]]         #    Insert value from server in FORM
            }
            if(str_sub(options[i],1,6)=="pico::") {                   # Deal with options in pico:: format
               picoName <- unlist(str_split(options[i], fixed("::"))) # The picoName is the string after pico::
               picoName <- unlist(str_split(picoName[2], fixed(";"))) # There may be additional options, remove them
               choices = recGet(S$db, "pico", "value", tibble(c("name", "=", picoName[1]))) # Get the select options from
               FORM$options[i] <- paste0(choices$value, collapse=";") #    the pico table and put them in expected format
               if(FORM$value[i]=="(No Response)") {                   # Update PICO values - if value is "(No Response)",
                  FORM$value[i] <- choices$value[2]                   #    change to next option
               }
            }
         }
      },
      "review" = {
         r = recGet(S$db, "review", c("decision", "detail", "comment"), WHERE=tibble(c("catalogID", "=", S$NUMs$catalogID)))
         FORM$value[1] <- ifelse(r$decision<3, "Unreviewed", ifelse(r$decision==3, "Fail - study ineligible", "Pass - study Ok"))
         if(FORM$options[3]=="settings::failBoxNames") {
            fBN <- recGet(S$db, "settings", "value", WHERE=tibble(c("name", "=", "failBoxNames")))
            FORM$options[3] <- paste0(fromJSON(fBN$value), collapse=";")
            FORM$value[3] <- paste0(fromJSON(r$detail), collapse=";")
         }
         FORM$value[5] <- r$comment
      },
      "analysis" = {
         A <- recGet(S$db, "analysis", c("analysisID", "name", "type", "P", "I", "C", "O", "TS", "comment"),
           tibble(c("analysisID", "=", S$NUMs$analysisNUM)))
         FORM$value[FORM$column=="name"] <- A$name
         FORM$value[FORM$column=="type"] <- A$type
         FORM$value[FORM$column=="comment"] <- A$comment
         for(i in c("P", "I", "C", "O", "TS")) {                      # The options come from the project's results table
            FORM$options[FORM$column==i] <- paste0(unique(S$R[[i]]), collapse=";")
            if(A[[i]]=="") {
               FORM$value[FORM$column==i] <- FORM$options[FORM$column==i]
            } else {
               FORM$value[FORM$column==i] <- A[[i]]
            }
         }
         # Change statistical analysis options here!
         FORM$options[FORM$column=="type"] <- "Cluster-robust hierarchical effects;Cluster-robust correlated effects"
      },
      warning(paste0("In imGetFORMvalues(), no handler for ", FORM$table[1], " table."))
      )
   return(FORM)
}
