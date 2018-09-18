### open-meta STAND-ALONE-Blankforms-list.R
### Tom Weishaar - Sep 2018 - v0.1
### This is a stand-alone file for creating a list of Blankform defintions.
###    This list is saved to a file and loaded during app startup.
###    The list is used to create customizable forms.

library(tibble)

imGetBlankFORMrow = function(type) {
   return(tibble(
      type=type,                # see S$IN$codeTypes
      id="",                    # identifies one of these parameter for Forms; otherwise paste0("id", paste0(utf8ToInt(name), collapse=""))
      name="",                  #    a short identifier for tables and figures (must be unique)
      label="",
      width="",                 # for width the possible options are 25%-50%-75%-100%
      value="",                 # selected option(s) for select-radio-checkbox; otherwise current value
      placeholder="",
      maxlength="",             # Number of characters accepted
      min="",                   # These three are for the numeric input type
      max="",                   #  ...the value of these numbers will be in strings, however
      step="",
      options="",               # all the options for select-radio-checkbox, separated by ";"
                                # for "form", the possible options are: inline, sameline, disabled, and locked
      inline=TRUE,              # whether radio buttons and checkboxes are inline or in a column
      sameline=FALSE,           # whether the current input should be on the same line as the previous input (if it would fit)
      disabled=FALSE,           # whether this input should be disabled
      locked=FALSE,             # whether this FORMrow should be locked
      order=999                 # used to reorder the inputs
#      required=,               # requires a true form submit to work, which we don't do
#      readonly=,               # use disabled, which won't accept focus like readonly will
#      size=0,                  # width of input, but form-control class overrides this one
   ))
}

imBlankforms <- list()         # This is a list of tibbles; one multi-row tibble for each type of input

### Simple Text input
w=1
   r <- imGetBlankFORMrow("text")
   r$id[w] <- "name"
   r$label[w] <- "Short name for this input's data in tables and graphs (must be unique)"
   r$placeholder[w] <- "15 chars max"
   r$maxlength[w] <- "15"
   r$width[w] <- "25%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "label"
   r$label[w] <- "Long name for forms"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Initial value for this input"
   r$placeholder[w] <- "This is placeholder text. Leave this initial value blank if you enter placeholder text next..."
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "placeholder"
   r$label[w] <- "Placeholder text for this input"
w=w+1
   r[w,] <- imGetBlankFORMrow("number")
   r$id[w] <- "maxlength"
   r$label[w] <- "Maximum number of characters allowed"
   r$placeholder[w] <- "Blank means no maximum..."
   r$width[w] <- "25%"
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "width"
   r$label[w] <- "Width of this input"
   r$options[w] <- "25%; 50%; 75%; 100%"
   r$value[w] <- "100%"
   r$inline[w] = TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("checkbox")
   r$id[w] <- "other"
   r$label[w] <- "Other options"
   r$options[w] <- "sameline; disabled"
   r$inline[w] = TRUE
imBlankforms[["text"]] <- r          # userType is "Simple Text"

### Text Editor input
w=1
   r <- imGetBlankFORMrow("text")
   r$id[w] <- "name"
   r$label[w] <- "Short name for this input's data in tables and graphs (must be unique)"
   r$placeholder[w] <- "15 chars max"
   r$maxlength[w] <- "15"
   r$width[w] <- "25%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "label"
   r$label[w] <- "Label for the editor"
w=w+1
   r[w,] <- imGetBlankFORMrow("quill")
   r$id[w] <- "value"
   r$label[w] <- "Any initial text for the editor"
   r$placeholder[w] <- "This is placeholder text. Leave this initial text blank if you want to enter placeholder text next..."
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "placeholder"
   r$label[w] <- "Placeholder text for the editor"
w=w+1
   r[w,] <- imGetBlankFORMrow("checkbox")
   r$id[w] <- "other"
   r$label[w] <- "Other options"
   r$options[w] <- ""
   r$inline[w] = TRUE
imBlankforms[["quill"]] <- r         # userType is "Text editor"

### Numeric input
w=1
   r <- imGetBlankFORMrow("text")
   r$id[w] <- "name"
   r$label[w] <- "Short name for this input's data in tables and graphs (must be unique)"
   r$placeholder[w] <- "15 chars max"
   r$maxlength[w] <- "15"
   r$width[w] <- "25%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "label"
   r$label[w] <- "Label for this input"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Initial value for this input"
   r$placeholder[w] <- "This is placeholder text. Leave this initial value blank if you enter placeholder text next..."
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "placeholder"
   r$label[w] <- "Placeholder text for this input"
w=w+1
   r[w,] <- imGetBlankFORMrow("number")
   r$id[w] <- "min"
   r$label[w] <- "Minimum"
   r$placeholder[w] <- "Blank means no minimum..."
   r$width[w] <- "25%"
w=w+1
   r[w,] <- imGetBlankFORMrow("number")
   r$id[w] <- "max"
   r$label[w] <- "Maximum"
   r$placeholder[w] <- "Blank means no maximum..."
   r$width[w] <- "25%"
w=w+1
   r[w,] <- imGetBlankFORMrow("number")
   r$id[w] <- "step"
   r$label[w] <- "Step"
   r$placeholder[w] <- "Blank means no step..."
   r$width[w] <- "25%"
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "width"
   r$label[w] <- "Width of this input"
   r$options[w] <- "25%; 50%; 75%; 100%"
   r$value[w] <- "25%"
   r$inline[w] = TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("checkbox")
   r$id[w] <- "other"
   r$label[w] <- "Other options"
   r$options[w] <- "sameline; disabled"
   r$inline[w] = TRUE
imBlankforms[["number"]] <- r         # userType is "Numeric"

### Select input
w=1
   r <- imGetBlankFORMrow("text")
   r$id[w] <- "name"
   r$label[w] <- "Short name for this input's data in tables and graphs (must be unique)"
   r$placeholder[w] <- "15 chars max"
   r$maxlength[w] <- "15"
   r$width[w] <- "25%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "label"
   r$label[w] <- "Label for this input"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "options"
   r$label[w] <- "Options for this input"
   r$placeholder[w] <- "Put a semicolon between options..."
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Selected option"
   r$width[w] <- "25%"
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "width"
   r$label[w] <- "Width of this input"
   r$options[w] <- "25%; 50%; 75%; 100%"
   r$value[w] <- "100%"
   r$inline[w] = TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("checkbox")
   r$id[w] <- "other"
   r$label[w] <- "Other options"
   r$options[w] <- "sameline; disabled"
   r$inline[w] = TRUE
imBlankforms[["select"]] <- r         # userType is "Select (dropdown)"

### Radio button input
w=1
   r <- imGetBlankFORMrow("text")
   r$id[w] <- "name"
   r$label[w] <- "Short name for this input's data in tables and graphs (must be unique)"
   r$placeholder[w] <- "15 chars max"
   r$maxlength[w] <- "15"
   r$width[w] <- "25%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "label"
   r$label[w] <- "Label for this input"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "options"
   r$label[w] <- "Options for this input"
   r$placeholder[w] <- "Put a semicolon between options..."
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Selected option"
   r$width[w] <- "25%"
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "width"
   r$label[w] <- "Width of this input"
   r$options[w] <- "25%; 50%; 75%; 100%"
   r$value[w] <- "100%"
   r$inline[w] = TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "direction"
   r$label[w] <- "Align the radio buttons"
   r$options[w] <- "in a column; across the page"
   r$value[w] <- "across the page"
   r$inline[w] = TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("checkbox")
   r$id[w] <- "other"
   r$label[w] <- "Other options"
   r$options[w] <- "sameline; disabled"
   r$inline[w] = TRUE
imBlankforms[["radio"]] <- r         # userType is "Radio buttons"

### Checkboxes input
w=1
   r <- imGetBlankFORMrow("text")
   r$id[w] <- "name"
   r$label[w] <- "Short name for this input's data in tables and graphs (must be unique)"
   r$placeholder[w] <- "15 chars max"
   r$maxlength[w] <- "15"
   r$width[w] <- "25%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "label"
   r$label[w] <- "Label for this input"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "options"
   r$label[w] <- "Options for this input"
   r$placeholder[w] <- "Put a semicolon between options..."
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Selected options"
   r$width[w] <- "50%"
   r$placeholder[w] <- "If more than one, put a semicolon between them..."
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "width"
   r$label[w] <- "Width of this input"
   r$options[w] <- "25%; 50%; 75%; 100%"
   r$value[w] <- "100%"
   r$inline[w] = TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "direction"
   r$label[w] <- "Align the checkboxes"
   r$options[w] <- "in a column; across the page"
   r$value[w] <- "across the page"
   r$inline[w] = TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("checkbox")
   r$id[w] <- "other"
   r$label[w] <- "Other options"
   r$options[w] <- "sameline; disabled"
   r$inline[w] = TRUE
imBlankforms[["checkbox"]] <- r         # userType is "Checkboxes"

### TF_Chex input
# This isn't working quite right and it was for Admins only. Leaving it out for now.
# w=1
#    r <- imGetBlankFORMrow("text")
#    r$id[w] <- "name"
#    r$label[w] <- "Short name for this input's data in tables and graphs (must be unique)"
#    r$placeholder[w] <- "15 chars max"
#    r$maxlength[w] <- "15"
#    r$width[w] <- "25%"
# w=w+1
#    r[w,] <- imGetBlankFORMrow("text")
#    r$id[w] <- "label"
#    r$label[w] <- "Label for this input"
# w=w+1
#    r[w,] <- imGetBlankFORMrow("text")
#    r$id[w] <- "options"
#    r$label[w] <- "Options for this input"
#    r$placeholder[w] <- "Put a semicolon between options..."
# w=w+1
#    r[w,] <- imGetBlankFORMrow("text")
#    r$id[w] <- "value"
#    r$label[w] <- "Selected options"
#    r$width[w] <- "50%"
#    r$placeholder[w] <- "If more than one, put a semicolon between them..."
# w=w+1
#    r[w,] <- imGetBlankFORMrow("radio")
#    r$id[w] <- "width"
#    r$label[w] <- "Width of this input"
#    r$options[w] <- "25%; 50%; 75%; 100%"
#    r$value[w] <- "100%"
#    r$inline[w] = TRUE
# w=w+1
#    r[w,] <- imGetBlankFORMrow("radio")
#    r$id[w] <- "direction"
#    r$label[w] <- "Align the checkboxes"
#    r$options[w] <- "in a column; across the page"
#    r$value[w] <- "across the page"
#    r$inline[w] = TRUE
# w=w+1
#    r[w,] <- imGetBlankFORMrow("checkbox")
#    r$id[w] <- "other"
#    r$label[w] <- "Other options"
#    r$options[w] <- "sameline; disabled"
#    r$inline[w] = TRUE
# imBlankforms[["TFchex"]] <- r         # userType is "TF_Chex"

saveRDS(imBlankforms, file="app/Blankforms.RDS")
##################################################################################


rm(r, w, imBlankforms, imGetBlankFORMrow)


#imBlankforms <- readRDS(file="app/Blankforms.RDS")

