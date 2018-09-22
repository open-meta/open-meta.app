### open-meta STAND-ALONE-Blankforms-list.R
### Tom Weishaar - Sep 2018 - v0.1
### This is a stand-alone file for creating a list of Blankform defintions.
###    This list is saved to a file and loaded during app startup.
###    The list is used to create customizable forms.

library(tibble)

source("app/inputMetaAux.R", local=TRUE)  # load the shared imGetBlankFORMrow function

imBlankforms <- list()         # This is a list of tibbles; one multi-row tibble for each type of input

imName <- imGetBlankFORMrow("text")
   imName$id <- "name"
   imName$label <- "Short name"
   imName$placeholder <- "Required..."
   imName$maxlength <- "15"
   imName$width <- "25%"
   imName$helptext <- "Used in tables and graphs.<br>Must be unique; can't be changed.<br>15 characters maximum."

imLabel <- imGetBlankFORMrow("text")
   imLabel$id <- "label"
   imLabel$label <- "Long name"
   imLabel$placeholder <- "Required..."
   imLabel$helptext <- "Used in forms."
   imLabel$width <- "75%"

imHelptext <- imGetBlankFORMrow("text")
   imHelptext$id <- "helptext"
   imHelptext$label <- "Help text"
   imHelptext$helptext <- "This is what help text looks like."
   imHelptext$width <- "75%"

imWidth <- imGetBlankFORMrow("radio")
   imWidth$id <- "width"
   imWidth$label <- "Width of this input"
   imWidth$options <- "25%; 33%; 50%; 66%; 75%; 100%"
   imWidth$value <- "100%"
   imWidth$inline = TRUE
   imWidth$width <- "50%"

imOther <- imGetBlankFORMrow("checkbox")            # options include disabled
   imOther$id <- "other"                            #    used by text, numeric, and select
   imOther$label <- "Other options"
   imOther$options <- "sameline; disabled"
   imOther$inline = TRUE
   imOther$sameline <- TRUE
   imOther$width <- "50%"


### Simple Text input
w=1
   r <- imName
w=w+1
   r[w,] <- imLabel
w=w+1
   r[w,] <- imHelptext
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Initial value for this input"
   r$width[w] <- "75%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "placeholder"
   r$label[w] <- "Placeholder text for this input"
   r$placeholder[w] <- "This is placeholder text. It appears only when the value is blank..."
   r$width[w] <- "75%"
w=w+1
   r[w,] <- imGetBlankFORMrow("number")
   r$id[w] <- "maxlength"
   r$label[w] <- "Maximum characters allowed"
   r$placeholder[w] <- "Blank means no maximum..."
   r$width[w] <- "33%"
w=w+1
   r[w,] <- imWidth
w=w+1
   r[w,] <- imOther
imBlankforms[["text"]] <- r          # userType is "Simple Text"

### Text Editor input
w=1
   r <- imName
w=w+1
   r[w,] <- imLabel
w=w+1
   r[w,] <- imHelptext
w=w+1
   r[w,] <- imGetBlankFORMrow("quill")
   r$id[w] <- "value"
   r$label[w] <- "Any initial text for the editor"
   r$width[w] <- "75%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "placeholder"
   r$label[w] <- "Placeholder text for the editor"
   r$placeholder[w] <- "This is placeholder text. It appears only when the editor is empty..."
   r$width[w] <- "75%"
w=w+1
   r[w,] <- imWidth
w=w+1
   r[w,] <- imOther
imBlankforms[["quill"]] <- r         # userType is "Text editor"

### Numeric input
w=1
   r <- imName
w=w+1
   r[w,] <- imLabel
w=w+1
   r[w,] <- imHelptext
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Initial value for this input"
   r$placeholder[w] <- "Blank means no initial value..."
   r$width[w] <- "33%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "placeholder"
   r$label[w] <- "Placeholder text for this input"
   r$width[w] <- "33%"
   r$sameline[w] <- TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("number")
   r$id[w] <- "min"
   r$label[w] <- "Minimum"
   r$placeholder[w] <- "Blank means no minimum..."
   r$helptext[w] <- "Smallest acceptable number."
   r$width[w] <- "33%"
w=w+1
   r[w,] <- imGetBlankFORMrow("number")
   r$id[w] <- "max"
   r$label[w] <- "Maximum"
   r$placeholder[w] <- "Blank means no maximum..."
   r$helptext[w] <- "Largest acceptable number."
   r$width[w] <- "33%"
   r$sameline[w] <- TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("number")
   r$id[w] <- "step"
   r$label[w] <- "Step"
   r$placeholder[w] <- "Blank means no step..."
   r$helptext[w] <- "Up/Down arrow increment/decrement."
   r$width[w] <- "33%"
   r$sameline[w] <- TRUE
w=w+1
   r[w,] <- imWidth
w=w+1
   r[w,] <- imOther
imBlankforms[["number"]] <- r         # userType is "Numeric"

### Select input
w=1
   r <- imName
w=w+1
   r[w,] <- imLabel
w=w+1
   r[w,] <- imHelptext
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "options"
   r$label[w] <- "Options for this dropdown"
   r$placeholder[w] <- "Required..."
   r$helptext[w] <- "Put exactly one semicolon between options."
   r$width[w] <- "66%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Selected option"
   r$helptext[w] <- "Only one option can be selected.<br>Text must exactly match the option name."
   r$width[w] <- "33%"
   r$sameline[w] <- TRUE
w=w+1
   r[w,] <- imWidth
w=w+1
   r[w,] <- imOther
imBlankforms[["select"]] <- r         # userType is "Select (dropdown)"

### Radio button input
w=1
   r <- imName
w=w+1
   r[w,] <- imLabel
w=w+1
   r[w,] <- imHelptext
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "options"
   r$label[w] <- "Radio button names"
   r$placeholder[w] <- "Required..."
   r$helptext[w] <- "Put exactly one semicolon between radio button names."
   r$width[w] <- "66%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Selected radio button"
   r$placeholder[w] <- "Required..."
   r$helptext[w] <- "One and only one radio button must be selected.<br>Text must exactly match the button name."
   r$width[w] <- "33%"
   r$sameline[w] <- TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "direction"
   r$label[w] <- "Align the radio buttons"
   r$options[w] <- "in a column; across the page"
   r$value[w] <- "across the page"
   r$inline[w] = TRUE
w=w+1
   r[w,] <- imWidth
w=w+1
   r[w,] <- imOther
imBlankforms[["radio"]] <- r         # userType is "Radio buttons"

### Checkboxes input
w=1
   r <- imName
w=w+1
   r[w,] <- imLabel
w=w+1
   r[w,] <- imHelptext
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "options"
   r$label[w] <- "A name for each checkbox"
   r$placeholder[w] <- "Required..."
   r$helptext[w] <- "Minimum is one checkbox name.<br>No maximum.<br>Put exactly one semicolon between checkbox names."
   r$width[w] <- "66%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Checked checkboxes"
   r$helptext[w] <- "Zero to all checkboxes can be checked.<br>Put exactly one semicolon between them.<br>Text must exactly match the checkbox name."
   r$width[w] <- "33%"
   r$sameline[w] <- TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "direction"
   r$label[w] <- "Align the checkboxes"
   r$options[w] <- "in a column; across the page"
   r$value[w] <- "across the page"
   r$inline[w] = TRUE
w=w+1
   r[w,] <- imWidth
w=w+1
   r[w,] <- imOther
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


rm(r, w, imBlankforms, imGetBlankFORMrow, imName, imLabel, imHelptext, imWidth, imOther)


#imBlankforms <- readRDS(file="app/Blankforms.RDS")

