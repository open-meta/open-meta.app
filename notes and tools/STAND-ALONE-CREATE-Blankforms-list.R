### open-meta STAND-ALONE-Blankforms-list.R
### Tom Weishaar - Nov 2018 - v0.2
### This is a stand-alone file for creating a list of Blankform defintions.
###    This list is saved to a file and loaded during app startup.
###    The list is used to create customizable forms.

library(tibble)

source("app/imGetBlankFORMrow.R", local=TRUE)  # load the shared imGetBlankFORMrow function

imBlankforms <- list()         # This is a list of tibbles; one multi-row tibble for each type of input

imName <- imGetBlankFORMrow("text")
   imName$id <- "name"
   imName$label <- "Short name"
   imName$placeholder <- "Required..."
   imName$maxlength <- "15"
   imName$width <- "25%"
   imName$helptext <- "Used in tables and graphs.<br>Must be unique; can't be changed.<br>15 characters maximum."

imTable <- imGetBlankFORMrow("text")
   imTable$id <- "table"
   imTable$label <- "Data table"
   imTable$placeholder <- "Required..."
   imTable$sameline <- TRUE
   imTable$width <- "25%"
   imTable$helptext <- "Name of the table that data from this field will be stored in."

imColumn <- imGetBlankFORMrow("text")
   imColumn$id <- "column"
   imColumn$label <- "Data column"
   imColumn$placeholder <- "Required..."
   imColumn$sameline <- TRUE
   imColumn$width <- "25%"
   imColumn$helptext <- "Name of the table column that data from this field will be stored in."

imColor <- imGetBlankFORMrow("select")
   imColor$id <- "color"
   imColor$label <- "Color"
   imColor$sameline <- TRUE
   imColor$width <- "25%"
   imColor$options <- "default;blue;green;purple;yellow;red"
   imColor$value <- "default"
   imColor$helptext <- "Background color of this input."

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

imWidth <- imGetBlankFORMrow("text")
   imWidth$id <- "width"
   imWidth$label <- "Width of this input"
   imWidth$placeholder <- "Blank means 100%..."
   imWidth$helptext <- "Width in rems or 25%-33%-50%-66%-75%-100%"
   imWidth$width <- "33%"

imOther <- imGetBlankFORMrow("checkbox")            # options include disabled
   imOther$id <- "other"                            #    used by text, numeric, and select
   imOther$label <- "Other options"
   imOther$options <- "sameline; disabled"
   imOther$sameline <- TRUE
   imOther$width <- "50%"
   imOther$inline = TRUE


### Simple Text input
w=1
   r <- imName
w=w+1
   r[w,] <- imTable
w=w+1
   r[w,] <- imColumn
w=w+1
   r[w,] <- imColor
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
   r[w,] <- imTable
w=w+1
   r[w,] <- imColumn
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
   r[w,] <- imTable
w=w+1
   r[w,] <- imColumn
w=w+1
   r[w,] <- imColor
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
   r[w,] <- imTable
w=w+1
   r[w,] <- imColumn
w=w+1
   r[w,] <- imColor
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
   r[w,] <- imTable
w=w+1
   r[w,] <- imColumn
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
   r[w,] <- imTable
w=w+1
   r[w,] <- imColumn
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

### Spacer
w=1
   r <- imGetBlankFORMrow("text")
   r[w,] <- imColor
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "label"
   r$label[w] <- "Text for spacer, if any"
   r$width[w] <- "75%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "width"
   r$label[w] <- "Width of the spacer"
   r$placeholder[w] <- "Blank means 100%..."
   r$helptext[w] <- "Width in rems or 25%-33%-50%-66%-75%-100%"
   r$width[w] <- "33%"
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "placeholder"
   r$label[w] <- "Position of text in the &lt;div&gt;"
   r$options[w] <- "Left;Center;Right"
   r$value[w] <- "Center"
   r$sameline[w] <- FALSE
   r$inline[w] = TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Height of the spacer"
   r$placeholder[w] <- "Blank means 1rem..."
   r$helptext[w] <- "Height in rems; 0 to 1.3 are all 29px"
   r$sameline[w] <- TRUE
   r$width[w] <- "33%"
   r$sameline[w] <- FALSE
w=w+1
   r[w,] <- imGetBlankFORMrow("checkbox")            # options include disabled
   r$id[w] <- "other"                            #    used by text, numeric, and select
   r$label[w] <- "Other options"
   r$options[w] <- "sameline; disabled"
   r$inline[w] = TRUE
   r$sameline[w] <- FALSE
   r$width[w] <- "50%"
imBlankforms[["spacer"]] <- r          # userType is "Spacer"

### Hidden
w=1
   r <- imGetBlankFORMrow("text")
   r$id[w] <- "label"
   r$label[w] <- "Hidden name"
   r$width[w] <- "75%"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Hidden value"
   r$width[w] <- "75%"
imBlankforms[["hidden"]] <- r          # userType is "Hidden"

### Button
w=1
   r <- imColor
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "label"
   r$label[w] <- "Button name"
   r$placeholder[w] <- "Required..."
   r$width[w] <- "15"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "value"
   r$label[w] <- "Button action"
   r$placeholder[w] <- "Required..."
   r$width[w] <- "15"
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "width"
   r$label[w] <- "Width of button's &lt;div&gt;"
   r$placeholder[w] <- "Blank means 100%..."
   r$helptext[w] <- "Width in rems or 25%-33%-50%-66%-75%-100%"
   r$width[w] <- "15"
   r$sameline[w] <- FALSE
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "placeholder"
   r$label[w] <- "Position of button in the &lt;div&gt;"
   r$options[w] <- "Left;Center;Right"
   r$value[w] <- "Right"
   r$inline[w] = TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "btnsize"
   r$label[w] <- "Button size"
   r$options[w] <- "Extra-small;Small;Normal;Large"
   r$value[w] <- "Normal"
   r$inline[w] = TRUE
w=w+1
   r[w,] <- imGetBlankFORMrow("radio")
   r$id[w] <- "btnshape"
   r$label[w] <- "Button shape"
   r$options[w] <- "Pill;Normal;Square"
   r$value[w] <- "Normal"
   r$inline[w] = TRUE
   r$sameline[w] <- FALSE
w=w+1
   r[w,] <- imGetBlankFORMrow("text")
   r$id[w] <- "options"
   r$label[w] <- "Button class"
   r$helptext[w] <- "Most useful for margins like mt-3 or my-1"
   r$sameline[w] <- FALSE
w=w+1
   r[w,] <- imOther
   r$sameline[w] <- FALSE
imBlankforms[["button"]] <- r          # userType is "Button"

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


rm(r, w, imBlankforms, imGetBlankFORMrow, imName, imLabel, imHelptext, imWidth, imOther, imTable, imColumn, imColor)


#imBlankforms <- readRDS(file="app/Blankforms.RDS")

