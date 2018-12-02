# This is pulled out here because it's used by the app and stand-alone files.


### Defines the columns in a form or FORM tibble
imGetBlankFORMrow <- function(type) {
   return(tibble(
      type=type,                # see S$IN$codeTypes
      formname="",
      id="",                    # identifies one of these parameters for Forms; otherwise the input name
      name="",                  #    a short identifier for tables and figures (must be unique)
      table="",                 # Table the data collected by this input is stored in
      column="",                # Column that data collected by this input is stored in
      color="",
      label="",
      helptext="",
      width="",                 # for width the possible options are 25%-50%-75%-100% or in rems
      value="",                 # selected option(s) for select-radio-checkbox; otherwise current value
      placeholder="",
      maxlength="",             # Number of characters accepted
      min="",                   # These three are for the numeric input type
      max="",                   #  ...the value of these numbers will be in strings, however
      step="",
      btnsize="",
      btnshape="",
      options="",               # all the options for select-radio-checkbox, separated by ";"
                                # for "form", the possible options are: inline, sameline, disabled, and locked
      inline=FALSE,             # whether radio buttons and checkboxes are inline or in a column
      sameline=FALSE,           # whether the current input should be on the same line as the previous input (if it would fit)
      disabled=FALSE,           # whether this input should be disabled
      locked=FALSE,             # whether this FORMrow should be locked
      order=999                 # used to reorder the inputs
   ))
}
