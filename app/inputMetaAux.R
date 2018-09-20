### inputMetaAux.R
### v1.0 - TomW - Sep 2018

### Pulled out because this function is needed by different files.

### Defines the columns in a form or FORM tibble
imGetBlankFORMrow = function(type) {
   return(tibble(
      type=type,                # see S$IN$codeTypes
      id="",                    # identifies one of these parameter for Forms; otherwise the input name
      name="",                  #    a short identifier for tables and figures (must be unique)
      label="",
      helptext="",
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

