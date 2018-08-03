### open-meta.app bs4.R
### Tom Weishaar - Dec 2017

### Bootstrap4 functions

### Based on the htmltools::tag(), this is a function that takes any number of parameters. Named parameters
#      (e.g., class="xyz") become attributes ("attribs" in the code) and unnamed parameters become "children".

# For the most part, what bs4() does is recursively create <div>s that have Bootstrap 4 classes. The very first bs4()
#     parameter, called "flavor", is a very short code indicating what Bootstrap class to add the the <div>. If you
#     also pass in your own class attribute, bs4() will add that to the <div>'s class. Any other attributes you pass
#     in will also be added to the <div>. The "children" get pasted together as the content between <div> and </div>.
#     So, for example:
#
#     bs4("r", style="width:400px;",    # Bootstrap 4 row
#        bs4("c6",                      # Bootstrap 4 6-unit column
#           "inner text"                # Text
#        )                              # Close column </div>
#     )                                 # Close row </div>
#
#     becomes the HTML:
#
#     <div style="width:400px;" class="row ">
#       <div class="col-6 ">inner text</div>
#     </div>
#
#     On-screen this is an invisible row with an invisible column half the width of the row with the words "inner text"
#        as the only visible thing on the screen . Bootstrap provides CSS that makes what appears on the screen the
#        same in different browsers and that nicely collapses on small phone screens. If you don't already know
#        Bootstrap coding, bs4() makes things a bit easier, but not so easy that you can ignore the Bootstrap 4
#        documentation at https://getbootstrap.com/docs/4.1/getting-started/introduction/
#
# The easiest way to review what "flavors" are available is to look through the code below. Note that if you pass in
#    an "id" attribute to a <div>, it will become a Shiny "ui" output, as if you had written "uiOutput(id)" instead.
#    Some of the flavors have additional required attributes - they make menus or buttons and more.
#

bs4 = function(flavor, ...) {                                            # the initial code comes from htmltools::tag()
   argList=list(...)                                                     # stuff all args but the first into a list
   argNames <- names(argList)                                            # get a vector of names
   if (is.null(argNames)) { argNames <- character(length(argList)) }     # if all are null, makes them ""; eg it's character(2), not as.character(2)
   namedArgs <- nzchar(argNames)                                         # nzchar makes a T/F vector, F=item is ""
   attribs <- htmltools:::dropNullsOrEmpty(argList[namedArgs])           # make the named items attribs
   children <- unname(argList[!namedArgs])                               # make the unnamed items children without any names (ie, not "")
   switch(flavor,
      "d"   = { },                                                       # Just a <div> with attributes and children
      "c"   = {attribs$class = paste("container", attribs$class)},       # container; 5 fixed widths with padding on sides if needed
      "cf"  = {attribs$class = paste("container-fluid container-fluid-spacious", attribs$class)}, # always 100% of viewport
      "r"   = {attribs$class = paste("row", attribs$class)},             # row
      "ca"  = {attribs$class = paste("col-auto", attribs$class)},        # width based on content
      "c0"  = {attribs$class = paste("col", attribs$class)},             # width based on equal-sized columns
      "c1"  = {attribs$class = paste("col-1", attribs$class)},           # width based on row having 12 width units
      "c2"  = {attribs$class = paste("col-2", attribs$class)},
      "c3"  = {attribs$class = paste("col-3", attribs$class)},
      "c4"  = {attribs$class = paste("col-4", attribs$class)},
      "c5"  = {attribs$class = paste("col-5", attribs$class)},
      "c6"  = {attribs$class = paste("col-6", attribs$class)},
      "c7"  = {attribs$class = paste("col-7", attribs$class)},
      "c8"  = {attribs$class = paste("col-8", attribs$class)},
      "c9"  = {attribs$class = paste("col-9", attribs$class)},
      "c10" = {attribs$class = paste("col-10", attribs$class)},
      "c11" = {attribs$class = paste("col-11", attribs$class)},
      "c12" = {attribs$class = paste("col-12", attribs$class)},
      "cd"  = {attribs$class = paste("card", attribs$class)},            # This set has to do with cards:
      "cdh" = {attribs$class = paste("card-header", attribs$class)},     # https://getbootstrap.com/docs/4.0/components/card/
      "cdb" = {attribs$class = paste("card-body", attribs$class)},       # You could have an image in the card but not in the body
      "cdT" = {attribs$class = paste("card-title", attribs$class)},      # Header is inside a color bar above the body
      "cdt" = {attribs$class = paste("card-text", attribs$class)},       # Title-text go within body
      "btn" = {return(bs4Button(attribs, children))},                    # button (see code below)
      "cbx" = {return(bs4Checkbox(attribs, children))},                  # checkbox (see code below)
      "quill" =  {return(bs4Quill(attribs, children))},                  # Quill editor (see code below and https://quilljs.com/)
      "chart" =  {return(bs4Chart(attribs, children))},                  # chart.js
      "pgn" =  {return(bs4Pagination(attribs, children))},               # pagination
      "cmt" =  {return(bs4Comments(attribs, children))},                 # comment display widget
      "dx"  = { },                                       # just like "d", but no shiny output with id; mostly needed by bs4Quill
      "mp"  = {return(HTML0('<ul class="nav nav-pills">', bs4Menus(attribs), '</ul>'))}, # menu as pills (or words with active=0)
      "mt"  = {return(HTML0('<ul class="nav nav-tabs">', bs4Menus(attribs), '</ul>'))},  # menu as tabs
      "md"  = {return(HTML0('<ul class="nav nav-bordered mb-0 clearfix">', bs4Menus(attribs), '</ul><hr class="mt-0 mb-3">'))}, # bs4-dashboard underlined tabs
      "hr"  = {attribs$class = paste("hr-divider pt-3 pb-4", attribs$class)},
      "hr0"  = {attribs$class = paste("hr-divider", attribs$class)},
      "hrt"  = {  # horizontal rule with text from unnamed item
                  attribs$class = paste("hr-divider mt-3 mb-4", attribs$class)
                  children[[1]] = HTML0('<h3 class="hr-divider-content hr-divider-heading">', children[[1]],'</h3>')
               },
      "hrm"  = {  # horizonal rule with a menu
                  attribs$class = paste("hr-divider mt-3 mb-4", attribs$class)
                  children[[1]] = HTML0('<h3 class="hr-divider-content hr-divider-heading"><ul class="nav nav-pills">',
                                       bs4Menus(attribs), '</ul></h3>')
               },
      "tip"  = { # div for tip; put tool inside. Page also needs to call js$tipsOn()
                 # bs4("tip", p="t", t="Click me", bs4("d", "Click"))
                  attribs["data-toggle"] = "tooltip"
                  if(is.null(attribs$p)) { attribs$p = "b"}
                  switch(attribs$p,
                     "t" = {attribs$p = 1},
                     "r" = {attribs$p = 2},
                     "b" = {attribs$p = 3},
                     "l" = {attribs$p = 4},
                     warning("Invalid location on tool tip (attrib$p must be t, r, b or l)")
                   )
                  attribs["data-placement"] = c("top", "right", "bottom", "left")[attribs$p]
                  attribs$p = NULL
                  attribs$title = attribs$t
                  attribs$t = NULL
               },
      stop(paste0('In bs4(), "', flavor, '" is an unrecognized flavor.'))
   )
   # if(flavor %in% c("c", "cf", "r") && length(children)>0 ) {           # prevents nesting from working!!
   #    stop("In bs4(), only columns can contain content.")
   # }
   if(!is.null(attribs$align)) {
      if(any(c("c", "cf", "hr") %in% flavor)) {
         stop("In bs4(), align options can't go on a container or horizontal rule.")
      }
      if(flavor=="r") {
         if(any(c("st", "sc", "sb", "ng") %in% attribs$align)) {
            stop("In bs4(), there are column align options on a row.")
         }
         if("vt" %in% attribs$align) {attribs$class = paste(attribs$class, "align-items-start")}
         if("vc" %in% attribs$align) {attribs$class = paste(attribs$class, "align-items-center")}
         if("vb" %in% attribs$align) {attribs$class = paste(attribs$class, "align-items-end")}
         if("hs" %in% attribs$align) {attribs$class = paste(attribs$class, "justify-content-start")}
         if("hc" %in% attribs$align) {attribs$class = paste(attribs$class, "justify-content-center")}
         if("he" %in% attribs$align) {attribs$class = paste(attribs$class, "justify-content-end")}
         if("ha" %in% attribs$align) {attribs$class = paste(attribs$class, "justify-content-around")}
         if("hb" %in% attribs$align) {attribs$class = paste(attribs$class, "justify-content-between")}
      }
      if(str_sub(flavor, 1, 1)=="c") {
         if(any(c("vt", "vc", "vb", "hs", "hc", "he", "ha", "hb") %in% attribs$align)) {
            stop("In bs4(), there are row align options on a column or container.")
         }
         if("st" %in% attribs$align) {attribs$class = paste(attribs$class, "align-self-start")}      # yep, only top to bottom
         if("sc" %in% attribs$align) {attribs$class = paste(attribs$class, "align-self-center")}     #   no right to left
         if("sb" %in% attribs$align) {attribs$class = paste(attribs$class, "align-self-end")}
         if("ng" %in% attribs$align) {attribs$class = paste(attribs$class, "no-gutters")}
      }
      attribs$align = NULL
   }
   if(!is.null(attribs$q)) {
      if("b" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-primary")}   # blue
      if("g" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-success")}   # green
      if("i" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-info")}      # purple (in for info, p is used for pills)
      if("y" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-warning text-dark")}  # yellow with black text
      if("r" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-danger")}    # red
      if("d" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-dark")}      # black
      if("n" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-secondary")} # none
      if("ac" %in% attribs$q) {attribs$class = paste(attribs$class, "text-center")} # align center
      if("ar" %in% attribs$q) {attribs$class = paste(attribs$class, "text-right")}  # align right
      attribs$q = NULL
   }
   if(!is.null(attribs$align)) {attribs$align = NULL}
   if(!is.null(attribs$text)) {attribs$text = NULL}             # remove attribs used only by bs4() so they don't appear in HTML
   if(!is.null(attribs$links)) {attribs$links = NULL}
   if(!is.null(attribs$active)) {attribs$active = NULL}
   if(!is.null(attribs$id) && flavor!="dx") {                   # if there's an "id", make it a uiOutput unless flavor is "dx"
      attribs$class = paste(attribs$class, "shiny-html-output")
   }
   return(structure(list(name = "div", attribs = attribs, children = children), class = "shiny.tag"))
}
#  How to set up a horizontal rule menu:
#     bs4("hrm", text=c("Register", "Login"), links=c("?profile","?login"), active=0)

#  How to set up a horizontal rule with text:
#     bs4("hrt", "Some Text"),

# Menus can either be links (urls) or on-clicks.
# All menus need a "text=c()" attribute with one string for each menu item.
# All menus need an active=n attribute desinating, by number, which text item is currently active.
#    (if you don't want to highlight the active menu item, make active=0.)
# If you want links, you need links=c() with the links associated with the menu items.
# If you want on-clicks, you need id="" and n=c(unique numbers); this is turned into "id_n";
#    and comes into an input$js.omclick observer that begins like this:
#    observeEvent(input$js.omclick, {
#       click = str_split(input$js.omclick, "_")
#       id = click[[1]][1]
#       n = click[[1]][2]
# There's an example of this observer on almost every page.
bs4Menus = function(attribs) {
   if((length(attribs$text)!=length(attribs$links)) && (length(attribs$text)!=length(attribs$n))) {
      stop('In bs4Menus(), text vector and links/onclick vector are different lengths.')
   }
   menuLines = ""
   for(i in 1:length(attribs$text)) {
      if(i==attribs$active) {active=" active"} else {active=""}
      if(!is.null(attribs$links)) {
         action = paste0('href="', attribs$links[i], '" ')
      } else {
         id = paste0(attribs$id, "_", attribs$n[i])
         action = paste0('id="', id, '"')
      }
      menuLines = paste0(menuLines, '<li class="nav-item"><a ',
                    action,
                    ' class="nav-link',
                    active,
                    '" aria-controls="',
                    attribs$text[i],
                    '">',
                    attribs$text[i],
                    '</a></li>')
   }
   return(menuLines)
}

# bs4("quill", id="xyz", h="N", text-to-edit) starts up the JavaScript-based Quill editor.
# Although you can override the default height of the editing window, the way it works by
#    default is pretty cool.
# To recover the edited text:
   # js$getEdit(id)
   # observeEvent(input$js.editorText, {
   #    id = input$js.editorText[1]        # The id of the text
   #    t  = input$js.editorText[2]        # The edited text
# NOTE: The embedded script doesn't work as a js$ function because it has to run after its container exists;
#   which means it has to be called after the render completes, not from inside the render function.
bs4Quill = function(attribs, children) {
   if(is.null(attribs$h)) {
      attribs$h="auto"
      attribs$min="1"
      attribs$max="65"    # in vertical height units; 100 is size of browser window
   }
   if(attribs$h=="auto") {
      style = paste0("height:auto; min-height:", attribs$min, "rem; max-height:", attribs$max, "vh;")
      placeholder = "placeholder: 'Edit window grows with text...',"
   } else {
      style = paste0('height:', attribs$h, 'rem;')
      placeholder = ""
   }
   return(tagList(
      bs4("dx", id=attribs$id, style=style, class="ql-container mb-3", HTML(unlist(children))),
      HTML0(
         "<script>
            var ", attribs$id, " = new Quill('#", attribs$id, "', {
               modules: {
                  toolbar: [
                     ['bold', 'italic', 'underline', { 'script': 'sub'}, { 'script': 'super' }, 'code'],
                     [{ 'list': 'bullet' }, { 'list': 'ordered'}, { 'indent': '-1'}, { 'indent': '+1' }],
                     ['link', 'blockquote', 'code-block'],
                     ['clean']
                  ]
               },",
               placeholder,
               "theme: 'snow',
               formats: ['bold', 'italic', 'underline', 'script', 'blockquote', 'indent', 'list', 'link', 'code', 'code-block', 'header']
            });
         </script>"
      )
   ))
}
# In the above code, "formats:" has to do with what HTML the editor will accept via paste.
# The "snow" theme as delievered by open-meta.org is slightly tweaked from the factory version.

# bs4Button() - called by code above after bs4("btn"...)
# Unlike bs4Menus, which creates all the links in a menu, bs4Button creates just one button at a time.
#   Like menu items, buttons can have id="" and n=(a unique number) or, alternatively, they can have a uid=""
#   that combines id and n using an underline separator. Clicks appear on the input$js.omclick
#   observer (see Menus above). They can also include classes not already supported by q=c() and
#   could also support style="" or links, but neither has been needed or implemented yet.
bs4Button = function(attribs, children) {
   if(is.null(attribs$uid)) {                                                     # unique id
      attribs$uid = paste0(attribs$id, "_", attribs$n)                            # makes unique ID (required!!!)
   }
   attribs$class = paste("btn border-dark", attribs$class)
   if("xs" %in% attribs$q) {attribs$class = paste(attribs$class, "btn-xs")}
   if("s"  %in% attribs$q) {attribs$class = paste(attribs$class, "btn-sm")}
   if("l"  %in% attribs$q) {attribs$class = paste(attribs$class, "btn-lg")}
   if("p"  %in% attribs$q) {attribs$class = paste(attribs$class, "btn-pill")}     # this means semi-circular right/left ends
   if("q"  %in% attribs$q) {attribs$class = paste(attribs$class, "btn-square")}   # this means unrounded corners
   if("sg" %in% attribs$q) {attribs$class = paste(attribs$class, "btn-group-sm")}
   if("lg" %in% attribs$q) {attribs$class = paste(attribs$class, "btn-group-lg")}
   if("b"  %in% attribs$q) {attribs$class = paste(attribs$class, "btn-primary")}  # blue
   if("g"  %in% attribs$q) {attribs$class = paste(attribs$class, "btn-success")}  # green
   if("i"  %in% attribs$q) {attribs$class = paste(attribs$class, "btn-info")}     # purple (in for info, p is used for pills)
   if("y"  %in% attribs$q) {attribs$class = paste(attribs$class, "btn-warning")}  # yellow
   if("r"  %in% attribs$q) {attribs$class = paste(attribs$class, "btn-danger")}   # red
   if("d"  %in% attribs$q) {attribs$class = paste(attribs$class, "btn-dark")}     # black
   if("on" %in% attribs$q) {attribs$class = paste(attribs$class, "btn-borderless")}
   if("ob" %in% attribs$q) {attribs$class = paste(attribs$class, "btn-outline-primary")}
   if("og" %in% attribs$q) {attribs$class = paste(attribs$class, "btn-outline-success")}
   if("oi" %in% attribs$q) {attribs$class = paste(attribs$class, "btn-outline-info")}
   if("oy" %in% attribs$q) {attribs$class = paste(attribs$class, "btn-outline-warning")}
   if("or" %in% attribs$q) {attribs$class = paste(attribs$class, "btn-outline-danger")}
   if("od" %in% attribs$q) {attribs$class = paste(attribs$class, "btn-outline-dark")}
   if("k"  %in% attribs$q) {attribs$class = paste(attribs$class, "btn-link")}
   return(HTML0('<button id="', attribs$uid, '" class="', attribs$class, '">', unlist(children[[1]]), '</button>'))
}

# Button Groups: this smacks the buttons together; vertical might be useful with icons?
#   For examples of how these look see:
#   https://www.w3schools.com/bootstrap/bootstrap_button_groups.asp

# Dropdown menu buttons: this is done by nesting button groups. See bootstrap docs for button groups.

# btn-group
# btn-group-vertical
# btn-group-justified
# btn-group-justified-spaced

# Button Toolbars: gather several Button Groups into one control
#    see: https://getbootstrap.com/docs/4.0/components/button-group/
# btn-toolbar
# btn-toolbar-divider


# Note: this code creates Shiny checkboxes; they return TRUE or FALSE to input$id
# supports vectorized ids and names plus T/F attribs vectors for ck-checked (required) and dis-disabled and il-inline (optional)
bs4Checkbox = function(attribs, children) {
   cbxNames = children[[1]]
   cbxL = length(cbxNames)
   if(is.null(attribs$il)) {               # Allow missing or length 1 attribs$il  (inline)
      attribs$il = rep(FALSE, cbxL)
   } else {
      if(length(attribs$il)==1) {
         attribs$il = rep(attribs$il, cbxL)
      }
   }
   if(is.null(attribs$dis)) {              # Allow missing or length 1 attribs$dis  (disabled)
      attribs$dis = rep(FALSE, cbxL)
   } else {
      if(length(attribs$dis)==1) {
         attribs$dis = rep(attribs$dis, cbxL)
      }
   }
   if(!all(length(attribs$id), length(attribs$il), length(attribs$dis), length(attribs$ck) %in% cbxL)) {
      stop("In bs4Checkbox(), attribs vectors have different lengths")
   }
# ck = a T/F vector where T means checked
   checked = rep("", cbxL)
   checked[attribs$ck] = ' checked="checked"'
# il = a T/F vector where T means inline
   class = rep("form-group shiny-input-container mb-0", cbxL)
   class[attribs$il] = "shiny-input-container-inline form-check-inline"
# dis = a T/F vector where T means disabled
   disabled = rep("", cbxL)
   disabled[attribs$dis] = ' disabled="disabled"'
   return(HTML0(paste0(
      '<div class="ml-4 ', class, '">',
         '<input class="form-check-input" type="checkbox" id="', attribs$id, '"', checked, disabled, '>',
         '<label class="form-check-label" for="', attribs$id, '">', cbxNames, '</label>',
      '</div>', collapse=""
   )))
}

# This function uses Chart.js to draw javascript graphs. See: https://www.chartjs.org/docs/latest/
bs4Chart = function(attribs, children) {
   # Must haves
   if(is.null(attribs$id))     {stop("No id attribute in bs4Chart()")}               # Each chart needs a unique id
   if(is.null(attribs$labels)) {stop("No labels attribute in bs4Chart()")}           # String vector of labels
   if(is.null(attribs$data))   {stop("No data attribute in bs4Chart()")}             # Numeric vector of data points
   # Probably want to specify
   if(is.null(attribs$c)) { attribs$c <- 6}                                          # Width of column the graph is embedded in
   if(is.null(attribs$title1)) { attribs$title1 <- ""}                               # Muted title at bottom
   if(is.null(attribs$title2)) { attribs$title2 <- ""}                               # h4 title at bottom
   if(is.null(attribs$zeroText)) { attribs$zeroText <- ""}                           # text above gray ghost graphs
   if(is.null(attribs$colors)) { attribs$colors <- rep("#6c757d", length(attribs$data))}  # segment colors (default is gray)
   # Default likely fine                                                             #  see toolkit-inverse.css line 50 for colors
   if(is.null(attribs$legend)) { attribs$legend <- "false"}                          # display legend? (lower case!!!)
   if(is.null(attribs$bdc)) { attribs$bdc <- rep("#252830", length(attribs$data))}   # border color (default is background color)
   if(is.null(attribs$bw)) { attribs$bw <- 4}                                        # border width
   if(is.null(attribs$type)) { attribs$type <- "doughnut"}                           # graph type
   if(is.null(attribs$cpc)) { attribs$cpc <- 73}                                     # doughnut cut out percentage
   if(is.null(attribs$fc)) { attribs$fc <- "#f8f9fa"}                                # font color for legends

   if(!all(c(length(attribs$labels), length(attribs$colors), length(attribs$bdc)) %in% length(attribs$data))) {
      stop("In bs4Chart(), data, labels, colors, and bdc attribs need to be the same length.")
   }

   return(HTML0('
<div class="col-', attribs$c, ' mb-3 text-center">',                     # Chart comes embedded in a column, this is its width
   attribs$zeroText,                                                     # Optional text for graphs with no data (gray ghost graphs)
  '<div class="w-3 mx-auto">',                                           # This centers the graph in the middle 75% of the column
     '<canvas id="', attribs$id, '" width="100%" height="100%"></canvas>
      <script>
         var ctx = document.getElementById("', attribs$id, '").getContext("2d");
         var ', attribs$id, ' = new Chart(ctx, {
            type: "', attribs$type, '",
            data: {
               labels: [', paste0("\"", paste0(attribs$labels, collapse='", "'), "\""), '],
               datasets: [{
                  data: [', paste0(attribs$data, collapse=","), '],
                  backgroundColor: [', paste0("\"", paste0(attribs$colors, collapse='", "'), "\""), '],
                  borderColor: [', paste0("\"", paste0(attribs$bdc, collapse='", "'), "\""), '],
                  borderWidth:', attribs$bw, '
               }]
            },
            options: {
              cutoutPercentage: ', attribs$cpc, ',
              rotation: 1.571,
               legend: {
                  display: ', attribs$legend, ',
                  labels: {
                     fontColor:"', attribs$fc, '"
                  }
               }
            }
         });
      </script>
   </div>
   <strong class="text-muted">', attribs$title1, '</strong>
   <h4>', attribs$title2, '</h4>
</div>')
)
}

bs4Pagination = function(attribs, children) {
   # Must haves
   if(is.null(attribs$np)) {stop("No active page attribute (ap=) in bs4Pagination()")}
   if(is.null(attribs$ap)) {stop("No number of pages attribute (np=) in bs4Pagination()")}
   # Could haves
   if(is.null(attribs$id)) {attribs$id="pgn"}  # Needed only if there's more than one pagination section on a page

   if(attribs$np<2) {return("")} # If there's only one page, no need for a widget

   pgnStart <- HTML0('<nav aria-label="Citation list pagination"><ul class="pagination">',
                     '<li class="page-item"><a id=', paste0(attribs$id, "_1"), ' class="page-link">First Page</a></li>')
   pgnEnd <-   HTML0('<li class="page-item"><a id=', paste0(attribs$id, "_", attribs$np), ' class="page-link">Last Page</a></li>',
                     '</ul></nav>')

   pLeft <- max(1, attribs$ap-2)               # pLeft is the first page in the widget , can't be less than 1
   pLeft <- min(pLeft , max(1, attribs$np-4))  #   but it can't push the right end over the number of pages, either

   pgnMiddle <- ""
   for(p in (pLeft:min(pLeft+4, attribs$np))) {
      pgnMiddle <- paste0(pgnMiddle,
                          ifelse(p==attribs$ap, '<li class="page-item active"><a id="', '<li class="page-item"><a id="'),
                          paste0(attribs$id, "_", p), '" class="page-link">', p, '</a></li>')
   }
   return(
      bs4("r", bs4("c12", class="mb-2", HTML(pgnStart, pgnMiddle, pgnEnd)))
   )
}

# This widget finds the comments for an item, displays them (perhaps in a scrollable window?), and
#    adds functionality for posting a new comment (and editing old ones?)
# NOTE: this widget has nothing to do with saving comments, which must be handled by the page
bs4Comments = function(attribs, children) {

# There are comment tables for the overall system and for each project.
# Each comment has a an "item" and "itemID" field that associate that comment with a particular item, like a
#   particular protocol, search, review, citation, etc.
# This widget should get all comments for the current item and display them, adding the verUser and verTime.
#   Perhaps members and non-members of a project should be identified.
# It should also display a field for adding a new comment and perhaps editing an old one (via a modal?)
# Imagining something simple like the comments in Stack Overflow.

    return(
      bs4("r", bs4("c12", HTML0("<p>Commenting still under construction.</p>")))
   )
}
