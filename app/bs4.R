### Bootstrap4 functions
#   TomW - Dec 2017

# bs4(tag, args)
#   Mostly creates a <div></div> with appropriate Bootstrap4 classes, buttons, menus, and toolTIPS are a little different.
#   Possible tags are
#      c = container
#      r = row
#      c1 through c12 = column with width
#   Other named args (style="width:12px;") are passed as <div> properties
#      If there's an id arg, "shiny-html-output" is added to class list
#   Unnamed args are passed as <div> content

bs4 = function(flavor, ...) {   # most of the code comes from htmltools::tag()
   argList=list(...)                                                     # stuff all args but the first into a list
   argNames <- names(argList)                                            # get a vector of names
   if (is.null(argNames)) { argNames <- character(length(argList)) }     # if all are null, makes them ""; eg it's character(2), not as.character(2)
   namedArgs <- nzchar(argNames)                                         # nzchar makes a T/F vector, F=item is ""
   attribs <- htmltools:::dropNullsOrEmpty(argList[namedArgs])           # make the named items attribs
   children <- unname(argList[!namedArgs])                               # make the unnamed items children without any names (ie, not "")
   switch(flavor,
      "d"   = { },                                                       # Just a <div> with attributes and children
      "c"   = {attribs$class = paste("container", attribs$class)},       # 5 fixed widths with padding on sides if needed
      "cf"  = {attribs$class = paste("container-fluid container-fluid-spacious", attribs$class)}, # always 100% of viewport
      "r"   = {attribs$class = paste("row", attribs$class)},
      "ca"  = {attribs$class = paste("col-auto", attribs$class)},        # width based on content
      "c0"  = {attribs$class = paste("col", attribs$class)},             # width based on equal-sized columns
      "c1"  = {attribs$class = paste("col-1", attribs$class)},
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
      "cdb" = {attribs$class = paste("card-body", attribs$class)},
      "cdT" = {attribs$class = paste("card-title", attribs$class)},
      "cdt" = {attribs$class = paste("card-text", attribs$class)},
      "btn" = {return(bs4Button(attribs, children))},
      "cbx" = {return(bs4Checkbox(attribs, children))},
      "quill" =  {return(bs4Quill(attribs, children))},
      "dx"  = { },   # just like "d", but no shiny output with id; mostly needed by bs4Quill
      "mp"  = {return(HTML0('<ul class="nav nav-pills">', bs4Menus(attribs), '</ul>'))},    # menu as pills (or words with active=0)
      "mt"  = {return(HTML0('<ul class="nav nav-tabs">', bs4Menus(attribs), '</ul>'))},     # menu as tabs
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
      "tip"  = { # div for tip; put tool inside. Page also needs to call js$tipsOn() - maybe not in bs4
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
      if("b" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-primary")}  # blue
      if("g" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-success")}  # green
      if("i" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-info")}     # purple (in for info, p is used for pills)
      if("y" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-warning text-dark")}  # yellow with black text
      if("r" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-danger")}   # red
      if("d" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-dark")}     # black
      if("n" %in% attribs$q) {attribs$class = paste(attribs$class, "bg-secondary")}     # none
      if("ac" %in% attribs$q) {attribs$class = paste(attribs$class, "text-center")} # align center
      if("ar" %in% attribs$q) {attribs$class = paste(attribs$class, "text-right")}  # align right
      attribs$q = NULL
   }
   if(!is.null(attribs$align)) {attribs$align = NULL}
   if(!is.null(attribs$text)) {attribs$text = NULL}  # remove attribs used only by bs4() so they don't appear in HTML
   if(!is.null(attribs$links)) {attribs$links = NULL}
   if(!is.null(attribs$active)) {attribs$active = NULL}
   if(!is.null(attribs$id) && flavor!="dx") { # if there's an "id", make it a uiOutput unless flavor is "dx"
      attribs$class = paste(attribs$class, "shiny-html-output")
   }
   return(structure(list(name = "div", attribs = attribs, children = children), class = "shiny.tag"))
}

# Menus can either be links (urls) or onclicks.
# All menus need a "text=c()" attribute with one string for each menu item.
# All menus need an active=n attribute desinating, by number, which text item is currently active.
#    (if you don't want to highlight the active menu item, make active=0.)
# If you want links, you need links=c() with the links associated with the menu items.
# If you want onclicks, you need id="" and n=c(unique numbers); this is turned into "id_n";
#    and comes into an input$js.omclick observer that begins like this:
#    observeEvent(input$js.omclick, {
#       click = str_split(input$js.omclick, "_")
#       id = click[[1]][1]
#       n = click[[1]][2]
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
#         id = paste0(attribs$id, "_", attribs$n[i], "_", generate_rnd())
         action = paste0('id="', id, '"')
#         action = paste0('id="', id, '" onclick="omclick(\'', id, '\')"')
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
# if h="1" (or more), the editor size is fixed to that number of lines and the editor always has a scrollbar.
# if h="auto", the editor autosizes to the amount of text.
#    In this case, attribs$min="1" (or more) controls minimum height and attribs$max
#       controls the point at which a scroll bar appears and the box no longer grows. 35 or so is
#       probably the maximum max you'd want to use, as the Quill toolbar can get too far away from the
#       text you're editing with larger sizes, although some keyboard shortcuts are available.
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

# Unlike bs4Menus, which creates all the links in a menu, bs4Button creates just one button at a time.
#   Like menu items, buttons can have id="" and n=(a unique number) or, alternatively, they can have a uid=""
#   that combines id and n using an underline separator. Clicks appear on the input$js.omclick
#   observer (see Menus above). They can also include classes not already supported by q=c() and
#   could also support style="" or links, but neither has been needed or implemented yet.
bs4Button = function(attribs, children) {
   if(is.null(attribs$uid)) {                                                    # unique id
      attribs$uid = paste0(attribs$id, "_", attribs$n)                           # makes unique ID (required!!!)
   }
#   attribs$uid = paste0(attribs$uid, "_", generate_rnd())
#   attribs$onclick = paste0("omclick('", attribs$uid, "')")
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
#   return(HTML0('<button id="', attribs$uid, '" class="', attribs$class, '" onclick="', attribs$onclick, '">', unlist(children[[1]]), '</button>'))
}

# Button Groups: this smacks the buttons together; vertical might be useful with icons?
#   For examples of how these look see:
#   https://www.w3schools.com/bootstrap/bootstrap_button_groups.asp

# btn-group
# btn-group-vertical
# btn-group-justified
# btn-group-justified-spaced

# Button Toolbars: gather several Button Groups into one control
#    see: https://getbootstrap.com/docs/4.0/components/button-group/
# btn-toolbar
# btn-toolbar-divider

# Dropdown menu buttons: this is done by nesting button groups. See bootstrap docs for button groups.



# Some tests:

# bs4("btn", id="user", n=10, q=c("s", "k"), "Submit")



# bs4("r", id="a", HTML0("<p>Here I am</p>"))
# bs4("ca", id="a", HTML0("<p>Here I am</p>"))
# bs4("r", align=c("vt", "hs"))
#

#  How to set up a horizontal rule menu:
#     bs4("hrm", text=c("Register", "Login"), links=c("?profile","?login"), active=0)

#  How to set up a horizontal rule with text:
#     bs4("hrt", "Some Text"),


# Note: this code creates Shiny checkboxes; they are not supported by omClick() but return TRUE or FALSE to input$id
bs4Checkbox = function(attribs, children) {
   disabled = ""
   class = "form-group shiny-input-container mb-0"
   if("il" %in% attribs$q) { class = "form-group shiny-input-container form-check-inline" }   # inline checkboxes
   checked = ""
   if("ck" %in% attribs$q) {checked = ' checked="checked"'}
   disabled = ""
   if("d" %in% attribs$q) {disabled = ' disabled="disabled"'}
   return(HTML0(
      '<div class="ml-4">',
         '<div class="', class, '">',
            '<input class="form-check-input" type="checkbox" id="', attribs$id, '"', checked, disabled, '>',
            '<label class="form-check-label" for="', attribs$id, '">', unlist(children[[1]]), '</label>',
         '</div>',
      '</div>'
   ))
}
