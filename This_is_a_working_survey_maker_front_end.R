# Module for questions



inputsUI <- function(id) {
  ns = NS(id)
  
  tags$div(id = paste0('input', id),
           fluidPage(
             fluidRow(h3(paste0("Question ", id)),
                      column(12,
                             uiOutput(ns(
                               'InputType'
                             )))),
             fluidRow(column(12,
                             uiOutput(ns(
                               "InputLabel"
                             )))),
             conditionalPanel('output.show == "select"', ns = ns,
                              fluidRow(column(12,
                                              uiOutput(
                                                ns("Choices")
                                              ))))
             
           ))
}


inputsServer <- function(input, output, session) {
  ns <- session$ns
  
  output$InputType <- renderUI({
    selectInput(
      inputId = ns("InputType"),
      label = "Choose an input Type",
      choices = list("Short Answer" = "input",
                     "Multiple Choice" = "select"),
      width = '600px'
    )
  })
  
  output$InputLabel <- renderUI({
    textInput(
      inputId = ns("InputLabel"),
      label = "This is where you put the question",
      width = '600px'
    )
  })
  
  output$show <- reactive({
    input$InputType
  })
  outputOptions(output, 'show', suspendWhenHidden = FALSE)
  
  
  output$numChoices <- renderUI({
    numericInput(
      inputId = ns("numChoices"),
      label = "How many Choices?",
      value = 2
    )
  })
  
  output$Choices <- renderUI({
    textAreaInput(
      inputId = ns("Choices"),
      label = "Choices, separate them by hitting the enter button",
      width = "600px",
      height = '100px'
    )
  })
  
}


library(shiny)
library(jsonlite)
library(tidyverse)



# This is the app of the module -------------------------------------------



ui <- fluidPage(
  sidebarPanel(
    style = "position:fixed;width:22%;",
    fluidRow(column(12,
                    h3('Features'),
                    textInput('title', "Survey Title")
    )), 
    fluidRow(
      column(12,
             textAreaInput('desc', 'brief description of survey', height = '200px')
      )),
    fluidRow(
      column(4,
             actionButton("add", "Add a new question!")
      )),
    fluidRow(
      column(4,
             downloadButton("done", "Done")
      )),
    width  = 3
  ),
  mainPanel(
    inputsUI(1),
    tags$div(id = "placeholder"),
    verbatimTextOutput("outLS")
  )
)

server <- function(input, output, session) {
  meta = reactiveValues(stuff = list(meta = NULL,
                                    questions = NULL))

  
  callModule(inputsServer, 1)
  
  btn = reactiveValues(value = 1)
  
  observeEvent(input$add, {
    meta$stuff$meta = list("title" = input$title,
                     "description" = input$desc)
    meta$stuff$questions[[btn$value]] = list(
      "InputType" = input[[NS(btn$value, 'InputType')]],
      "InputLabel" = input[[NS(btn$value, "InputLabel")]],
      "Choices" = if(input[[NS(btn$value, 'InputType')]] == "select") {
        as.list(strsplit(input[[NS(btn$value, "Choices")]], "\n")[[1]])
      } else {
        NULL
      }
    )
    
    btn$value = btn$value + 1
    btn.tmp = btn$value
    callModule(inputsServer, btn.tmp)
    insertUI(selector = "#placeholder",
             where = "beforeEnd",
             ui = inputsUI(btn.tmp))
  })
  output$outLS <- renderPrint({
    print((meta$stuff))
  })
  
  output$done <- downloadHandler(
    filename = function() {paste(
      paste(
        strsplit(meta$stuff$meta$title,
                  " " )[[1]],
        collapse ="_"),
      ".json",
      sep = "")},
    content = function(file) {
      write(toJSON(meta$stuff, pretty = TRUE, auto_unbox = TRUE), file)
  }
  )
}

shinyApp(ui, server)



# tring to parse together everything into HTML ----------------------------


x = jsonlite::read_json("~/samharbison.github.io/something_here.json")

x$meta
html_parser = function(x) {

# Header Stuff (links to external libraries, css files, google fon --------

head = paste(
  '<html lang="en">
  <head>
  <meta charset="UTF-8">
  <title>',
  paste('MFS R2R:', x$meta$title, sep = " "),
  '</title>
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/normalize/5.0.0/normalize.min.css">
  <link rel="stylesheet" href="',
  paste(paste('style',paste(strsplit(x$meta$title, " ")[[1]], collapse = "_"), sep = "_"), "css", sep = '.'),
  '">
  <link href="https://fonts.googleapis.com/css?family=Poppins:800" rel="stylesheet">
  <!-- Latest compiled and minified CSS -->
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
  <!-- jQuery library -->
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"></script>
  <!-- Latest compiled JavaScript -->
  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
  </head>'
  , sep = "")


head_locate = paste(
  '<html lang="en">
  <head>
  <meta charset="UTF-8">
  <title>',
  paste('MFS R2R:', x$meta$title, sep = " "),
  '</title>
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/normalize/5.0.0/normalize.min.css">
  <link rel="stylesheet" href="',
  paste(paste('style',paste(strsplit(x$meta$title, " ")[[1]], collapse = "_"), sep = "_"), "css", sep = '.'),
  '">
  <link href="https://fonts.googleapis.com/css?family=Poppins:800" rel="stylesheet">
  <!-- Latest compiled and minified CSS -->
  <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css">
  <!-- jQuery library -->
  <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"></script>
  <!-- Latest compiled JavaScript -->
  <script src="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js"></script>
  <script>

  var x = document.getElementById("latlong");

  function getLocation() {
    if (navigator.geolocation) {
      navigator.geolocation.getCurrentPosition(showPosition);
    } else {
      x.value = "Geolocation is not supported by this browser.";
    }
  }

  function showPosition(position) {
    x.value = position.coords.latitude + "," + position.coords.longitude;
  }
  </script>
  </head>'
  , sep = "")






# Title and description ---------------------------------------------------


title = paste(
  '<header>
  <div class="wrapper">
  <h1>',x$meta$title,'</h1>
  </div>
  </header>', collapse = "\n")

description = paste(
  '<p class="form-group">', x$meta$description,'<p>',sep = "", collapse="\n")


# how to make inputs ------------------------------------------------------

stuff = lapply(1:length(x$questions), function(i) {
  if (x$questions[[i]]$InputType=='input') {
    paste(
      '<div class="form-group">
      <label for="',
      paste(
        paste(
          strsplit(x$meta$title, " ")[[1]],
          collapse = "_"),
        'question',
        i,
        sep = "_"),
      '">',
      x$questions[[i]]$InputLabel,
      '</label>
      <input type="text" class="form-control" name="" id=',
      paste(paste(strsplit(x$meta$title, " ")[[1]], collapse = "_"), 'question', i, sep = "_"),
      '>
      </div>
      <br>',
      sep = ""
    )
  } else if (x$questions[[i]]$InputType=='select') {
    options =lapply(1:length(x$questions[[i]]$Choices), function(j) {
      paste(
        '<option value="',j,'">',x$questions[[i]]$Choices[[j]][1],
        '</option>',
        sep = ""
      )
    })
    
    paste(
      '<div class="form-group">
      <label for="',
      paste(paste(strsplit(x$meta$title, " ")[[1]], collapse = "_"), 'question', i, sep = "_"),
      '">',
      x$questions[[i]]$InputLabel,
      '</label>
      <br>
      <select class="form-control" name="" id="',
      paste(paste(strsplit(x$meta$title, " ")[[1]], collapse = "_"), 'question', i, sep = "_"),
      '">
      <option style="display:none;"></option>\n',
      paste(do.call("c", options), collapse = "\n"),
      '\n</select>
      </div>
      <br>',
      sep=""
    )
  }
})


geolocate_script = '
<script>

var x = document.getElementById("latlong");

function getLocation() {
if (navigator.geolocation) {
navigator.geolocation.getCurrentPosition(showPosition);
} else {
x.value = "Geolocation is not supported by this browser.";
}
}

function showPosition(position) {
x.value = position.coords.latitude + "," + position.coords.longitude;

}
</script>'

locate = TRUE
if (locate == TRUE) {
  cat(
    paste(
      head,
      '<body onload="getLocation()">',
      title,
      '<section>',
      description,
      '<form action=""  method="POST" id="ss-form" target="hidden_iframe" onsubmit="submitted=true;">',
      '<input name="" id="latlong">',
      paste(do.call("c", stuff), collapse = "\n"),
      '</form>',
      geolocate_script,
      '</section>
      </body>
      </html>',
      sep = "\n"
    ),
    file = "~/samharbison.github.io/tester.html"
  )
} else {
  cat(
    paste(
      head,
      '<body>',
      title,
      '<section>',
      description,
      '<form action=""  method="POST" id="ss-form" target="hidden_iframe" onsubmit="submitted=true;">',
      paste(do.call("c", stuff), collapse = "\n"),
      '</form>
      </section>
      </body>
      </html>',
      sep = "\n"
    ),
    file = "~/samharbison.github.io/tester.html"
  )
}
}  














