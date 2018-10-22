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
  
    filename = paste(paste(strsplit(meta$stuff$meta$title, " " )[[1]], collapse ="_"), ".json", sep = ""),
    content = function(file) {
    write(toJSON(meta$stuff, pretty = TRUE, auto_unbox = TRUE), file)
  }
  )
  
  
}

shinyApp(ui, server)
