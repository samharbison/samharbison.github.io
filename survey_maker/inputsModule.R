
# Inserting quetions Module -----------------------------------------------


# inputs UI ---------------------------------------------------------------

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


# inputs Server -----------------------------------------------------------

inputsServer <- function(input, output, session) {
  ns <- session$ns
  
  output$InputType <- renderUI({
    selectInput(
      inputId = ns("InputType"),
      label = "Choose an input Type",
      choices = list("Short Answer" = "input",
                     "Multiple Choice" = "select"),
      width = '100%'
    )
  })
  
  output$InputLabel <- renderUI({
    textInput(
      inputId = ns("InputLabel"),
      label = "This is where you put the question",
      width = '100%'
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
      width = "100%",
      height = '100px'
    )
  })
  
}
