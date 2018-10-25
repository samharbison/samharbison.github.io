library(shiny)
library(jsonlite)
library(tidyverse)
library(googledrive)


source("inputsModule.R")

# This is the app of the module -------------------------------------------


ui <- fluidPage(
  titlePanel("MFS Survey Series Maker thing application"),
  sidebarLayout(
    sidebarPanel(
      # tags$style(type = "text/css", ".well {background: linear-gradient(-270deg, #ff3975 19%, #ffc229 83%);}"),
      fluidRow(column(
      12,
      p('First things first, you need to create a file for this survey. This measn that you need to provide a Survey Title and a an intro to the survey. 
         Both of these options will be placed in the survey themselves. The "Survey Title" will be the title of the survey, as well as the directory title and the title on the webpage header.
         The description will a text blurb that describes the survey to the participant. This is can be though of as an intro!'),
      p('Once you are done with these two boxes, click "Create File" and move on to adding questions'),
      textInput('title', "Survey Title")
    )),
    fluidRow(column(
      12,
      textAreaInput('desc', 'Introduction (description) of Survey', height = '200px')
    )),
    fluidRow(column(4,
                    actionButton("create", "Create file"))),
    width  = 12),
    mainPanel(
      inputsUI(1),
      tags$div(id = "placeholder"),
      fluidRow(column(
        4,
        actionButton("add", "Add a new question!")
      )),
      # fluidRow(column(4,
      #                 downloadButton("done", "Done"))),
      fluidRow(style = "padding-top: 10px;",
               column(4,
                      actionButton("save", "Save")))#,
      # verbatimTextOutput("outLS")
    )
  )
)



server <- function(input, output, session) {
  meta = reactiveValues(stuff = list(meta = NULL,
                                    questions = NULL))

  
  callModule(inputsServer, 1)
  
  btn = reactiveValues(value = 1)
  observeEvent(input$create, {
    meta$stuff$meta = list("title" = input$title,
                           "description" = input$desc)
  })
  
  observeEvent(input$add, {
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
    print(toJSON(meta$stuff, pretty = TRUE, auto_unbox = TRUE))
  })
  
  # output$done <- downloadHandler(
  #   filename = function() {paste(
  #     paste(
  #       strsplit(meta$stuff$meta$title,
  #                 " " )[[1]],
  #       collapse ="_"),
  #     ".json",
  #     sep = "")},
  #   content = function(file) {
  #     write(toJSON(meta$stuff, pretty = TRUE, auto_unbox = TRUE), file)
  # }
  # )
  observeEvent(input$save, {
    folder_name = paste(strsplit(meta$stuff$meta$title," " )[[1]],collapse ="_")
    drive_mkdir(folder_name, parent = "Surveys")
    filename = paste(folder_name,".json",sep = "")
    filePath = file.path(tempdir(), filename)
    write(toJSON(meta$stuff, pretty = TRUE, auto_unbox = TRUE), filePath)
    pa = drive_get(folder_name)
    drive_upload(filePath, path = pa$path)
  })
}















shinyApp(ui = ui, server = server)