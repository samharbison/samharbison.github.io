library(shiny)
library(jsonlite)

ui <- shinyUI(fluidPage(

  headerPanel("Add Features"),
  sidebarPanel(width=4,
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
                        actionButton("remove", "Remove the last question!")
                 ), style='padding-bottom:8px; padding-top: 8px;'),
                fluidRow(
                   column(4,
                          actionButton('goButton',"Put it all together")
                   ))
  ),
               
  mainPanel(
    uiOutput('uiOutpt'),
    textOutput("text2"),
    tableOutput('tbl'),
    verbatimTextOutput("lists")
  )
))









server <- shinyServer(function(input, output, session) {
  features <- reactiveValues(renderd=c(1),
                             conv=c(50),
                             inlabels=c(),
                             outlabels=c())
  
  df <- eventReactive(input$goButton, {
    out <- lapply(features$renderd,function(i){
      fv <- paste0('InputLabel',i-1)
      vn <- paste0('InputType',i-1)
      data.frame(Variable=input[[vn]], Value=input[[fv]] )
    })
    do.call(rbind,out)
  })
  
  feat <- eventReactive(input$goButton, {
    out <- lapply(features$renderd,function(i){
      fv <- paste0('InputLabel',i)
      vn <- paste0('InputType',i)
      vo <- paste0('OutLabel',i)
      list("div_type" = input[[vn]],"label" = input[[fv]], "q3" = input[[vo]] )
    })
    toJSON(list("title" = input$title, "description" = input$desc, "questions" = out), pretty = T, auto_unbox = T)
  })
  
  output$nText <- renderText({
    ntext()
  })
  output$text2 <- renderText({ 
    paste(sprintf("You have selected feature: %s", paste(features$renderd,collapse=", ")))
  })
  
  output$tbl <- renderTable({
    df()
  })
  output$lists <- renderPrint({
      feat()
    
  })
  
  # Increment reactive values array used to store how may rows we have rendered
  observeEvent(input$add,{
    out <- lapply(features$renderd,function(i){
      fv <- paste0('InputLabel',i)
      vn <- paste0('InputType',i)
      vo <- paste0('OutLabel',i)
      list(inlabels=input[[vn]],outlabels=input[[vo]], conv=input[[fv]] )
    })
    df<-do.call(rbind,out)
    print(df)
    print(out)
    features$inlabels <- c(as.character(df$inlabels),' ')
    features$outlabels <- c(as.character(df$outlabels),' ')
    #print(c(features$inlabels,features$outlabels))
    
    features$renderd <- c(features$renderd, length(features$renderd)+1)
    #print(features$renderd)
    #print(names(features))
    features$conv<-c(df$conv,51-length(features$renderd))
  })
  
  observeEvent(input$remove,{
    features$renderd <- features$renderd[-length(features$renderd)]
  })
  
  # If reactive vector updated we render the UI again
  observe({
    output$uiOutpt <- renderUI({
      # Create rows
      rows <- lapply(features$renderd,function(i){
        fluidPage(
          fluidRow(h3(paste0("Question",i)),
          # duplicate choices make selectize poop the bed, use unique():
          column(12,
                 selectizeInput(paste0('InputType',i), 
                                    label = 'Input Name',selected=features$inlabels[i],
                                    choices=list("Short Answer"="input",
                                                 "Multiple Choice"= "select"),
                                    options = list(create = TRUE)))),
        fluidRow(
          column(12, 
                 textInput(paste0('InputLabel',i), label="Question Label", width = "600px"))
        ),
        fluidRow(
          column(12,
                 conditionalPanel(condition = paste(paste("input",paste0('InputType',i), sep = "."), "'select'", sep = "=="),
                                  textAreaInput(paste0("choices",i), "Enter Choices in order separated by a COMMA", width = "600px")
                                  
                                  
                                  
                )
          )
        )
        )
      })
      do.call(shiny::tagList,rows)
    })
  })
})

shinyApp(ui=ui,server=server)  
