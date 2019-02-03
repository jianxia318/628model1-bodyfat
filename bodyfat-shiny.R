library(shiny)
ui <- fluidPage(
  titlePanel("Body Fat Calculator for male"),
  sidebarPanel(
    h5("Please input the corresponding numbers"),
    numericInput('ABDOMEN', 'ABDOMEN(cm)', 92,min = 0),
    numericInput('WEIGHT', 'WEIGHT(kg)', 80,min = 0),
    numericInput('WRIST', 'WRIST(cm)', 18,min = 0),
    submitButton("Update View", icon("refresh"))
    #verbatimTextOutput("value1")
  ),
  mainPanel(
    h3("Body Fat Percentage"),
    verbatimTextOutput("value1")
  )
)

server <- function(input, output) {
  model=function(ABDOMEN,WEIGHT,WRIST){
    bodyFat=-45.32+0.91*ABDOMEN+-0.01*WEIGHT*WRIST
    if (bodyFat>0 & bodyFat<100){
      return(bodyFat)
    }
    else{
      print("please enter true value")
    }
  }
  output$value1=renderPrint({model(input$ABDOMEN,input$WEIGHT,input$WRIST)})
}

shinyApp(ui = ui, server = server)