library(shiny)
ui <- fluidPage(
  titlePanel("Body Fat Calculator"),
  sidebarPanel(
    numericInput('ABDOMEN', 'ABDOMEN', 92,min = 0),
    numericInput('WEIGHT', 'WEIGHT', 80,min = 0),
    numericInput('WRIST', 'WRIST', 18,min = 0),
    verbatimTextOutput("value1")
  )
)
server <- function(input, output) {
  model=function(ABDOMEN,WEIGHT,WRIST){
    bodyFat=-45.32548636+0.90924227*ABDOMEN+-0.01343309*WEIGHT*WRIST
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





