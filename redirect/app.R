library(shiny)

jscode <- "Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = 'https://time.com/3306405/whale-penis-pelvis/';});"

ui <- fluidPage(
  tags$head(tags$script(jscode)),     
  # checkboxInput("Redirect","Redirect",value = FALSE)
)

server <- function(input, output, session) {
  
  # observeEvent(input$Redirect,{
  #   if(!input$Redirect){
  session$sendCustomMessage("mymessage", "mymessage")
  #   }
  # })
}

shinyApp(ui,server)