library(shiny)
library(ggplot2)
library(DT)
library(survival)
library(ggvis)
library(dplyr)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(shinyjqui)
library(shinymaterial)
library(circlize)
# library(RCircos)

observe({
  y <- input$showpanelGeneCiR
  if (y == TRUE)
  {
    shinyjs::show(id = "SidebarGeneCiR", anim = TRUE, animType = "fade")
  }
  else if (y == FALSE)
  {
    shinyjs::hide(id = "SidebarGeneCiR", anim = TRUE, animType = "fade")
  }
})

# Show 2 genomes isoforms

output$genomesPlot <- renderPlot({
  

})


output$HomeGCR <- renderText({
  "Home of GeneCiR"
  input$genome1
  
})
