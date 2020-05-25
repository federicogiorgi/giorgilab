suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(survival))
suppressPackageStartupMessages(library(ggvis))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinyjqui))
suppressPackageStartupMessages(library(shinymaterial))
suppressPackageStartupMessages(library(parallel))
suppressPackageStartupMessages(library(gridExtra))

# Define server logic
shinyServer(function(input, output, session) {
  
  # use observe() to manage an event that changes the inputs
  # observe({
  #   
  # })
  
  # use onclick() to manage an event on a click of a particular input
  # onclick("g1", {
  #   updateSelectizeInput(session, "g1", selected = "")
  # })
  
  # Show 2 genes correlation
  
  # use output$PLOT_NAME <- renderPlot({}) to print a plot in the output
  # output$firstPlot <- renderPlot({
  #   if ((input$selectMode == "Show 2 genes correlation in a tissue" | 
  #        input$selectMode == "Compare 2 genes correlation in two tissues") &
  #       input$g1!="" & input$g2!="")
  #   {
  #     par(mar=c(4.8,5.1,4.8,2.1))
  #     pathTis1<-calcPath(filePath,input$selectDataType,input$tissue1)
  #     primplot<-outPlot(pathTis1, input$tissue1, input$g1, input$g2, input$corrType,
  #                       input$cex, input$width, input$height)
  #     primplot
  #   }
  # })
  
  # use output$TABLE_NAME <- renderDataTable({}) to print a table in the output
  # output$topCoexTis <- renderDataTable({
  #   if(input$selectMode == "Search the main coexpressor genes for a gene in a tissue")
  #   {
  #     pathTis1<-calcPath(filePath,input$selectDataType,input$tissue1)
  #     DTTF<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis1, input$tissue1)
  #     DTTF
  #   }
  # })
  
  # use output$NAME_OF_DOWNLOAD_BUTTON <-downloadHandler() to download a PDF or a CSV
  # in the function filename you decide the name of the downloaded file
  # in the content function you save what you want into the file
  #
  # esempio CSV
  # output$downloadCSV <- downloadHandler(
  #   filename = function() {
  #     paste(input$tissue1,"_",input$g1,".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(DTTF, file, row.names = FALSE)
  #   }
  # )
  #
  # esempio PDF
  # output$downloadPDF<- downloadHandler(
  #   filename = function() {
  #     paste(input$tissue1,"_",input$g1,"_",input$g2,".pdf", sep = "")
  #   },
  #   content = function(file) {
  #     
  #     if(input$selectMode == "Show 2 genes correlation in a tissue" &
  #        input$g1!="" & input$g2!=""){
  #       pdf(file)
  #         pathTis1<-calcPath(filePath,input$selectDataType,input$tissue1)
  #         print(outPlot(pathTis1, input$tissue1, input$g1, input$g2, input$corrType,
  #                       input$cex, input$width, input$height))
  #       dev.off()
  #     }
  #     else if(input$selectMode == "Compare 2 genes correlation in two tissues" &
  #             input$g1!="" & input$g2!=""){
  #       pdf(file)
  #         pathTis1<-calcPath(filePath,input$selectDataType,input$tissue1)
  #         print(outPlot(pathTis1, input$tissue1, input$g1, input$g2, input$corrType,
  #                       input$cex, input$width, input$height))
  #         pathTis2<-calcPath(filePath,input$selectDataType,input$tissue2)
  #         print(outPlot(pathTis2, input$tissue2, input$g1, input$g2, input$corrType,
  #                       input$cex, input$width, input$height))
  #       dev.off()
  #     }
  #     
  #   }
  # )
})