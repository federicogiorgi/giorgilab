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
source("/srv/shiny-server/Tucana/data/LucaFunct.R")

allTissue = insertPath("/srv/shiny-server/Corto/data/")
filePath="/srv/shiny-server/Corto/data/"
DTTF<-data.frame()
DTTF1<-data.frame()
DTTF2<-data.frame()
DTTFfinal<-data.frame()

genes = get(load("/srv/shiny-server/Tucana/data/geneList.rda"))
cotfArray<-read.delim("/srv/shiny-server/Tucana/data/cotfgenes_2018_08_06.txt",as.is=TRUE,sep="\t",header=FALSE)
signalingArray<-read.delim("/srv/shiny-server/Tucana/data/signaling_2018_08_06.txt",as.is=TRUE,sep="\t",header=FALSE)
tfArray<-read.delim("/srv/shiny-server/Tucana/data/tfgenes_2018_08_06.txt",as.is=TRUE,sep="\t",header=FALSE)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  updateSelectizeInput(session = session, inputId = 'g1', choices = genes, server = TRUE)
  updateSelectizeInput(session = session, inputId = 'g2', choices = genes, server = TRUE)
  
  observe( {
    x <- input$selectMode
    #Gestione tipo di ricerca
    if (x == "---")
    {
      shinyjs::hide("tissue2",anim = TRUE, animType = "slide")
      shinyjs::hide("g2",anim = TRUE, animType = "slide")
      shinyjs::hide("typeOfGenes",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadData",anim = TRUE, animType = "slide")
      shinyjs::hide("tissue1",anim = TRUE, animType = "slide")
      shinyjs::hide("coupletissue",anim = TRUE, animType = "slide")
      shinyjs::hide("g1",anim = TRUE, animType = "slide")
      shinyjs::hide("selectDataType",anim = TRUE, animType = "slide")
      shinyjs::hide("corrType",anim = TRUE, animType = "slide")
      shinyjs::hide("firstPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("secondPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRow",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGTum",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGHeal",anim = TRUE, animType = "fade")
      shinyjs::hide("topCoexTis",anim = TRUE, animType = "fade")
      shinyjs::hide("DEGTable",anim = TRUE, animType = "fade")
      shinyjs::hide("info",anim = TRUE, animType = "fade")
      shinyjs::hide("curvaKaplanMeier",anim = TRUE, animType = "fade")
      
      shinyjs::show("Home",anim = TRUE, animType = "slide")
    }
    else if (x == "Show 2 genes correlation in a tissue")
    {
      shinyjs::hide("tissue2",anim = TRUE, animType = "slide")
      shinyjs::hide("coupletissue",anim = TRUE, animType = "slide")
      shinyjs::hide("typeOfGenes",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadData",anim = TRUE, animType = "slide")
      shinyjs::hide("topCoexTis",anim = TRUE, animType = "fade")
      shinyjs::hide("secondPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRow",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGTum",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGHeal",anim = TRUE, animType = "fade")
      shinyjs::hide("DEGTable",anim = TRUE, animType = "fade")
      shinyjs::hide("curvaKaplanMeier",anim = TRUE, animType = "fade")
      shinyjs::hide("Home",anim = TRUE, animType = "fade")
      
      shinyjs::show("tissue1",anim = TRUE, animType = "slide")
      shinyjs::show("g1",anim = TRUE, animType = "slide")
      shinyjs::show("selectDataType",anim = TRUE, animType = "slide")
      shinyjs::show("corrType",anim = TRUE, animType = "slide")
      shinyjs::show("g2",anim = TRUE, animType = "slide")
      shinyjs::show("firstPlot",anim = TRUE, animType = "fade")
      shinyjs::show("info",anim = TRUE, animType = "fade")
      
    }
    else if (x == "Compare 2 genes correlation in two tissues")
    {
      shinyjs::hide("typeOfGenes",anim = TRUE, animType = "slide")
      shinyjs::hide("coupletissue",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadData",anim = TRUE, animType = "slide")
      shinyjs::hide("topCoexTis",anim = TRUE, animType = "fade")
      shinyjs::hide("DEGTable",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRow",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGTum",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGHeal",anim = TRUE, animType = "fade")
      shinyjs::hide("curvaKaplanMeier",anim = TRUE, animType = "fade")
      shinyjs::hide("Home",anim = TRUE, animType = "fade")
      
      shinyjs::show("g1",anim = TRUE, animType = "slide")
      shinyjs::show("selectDataType",anim = TRUE, animType = "slide")
      shinyjs::show("corrType",anim = TRUE, animType = "slide")
      shinyjs::show("tissue2",anim = TRUE, animType = "slide")
      shinyjs::show("g2",anim = TRUE, animType = "slide")
      shinyjs::show("tissue1",anim = TRUE, animType = "slide")
      shinyjs::show("firstPlot",anim = TRUE, animType = "fade")
      shinyjs::show("secondPlot",anim = TRUE, animType = "fade")
      shinyjs::show("info",anim = TRUE, animType = "fade")
    }
    else if (x == "Search the main coexpressor genes for a gene in a tissue")
    {
      shinyjs::hide("tissue2",anim = TRUE, animType = "slide")
      shinyjs::hide("g2",anim = TRUE, animType = "slide")
      shinyjs::hide("coupletissue",anim = TRUE, animType = "slide")
      shinyjs::hide("firstPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("secondPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("DEGTable",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGTum",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGHeal",anim = TRUE, animType = "fade")
      shinyjs::hide("curvaKaplanMeier",anim = TRUE, animType = "fade")
      shinyjs::hide("Home",anim = TRUE, animType = "fade")
      
      shinyjs::show("typeOfGenes",anim = TRUE, animType = "slide")
      shinyjs::show("tissue1",anim = TRUE, animType = "slide")
      shinyjs::show("g1",anim = TRUE, animType = "slide")
      shinyjs::show("selectDataType",anim = TRUE, animType = "slide")
      shinyjs::show("corrType",anim = TRUE, animType = "slide")
      shinyjs::show("downloadData",anim = TRUE, animType = "slide")
      shinyjs::show("topCoexTis",anim = TRUE, animType = "fade")
      shinyjs::show("plotRow",anim = TRUE, animType = "fade")
      shinyjs::show("info",anim = TRUE, animType = "fade")
    }
    else if (x == "Search the main DEGs for the couples healthy/tumor")
    {
      shinyjs::hide("tissue2",anim = TRUE, animType = "slide")
      shinyjs::hide("g2",anim = TRUE, animType = "slide")
      shinyjs::hide("tissue1",anim = TRUE, animType = "slide")
      shinyjs::hide("topCoexTis",anim = TRUE, animType = "fade")
      shinyjs::hide("firstPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("secondPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRow",anim = TRUE, animType = "fade")
      shinyjs::hide("curvaKaplanMeier",anim = TRUE, animType = "fade")
      shinyjs::hide("Home",anim = TRUE, animType = "fade")
      
      shinyjs::show("coupletissue",anim = TRUE, animType = "fade")
      shinyjs::show("g1",anim = TRUE, animType = "slide")
      shinyjs::show("typeOfGenes",anim = TRUE, animType = "slide")
      shinyjs::show("downloadData",anim = TRUE, animType = "slide")
      shinyjs::show("selectDataType",anim = TRUE, animType = "slide")
      shinyjs::show("corrType",anim = TRUE, animType = "slide")
      shinyjs::show("DEGTable",anim = TRUE, animType = "fade")
      shinyjs::show("plotRowDEGTum",anim = TRUE, animType = "fade")
      shinyjs::show("plotRowDEGHeal",anim = TRUE, animType = "fade")
      shinyjs::show("info",anim = TRUE, animType = "fade")
    }
    else if (x == "Perform a KaplanMeier study")
    {
      
      output$curvaKaplanMeier <- renderPlot({
        req(input$selectMode == "Perform a KaplanMeier study")
        time <- c(1,3,4,5,5,6,7,7,7,8)
        status <- c(0,1,0,1,1,0,1,1,0,1)
        popol <- data.frame(time, status)
        fit <- survfit(Surv(time, status) ~ 1, data=popol)
        plot(fit)
      })
      
      shinyjs::hide("tissue2",anim = TRUE, animType = "slide")
      shinyjs::hide("g2",anim = TRUE, animType = "slide")
      shinyjs::hide("typeOfGenes",anim = TRUE, animType = "slide")
      shinyjs::hide("tissue1",anim = TRUE, animType = "slide")
      shinyjs::hide("coupletissue",anim = TRUE, animType = "slide")
      shinyjs::hide("g1",anim = TRUE, animType = "slide")
      shinyjs::hide("selectDataType",anim = TRUE, animType = "slide")
      shinyjs::hide("corrType",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadData",anim = TRUE, animType = "slide")
      shinyjs::hide("topCoexTis",anim = TRUE, animType = "fade")
      shinyjs::hide("firstPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("secondPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("DEGTable",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRow",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGTum",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGHeal",anim = TRUE, animType = "fade")
      shinyjs::hide("info",anim = TRUE, animType = "fade")
      shinyjs::hide("Home",anim = TRUE, animType = "fade")
      
    }
  })
  
  # Show 2 genes correlation
  
  output$firstPlot <- renderPlot({
    if (input$selectMode == "Show 2 genes correlation in a tissue" |
        input$selectMode == "Compare 2 genes correlation in two tissues")
    {
      for(tis in allTissue)
      {
        if(input$tissue1 == tis)
        {
          pathTis<-calcPath(filePath,input$selectDataType,input$tissue1)
          primplot<-outPlot(pathTis,input$tissue1,input$g1,input$g2,input$corrType)
          primplot
        }
      }
    }
  })
  
  output$secondPlot <- renderPlot({
    if(input$selectMode == "Compare 2 genes correlation in two tissues")
    {
      for(tis in allTissue)
      {
        if(input$tissue2 == tis)
        {
          pathTis<-calcPath(filePath,input$selectDataType,input$tissue2)
          secplot<-outPlot(pathTis,input$tissue2,input$g1,input$g2,input$corrType)
          secplot
        }
      }
    }
  })
  
  output$topCoexTis <- renderDataTable({
    if(input$selectMode == "Search the main coexpressor genes for a gene in a tissue")
    {
      pathTis<-calcPath(filePath,input$selectDataType,input$tissue1)
      if(input$typeOfGenes == "All")
      {
        DTTF<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis, genes)
      }
      else if(input$typeOfGenes == "Transcription Factors")
      {
        message("righe prima di calcCorr ",nrow(DTTF))
        DTTF<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis, tfArray)
      }
      else if(input$typeOfGenes == "co-Transcription factors")
      {
        DTTF<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis, cotfArray)
      }
      else if(input$typeOfGenes == "signaling proteins")
      {
        DTTF<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis, signalingArray)
      }
      DTTF
    }
  })
  
  output$plotRow <- renderPlot({
    if(input$selectMode == "Search the main coexpressor genes for a gene in a tissue")
    {
      req(input$topCoexTis_rows_selected)
      sGene <- input$topCoexTis_rows_selected
      sndGen <- as.character(DTTF[sGene,"Gene2"])
      pathTis <- calcPath(filePath,input$selectDataType,input$tissue1)
      terPlot <- outPlot(pathTis,input$tissue1,input$g1,sndGen,input$corrType)
      terPlot
    }
  })
  
  output$DEGTable <- renderDataTable({
    if(input$selectMode == "Search the main DEGs for the couples healthy/tumor")
    {
      
      message(paste(Sys.time(),"  searching DEGs..."))
      tisArra <- unlist(strsplit(input$coupletissue, "[,]"))
      message(tisArra[1])
      message(tisArra[2])
      pathTis1 <- calcPath(filePath,input$selectDataType,tisArra[1])
      pathTis2 <- calcPath(filePath,input$selectDataType,tisArra[2])
      
      if(input$typeOfGenes == "All")
      {
        DTTF1<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis1, genes)
      }
      else if(input$typeOfGenes == "Transcription Factors")
      {
        DTTF1<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis1, tfArray)
      }
      else if(input$typeOfGenes == "co-Transcription factors")
      {
        DTTF1<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis1, cotfArray)
      }
      else if(input$typeOfGenes == "signaling proteins")
      {
        DTTF1<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis1, signalingArray)
      }
      
      if(input$typeOfGenes == "All")
      {
        DTTF2<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis2, genes)
      }
      else if(input$typeOfGenes == "Transcription Factors")
      {
        DTTF2<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis2, tfArray)
      }
      else if(input$typeOfGenes == "co-Transcription factors")
      {
        DTTF2<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis2, cotfArray)
      }
      else if(input$typeOfGenes == "signaling proteins")
      {
        DTTF2<<-calcCorr(input$corrType,input$typeOfGenes,input$g1,pathTis2, signalingArray)
      }
      
      DTTFfinal<<-rbind(DTTF1,DTTF2)
      DTTFfinal<<-DTTFfinal[order(DTTFfinal$Gene2),]
      DTTFfinal<<-DEGsTab(DTTFfinal)
      DTTFfinal<<-DTTFfinal[rowSums(is.na(DTTFfinal)) != ncol(DTTFfinal),]
      DTTFfinal<<-DTTFfinal[order(DTTFfinal$Correlation, decreasing = TRUE),]
      DTTFfinal
    }
  })
  
  output$plotRowDEGTum <- renderPlot({
    if(input$selectMode == "Search the main DEGs for the couples healthy/tumor")
    {
      req(input$DEGTable_rows_selected)
      sGene <- input$DEGTable_rows_selected
      sndGen <- as.character(DTTFfinal[sGene,"Gene2"])
      tisArra <- unlist(strsplit(input$coupletissue, "[,]"))
      message(tisArra[1])
      pathTis1 <- calcPath(filePath,input$selectDataType,tisArra[1])
      DEGPlot1 <- outPlot(pathTis1,tisArra[1],input$g1,sndGen,input$corrType)
      DEGPlot1
    }
  })
  
  output$plotRowDEGHeal <- renderPlot({
    if(input$selectMode == "Search the main DEGs for the couples healthy/tumor")
    {
      req(input$DEGTable_rows_selected)
      sGene <- input$DEGTable_rows_selected
      sndGen <- as.character(DTTFfinal[sGene,"Gene2"])
      tisArra <- unlist(strsplit(input$coupletissue, "[,]"))
      message(tisArra[2])
      pathTis2 <- calcPath(filePath,input$selectDataType,tisArra[2])
      DEGPlot2 <- outPlot(pathTis2,tisArra[2],input$g1,sndGen,input$corrType)
      DEGPlot2
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$tissue1,input$g1,".csv", sep = "")
    },
    content = function(file) {
      write.csv(DTTF, file, row.names = FALSE)
    }
  )
  
  output$Home <- renderText({
    "Home of Tucana"
    
  })
  
  # output$info <- renderText({
  #   paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  # })
  
})