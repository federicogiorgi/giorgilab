Packages <- c("shiny","ggplot2","DT","survival","ggvis","dplyr","shinyjs","shinyBS","shinydashboard",
              "shinyjqui","shinymaterial","parallel","gridExtra")
lapply(Packages, function(x){
  suppressPackageStartupMessages(library)
  }
)
source("/srv/shiny-server/Tucana/data/LucaFunct.R")

dataframe1 <- data.frame("Study Abbreviation" = c("NBL","ACC","BLCA","BRCA","CESC",
                                                  "CHOL","COAD","DLBC","ESCA","GBM",
                                                  "HNSC","KICH","KIRC","KIRP","LAML",
                                                  "LGG","LIHC","LUAD","LUSC","MESO",
                                                  "OV","PAAD","PCPG","PRAD","READ",
                                                  "SARC","SKCM","STAD","TGCT","THCA",
                                                  "THYM","UCEC","UCS","UVM"),
                         "Study Name" = c("NeuroBLastoma",
                                          "Adrenocortical carcinoma",
                                          "Bladder Urothelial Carcinoma",
                                          "Breast invasive carcinoma",
                                          "Cervical squamous cell carcinoma
                                          and endocervical adenocarcinoma",
                                          "Cholangiocarcinoma",
                                          "Colon adenocarcinoma",
                                          "Lymphoid Neoplasm Diffuse Large B-cell Lymphoma",
                                          "Esophageal carcinoma",
                                          "Glioblastoma multiforme",
                                          "Head and Neck squamous cell carcinoma",
                                          "Kidney Chromophobe",
                                          "Kidney renal clear cell carcinoma",
                                          "Kidney renal papillary cell carcinoma",
                                          "Acute Myeloid Leukemia",
                                          "Brain Lower Grade Glioma",
                                          "Liver hepatocellular carcinoma",
                                          "Lung adenocarcinoma",
                                          "Lung squamous cell carcinoma",
                                          "Mesothelioma",
                                          "Ovarian serous cystadenocarcinoma",
                                          "Pancreatic adenocarcinoma",
                                          "Pheochromocytoma and Paraganglioma",
                                          "Prostate adenocarcinoma",
                                          "Rectum adenocarcinoma",
                                          "Sarcoma",
                                          "Skin Cutaneous Melanoma",
                                          "Stomach adenocarcinoma",
                                          "Testicular Germ Cell Tumors",
                                          "Thyroid carcinoma",
                                          "Thymoma",
                                          "Uterine Corpus Endometrial Carcinoma",
                                          "Uterine Carcinosarcoma",
                                          "Uveal Melanoma"))

filePath<-"/srv/shiny-server/DATA/"
pathGeneList<-"/srv/shiny-server/DATA/"
allTissue = insertPath("/srv/shiny-server/DATA/")
DTTF<-data.frame()
DTTF1<-data.frame()
DTTF2<-data.frame()
DTTFfinal<-data.frame()

# Define server logic
shinyServer(function(input, output, session) {
  
  genelist1 <- eventReactive(input$tissue1,{
    read.csv(paste0(pathGeneList,"geneList",input$tissue1,"-expmat.csv"),stringsAsFactors = FALSE)
  })
  genelist2 <- eventReactive(input$tissue2,{
    read.csv(paste0(pathGeneList,"geneList",input$tissue2,"-expmat.csv"),stringsAsFactors = FALSE)
  })
  
  g1 <- eventReactive(input$g1,{input$g1})
  g2 <- eventReactive(input$g2,{input$g2})
  
  inizio<-TRUE
  observeEvent(c(input$selectMode, input$tissue1),{
    x <- input$selectMode
    write.csv(c(input$g1,input$g2),file = "/srv/shiny-server/Tucana/tmp.txt")
    #Gestione tipo di ricerca
    if (x == "Show 2 genes correlation in a tissue")
    {
      shinyjs::hide("tissue2",anim = TRUE, animType = "slide")
      shinyjs::hide("typeOfGenes",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadCSV",anim = TRUE, animType = "slide")
      
      shinyjs::hide("topCoexTis",anim = TRUE, animType = "fade")
      shinyjs::hide("secondPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRow",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGT1",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGT2",anim = TRUE, animType = "fade")
      shinyjs::hide("DEGTable",anim = TRUE, animType = "fade")

      shinyjs::show("downloadPDF",anim = TRUE, animType = "slide")
      shinyjs::show("tissue1",anim = TRUE, animType = "slide")
      shinyjs::show("g2",anim = TRUE, animType = "slide")
      shinyjs::show("downloadAllPlotsG",anim = TRUE, animType = "slide")
      shinyjs::show("downloadAllPlotsT",anim = TRUE, animType = "slide")
      shinyjs::show("downloadAllPlotsN",anim = TRUE, animType = "slide")
      
      shinyjs::show("firstPlot",anim = TRUE, animType = "fade")

      previousGenes<-read.csv("/srv/shiny-server/Tucana/tmp.txt")
      if(is.na(previousGenes[1,2]) & is.na(previousGenes[2,2]) & inizio==TRUE) {
        updateSelectizeInput(session = session, inputId = 'g1',
                             choices = sort(genelist1()[,]), selected = "E2F3", server = TRUE)
        updateSelectizeInput(session = session, inputId = 'g2',
                             choices = sort(genelist1()[,]), selected = "MYC", server = TRUE)
      }
      else if (previousGenes[1,2]!=g1() & previousGenes[2,2]!=g2()) {
        updateSelectizeInput(session = session, inputId = 'g1', choices = sort(genelist1()[,]),
                             selected = previousGenes[1,2], server = TRUE)
        updateSelectizeInput(session = session, inputId = 'g2', choices = sort(genelist1()[,]),
                             selected = previousGenes[2,2], server = TRUE)
      }
    }
    else if (x == "Compare 2 genes correlation in two tissues")
    {
      shinyjs::hide("typeOfGenes",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadCSV",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadAllPlotsG",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadAllPlotsT",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadAllPlotsN",anim = TRUE, animType = "slide")
      
      shinyjs::hide("topCoexTis",anim = TRUE, animType = "fade")
      shinyjs::hide("DEGTable",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRow",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGT1",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGT2",anim = TRUE, animType = "fade")

      shinyjs::show("downloadPDF",anim = TRUE, animType = "slide")
      shinyjs::show("tissue2",anim = TRUE, animType = "slide")
      shinyjs::show("g2",anim = TRUE, animType = "slide")
      shinyjs::show("tissue1",anim = TRUE, animType = "slide")
      
      shinyjs::show("firstPlot",anim = TRUE, animType = "fade")
      shinyjs::show("secondPlot",anim = TRUE, animType = "fade")

      gltot<-sort(unique(rbind(genelist1(),genelist2()))[,])
      previousGenes<-read.csv("/srv/shiny-server/Tucana/tmp.txt")
      if(is.na(previousGenes[1,2]) & is.na(previousGenes[2,2])) {
        updateSelectizeInput(session = session, inputId = 'g1',
                             choices = gltot, selected = "E2F3", server = TRUE)
        updateSelectizeInput(session = session, inputId = 'g2',
                             choices = gltot, selected = "MYC", server = TRUE)
      }
      else if(previousGenes[1,2]!=g1() & previousGenes[2,2]!=g2()) {
        updateSelectizeInput(session = session, inputId = 'g1', choices = gltot,
                             selected = previousGenes[1,2], server = TRUE)
        updateSelectizeInput(session = session, inputId = 'g2', choices = gltot,
                             selected = previousGenes[2,2], server = TRUE)
      }
    }
    else if (x == "Search the main coexpressor genes for a gene in a tissue")
    {
      shinyjs::hide("tissue2",anim = TRUE, animType = "slide")
      shinyjs::hide("g2",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadPDF",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadAllPlotsG",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadAllPlotsT",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadAllPlotsN",anim = TRUE, animType = "slide")
      
      shinyjs::hide("firstPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("secondPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("DEGTable",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGT1",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRowDEGT2",anim = TRUE, animType = "fade")

      shinyjs::show("typeOfGenes",anim = TRUE, animType = "slide")
      shinyjs::show("tissue1",anim = TRUE, animType = "slide")
      shinyjs::show("downloadCSV",anim = TRUE, animType = "slide")
      
      shinyjs::show("topCoexTis",anim = TRUE, animType = "fade")
      shinyjs::show("plotRow",anim = TRUE, animType = "fade")

      previousGenes<-read.csv("/srv/shiny-server/Tucana/tmp.txt")
      if(is.na(previousGenes[1,2]) & is.na(previousGenes[2,2])) {
        updateSelectizeInput(session = session, inputId = 'g1',
                             choices = sort(genelist1()[,]), selected = "E2F3", server = TRUE)
        updateSelectizeInput(session = session, inputId = 'g2',
                             choices = sort(genelist1()[,]), selected = "MYC", server = TRUE)
      }
      else if(previousGenes[1,2]!=g1() & previousGenes[2,2]!=g2()) {
        updateSelectizeInput(session = session, inputId = 'g1', choices = sort(genelist1()[,]),
                             selected = previousGenes[1,2], server = TRUE)
        updateSelectizeInput(session = session, inputId = 'g2', choices = sort(genelist1()[,]),
                             selected = previousGenes[2,2], server = TRUE)
      }
    }
    else if (x == "Search the main DEGs for the couples healthy/tumor")
    {
      shinyjs::show("tissue2",anim = TRUE, animType = "slide")
      shinyjs::hide("g2",anim = TRUE, animType = "slide")
      shinyjs::show("tissue1",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadPDF",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadAllPlotsG",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadAllPlotsT",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadAllPlotsN",anim = TRUE, animType = "slide")
      
      shinyjs::hide("topCoexTis",anim = TRUE, animType = "fade")
      shinyjs::hide("firstPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("secondPlot",anim = TRUE, animType = "fade")
      shinyjs::hide("plotRow",anim = TRUE, animType = "fade")

      shinyjs::show("typeOfGenes",anim = TRUE, animType = "slide")
      shinyjs::show("downloadCSV",anim = TRUE, animType = "slide")
      
      shinyjs::show("DEGTable",anim = TRUE, animType = "fade")
      shinyjs::show("plotRowDEGT1",anim = TRUE, animType = "fade")
      shinyjs::show("plotRowDEGT2",anim = TRUE, animType = "fade")

      genesListDegs<-sort(unique(rbind(genelist1(),genelist2()))[,])
      previousGenes<-read.csv("/srv/shiny-server/Tucana/tmp.txt")
      if(is.na(previousGenes[1,2]) & is.na(previousGenes[2,2])) {
        updateSelectizeInput(session = session, inputId = 'g1',
                             choices = genesListDegs, selected = "E2F3", server = TRUE)
        updateSelectizeInput(session = session, inputId = 'g2',
                             choices = genesListDegs, selected = "MYC", server = TRUE)
      }
      else if(previousGenes[1,2]!=g1() & previousGenes[2,2]!=g2()) {
        updateSelectizeInput(session = session, inputId = 'g1', choices = genesListDegs,
                             selected = previousGenes[1,2], server = TRUE)
        updateSelectizeInput(session = session, inputId = 'g2', choices = genesListDegs,
                             selected = previousGenes[2,2], server = TRUE)
      }
    }
    inizio<<-FALSE
    if(grepl("nrc_NBL",input$tissue1) | grepl("kocak_NBL",input$tissue1)){
      shinyjs::useShinyjs()
      shinyjs::disable(selector = "[type=radio][value=FPKM]")
      shinyjs::runjs("$('[type=radio][value=FPKM]').parent().parent().addClass('disabled').css('opacity', 0.4)")
      shinyjs::disable(selector = "[type=radio][value=TPM]")
      shinyjs::runjs("$('[type=radio][value=TPM]').parent().parent().addClass('disabled').css('opacity', 0.4)")
      updateRadioButtons(session = session, inputId = "selectDataType", selected = "Expmat")
    }
    else{
      shinyjs::useShinyjs()
      shinyjs::enable(selector = "[type=radio][value=FPKM]")
      shinyjs::runjs("$('[type=radio][value=FPKM]').parent().parent().addClass('enabled').css('opacity', 1)")
      shinyjs::enable(selector = "[type=radio][value=TPM]")
      shinyjs::runjs("$('[type=radio][value=TPM]').parent().parent().addClass('enabled').css('opacity', 1)")
    }
  })
               
  onclick("g1", {
    updateSelectizeInput(session, "g1", selected = "")
  })
  onclick("g2", {
    updateSelectizeInput(session, "g2", selected = "")
  })
  
  # Show 2 genes correlation
  
  output$firstPlot <- renderPlot({
    if ((input$selectMode == "Show 2 genes correlation in a tissue" | 
         input$selectMode == "Compare 2 genes correlation in two tissues") &
        g1()!="" & g2()!="")
    {
      par(mar=c(4.8,5.1,4.8,2.1))
      pathTis1<-calcPath(filePath,input$selectDataType,input$tissue1)
      primplot<-outPlot(pathTis1, input$tissue1, g1(), g2(), input$corrType,
                        input$cex)
      primplot
    }
  })
  
  output$secondPlot <- renderPlot({
    if(input$selectMode == "Compare 2 genes correlation in two tissues" 
       & g1()!="" & g2()!="")
    {
      par(mar=c(4.8,5.1,4.8,2.1))
      pathTis2<-calcPath(filePath,input$selectDataType,input$tissue2)
      secplot<-outPlot(pathTis2, input$tissue2, g1(), g2(), input$corrType,
                       input$cex)
      secplot
    }
  })
  
  output$topCoexTis <- renderDataTable({
    if(input$selectMode == "Search the main coexpressor genes for a gene in a tissue")
    {
      pathTis1<-calcPath(filePath,input$selectDataType,input$tissue1)
      DTTF<<-calcCorr(input$corrType,input$typeOfGenes,g1(),pathTis1, input$tissue1)
      DTTF
    }
  },rownames = FALSE)
  
  output$plotRow <- renderPlot({
    if(input$selectMode == "Search the main coexpressor genes for a gene in a tissue" & g1()!="")
    {
      par(mar=c(4.8,5.1,4.8,2.1))
      req(input$topCoexTis_rows_selected)
      sGene <- input$topCoexTis_rows_selected
      sndGen <- as.character(DTTF[sGene,"Gene2"])
      pathTis1<-calcPath(filePath,input$selectDataType,input$tissue1)
      terPlot <- outPlot(pathTis1, input$tissue1, g1(), sndGen, input$corrType,
                         input$cex)
      terPlot
    }
  })
  
  output$DEGTable <- renderDataTable({
    if(input$selectMode == "Search the main DEGs for the couples healthy/tumor")
    {
      
      message(paste0(Sys.time()," searching DEGs about ",g1()," in ",input$tissue1," and ",input$tissue2))
      pathTis1<-calcPath(filePath,input$selectDataType,input$tissue1)
      pathTis2<-calcPath(filePath,input$selectDataType,input$tissue2)
      
      DTTF1<<-calcCorr(input$corrType,input$typeOfGenes,g1(),pathTis1, input$tissue1)
      DTTF2<<-calcCorr(input$corrType,input$typeOfGenes,g1(),pathTis2, input$tissue2)

      DTTFfinal<<-rbind(DTTF1,DTTF2)
      DTTFfinal<<-DTTFfinal[order(DTTFfinal$Gene2),]
      DTTFfinal<<-DEGsTab(DTTFfinal)
      DTTFfinal<<-DTTFfinal[rowSums(is.na(DTTFfinal)) != ncol(DTTFfinal),]
      DTTFfinal<<-DTTFfinal[!(is.na(DTTFfinal$DeltaCoeX)),]
      DTTFfinal<<-DTTFfinal[order(DTTFfinal$DeltaCoeX, decreasing = TRUE),]
      
      DTTFfinal
    }
  },rownames = FALSE)
  
  output$plotRowDEGT1 <- renderPlot({
    if(input$selectMode == "Search the main DEGs for the couples healthy/tumor" & g1()!="")
    {
      par(mar=c(4.8,5.1,4.8,2.1))
      req(input$DEGTable_rows_selected)
      sGene <- input$DEGTable_rows_selected
      sndGen <- as.character(DTTFfinal[sGene,"Gene2"])
      pathTis1<-calcPath(filePath,input$selectDataType,input$tissue1)
      DEGPlot1 <- outPlot(pathTis1, input$tissue1, g1(), sndGen, input$corrType,
                          input$cex)
      DEGPlot1
    }
  })
  
  output$plotRowDEGT2 <- renderPlot({
    if(input$selectMode == "Search the main DEGs for the couples healthy/tumor" & g1()!="")
    {
      par(mar=c(4.8,5.1,4.8,2.1))
      req(input$DEGTable_rows_selected)
      sGene <- input$DEGTable_rows_selected
      sndGen <- as.character(DTTFfinal[sGene,"Gene2"])
      pathTis2<-calcPath(filePath,input$selectDataType,input$tissue2)
      DEGPlot2 <- outPlot(pathTis2, input$tissue2, g1(), sndGen, input$corrType,
                          input$cex)
      DEGPlot2
    }
  })
  
  plotModal <- function() {
    modalDialog(
      title = "TCGA CODEs",
      dataTableOutput("tableSigle"),
      easyClose = TRUE
    )
  }
  
  observeEvent(input$showTable, {
    showModal(plotModal())
  })
  
    output$tableSigle <- renderDataTable(
    dataframe1
  )
  
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste(input$tissue1,"_",g1(),".csv", sep = "")
    },
    content = function(file) {
      write.csv(DTTF, file, row.names = FALSE)
    }
  )
  
  output$downloadPDF<- downloadHandler(
    filename = function() {
      paste(input$tissue1,"_",g1(),"_",g2(),".pdf", sep = "")
    },
    content = function(file) {
      
      if(input$selectMode == "Show 2 genes correlation in a tissue" &
         g1()!="" & g2()!=""){
        pdf(file)
          pathTis1<-calcPath(filePath,input$selectDataType,input$tissue1)
          print(outPlot(pathTis1, input$tissue1, g1(), g2(), input$corrType,
                        input$cex))
        dev.off()
      }
      else if(input$selectMode == "Compare 2 genes correlation in two tissues" &
              g1()!="" & g2()!=""){
        pdf(file)
          pathTis1<-calcPath(filePath,input$selectDataType,input$tissue1)
          print(outPlot(pathTis1, input$tissue1, g1(), g2(), input$corrType,
                        input$cex))
          pathTis2<-calcPath(filePath,input$selectDataType,input$tissue2)
          print(outPlot(pathTis2, input$tissue2, g1(), g2(), input$corrType,
                        input$cex))
        dev.off()
      }
      
    }
  )
  
  output$downloadAllPlotsG<- downloadHandler(
    filename = function() {
      paste0("coexp_",g1(),"_",g2(),"_gtex_expmat.png")
    },
    content = function(file) {
      png(file,w=6000,h=5000,p=60)
      par(mfrow=c(5,6))
      for(tis in allTissue) {
        if(grepl("gtex",tis))
        {
          pathTis1<-calcPath(filePath,input$selectDataType,tis)
          outPlot(pathTis1, tis, g1(), g2(), input$corrType,
                      0.6)
        }
      }
      dev.off()
    }
  )
  
  output$downloadAllPlotsT<- downloadHandler(
    filename = function() {
      paste0("coexp_",g1(),"_",g2(),"_tcga_expmat.png")
    },
    content = function(file) {
      png(file,w=6000,h=5000,p=60)
      par(mfrow=c(5,7))
      for(tis in allTissue) {
        if(grepl("tcga",tis))
        {
          pathTis1<-calcPath(filePath,input$selectDataType,tis)
          outPlot(pathTis1, tis, g1(), g2(), input$corrType,
                  0.6)
        }
      }
      dev.off()
    }
  )
  
  output$downloadAllPlotsN<- downloadHandler(
    filename = function() {
      paste0("coexp_",g1(),"_",g2(),"_nbl_expmat.png")
    },
    content = function(file) {
      png(file,w=3000,h=3000,p=80)
      par(mfrow=c(2,2))
      for(tis in allTissue) {
        if(grepl("NBL",tis))
        {
          pathTis1<-calcPath(filePath,input$selectDataType,tis)
          outPlot(pathTis1, tis, g1(), g2(), input$corrType,
                  0.6)
        }
      }
      dev.off()
    }
  )
})