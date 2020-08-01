source("/srv/shiny-server/KaMeR/data/stepsurvival.R")

allTissue = insertPath("/srv/shiny-server/DATA/")
pathGeneList<-"/srv/shiny-server/DATA/"
pvalues<-c()

#StAb <- c("LAML","ACC")
#StNa <- 

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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  genelist1 <- eventReactive(input$tissue1,{
    read.csv(paste0(pathGeneList,"geneList",input$tissue1,"-expmat.csv"),stringsAsFactors = FALSE)
  })
    
  g1 <- eventReactive(input$g1,{input$g1})
  g2 <- eventReactive(input$g2,{input$g2})
  
  inizio<-TRUE
  observeEvent(c(input$selectMode, input$tissue1),{
    x <- input$selectMode
    write.csv(c(input$g1,input$g2),file = "/srv/shiny-server/KaMeR/tmp.txt")
    #Gestione tipo di ricerca
    if (x == "Show classic survival study")
    {
      shinyjs::hide("g2",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadDataStep",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadDataStep1p",anim = TRUE, animType = "slide")
      shinyjs::hide("stepNum",anim = TRUE, animType = "slide")
      shinyjs::hide("stepKM",anim = TRUE, animType = "fade")
      shinyjs::hide("comboKM1",anim = TRUE, animType = "fade")
      shinyjs::hide("comboKM2",anim = TRUE, animType = "fade")
      shinyjs::hide("comboKM3",anim = TRUE, animType = "fade")
      shinyjs::hide("comboKM4",anim = TRUE, animType = "fade")
      shinyjs::hide("comboKMTitle",anim = TRUE, animType = "fade")
      
      shinyjs::show("classicKM",anim = TRUE, animType = "fade")
      shinyjs::show("downloadDataClas",anim = TRUE, animType = "slide")
      shinyjs::show("downloadDataClas1p",anim = TRUE, animType = "slide")

      previousGenes<-read.csv("/srv/shiny-server/KaMeR/tmp.txt")
      if(is.na(previousGenes[1,2]) & inizio==TRUE) {
        updateSelectizeInput(session = session, inputId = 'g1',
                             choices = sort(genelist1()[,]), selected = "MYC", server = TRUE)
      }
      else if(!(is.na(previousGenes[1,2])) & previousGenes[1,2]!=g1()) {
        updateSelectizeInput(session = session, inputId = 'g1', choices = sort(genelist1()[,]),
                             selected = previousGenes[1,2], server = TRUE)
      }
    }
    else if (x == "Show step survival study")
    {
      shinyjs::hide("classicKM",anim = TRUE, animType = "fade")
      shinyjs::hide("comboKM1",anim = TRUE, animType = "fade")
      shinyjs::hide("comboKM2",anim = TRUE, animType = "fade")
      shinyjs::hide("comboKM3",anim = TRUE, animType = "fade")
      shinyjs::hide("comboKM4",anim = TRUE, animType = "fade")
      shinyjs::hide("comboKMTitle",anim = TRUE, animType = "fade")
      shinyjs::hide("g2",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadDataClas",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadDataClas1p",anim = TRUE, animType = "slide")
      
      shinyjs::show("stepKM",anim = TRUE, animType = "fade")
      shinyjs::show("downloadDataStep",anim = TRUE, animType = "slide")
      shinyjs::show("downloadDataStep1p",anim = TRUE, animType = "slide")
      shinyjs::show("stepNum",anim = TRUE, animType = "slide")

      previousGenes<-read.csv("/srv/shiny-server/KaMeR/tmp.txt")
      if(is.na(previousGenes[1,2]) & inizio==TRUE) {
        updateSelectizeInput(session = session, inputId = 'g1',
                             choices = sort(genelist1()[,]), selected = "MYC", server = TRUE)
        updateSelectizeInput(session = session, inputId = 'g2',
                             choices = sort(genelist1()[,]), selected = "E2F3", server = TRUE)
      }
      else if(previousGenes[1,2]!=g1() & !(is.na(previousGenes[1,2]))) {
        updateSelectizeInput(session = session, inputId = 'g1', choices = sort(genelist1()[,]),
                             selected = previousGenes[1,2], server = TRUE)
      }
    }
    else if (x == "Show combo survival study")
    {
      shinyjs::hide("classicKM",anim = TRUE, animType = "fade")
      shinyjs::hide("stepKM",anim = TRUE, animType = "fade")
      shinyjs::hide("downloadDataClas",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadDataStep",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadDataClas1p",anim = TRUE, animType = "slide")
      shinyjs::hide("downloadDataStep1p",anim = TRUE, animType = "slide")
      shinyjs::hide("stepNum",anim = TRUE, animType = "slide")
      
      shinyjs::show("g2",anim = TRUE, animType = "slide")
      shinyjs::show("comboKMTitle",anim = TRUE, animType = "fade")
      shinyjs::show("comboKM1",anim = TRUE, animType = "fade")
      shinyjs::show("comboKM2",anim = TRUE, animType = "fade")
      shinyjs::show("comboKM3",anim = TRUE, animType = "fade")
      shinyjs::show("comboKM4",anim = TRUE, animType = "fade")
      
      previousGenes<-read.csv("/srv/shiny-server/KaMeR/tmp.txt")
      if(is.na(previousGenes[1,2]) & is.na(previousGenes[2,2])) {
        updateSelectizeInput(session = session, inputId = 'g1',
                             choices = sort(genelist1()[,]), selected = "MYC", server = TRUE)
        updateSelectizeInput(session = session, inputId = 'g2',
                             choices = sort(genelist1()[,]), selected = "E2F3", server = TRUE)
      }
      else if(previousGenes[1,2]!=g1() & previousGenes[2,2]!=g2()) {
        updateSelectizeInput(session = session, inputId = 'g1', choices = sort(genelist1()[,]),
                             selected = previousGenes[1,2], server = TRUE)
        updateSelectizeInput(session = session, inputId = 'g2', choices = sort(genelist1()[,]),
                             selected = previousGenes[2,2], server = TRUE)
      }
    }
    
    inizio<<-FALSE
  })
  
  onclick("g1", {
    updateSelectizeInput(session, "g1", selected = "")
  })
  onclick("g2", {
    updateSelectizeInput(session, "g2", selected = "")
  })

  
  output$classicKM <- renderPlot({
    if (input$selectMode == "Show classic survival study" & g1()!="")
    {
      message(paste0("classical survival plot on ",input$tissue1, " and ",g1()))
      
      surv<-get(load(paste0(pathGeneList,input$tissue1,"-survival.rda")))
      if(input$selectDataType=="VST") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-expmat.rda")))
      }
      if(input$selectDataType=="FPKM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-fpkms.rda")))
      }
      if(input$selectDataType=="TPM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-tpms.rda")))
      }
      
      common<-intersect(colnames(expSurv),rownames(surv))
      expSurva<-expSurv[,common]
      surv<-surv[common,]
      
      oritrack<-expSurva[g1(),]
      
      if( !(length(surv)<100) & !(var(oritrack)==0) ){
        p<-plotclassicsurv(oritrack,surv,title=paste0(input$tissue1," dataset"),plot=TRUE,mygene=g1(),input$cex)
        p
      }
      else
      {
        validate(
          need(length(surv)>100, "Too few samples. Select another tissue")
        )
        validate(
          need(var(oritrack)!=0, "Variance too little across samples. Select another tissue")
        )
      }
    }
  })
  
  output$stepKM <- renderPlot({
    if (input$selectMode == "Show step survival study" & g1()!="")
    {
      message(paste0("step survival plot on ",input$tissue1, " and ",g1()))
      
      surv<-get(load(paste0(pathGeneList,input$tissue1,"-survival.rda")))
      if(input$selectDataType=="VST") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-expmat.rda")))
      }
      if(input$selectDataType=="FPKM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-fpkms.rda")))
      }
      if(input$selectDataType=="TPM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-tpms.rda")))
      }
      
      common<-intersect(colnames(expSurv),rownames(surv))
      expSurva<-expSurv[,common]
      surv<-surv[common,]
      
      oritrack<-expSurva[g1(),]
      
      if( !(length(surv)<100) & !(var(oritrack)==0) ){
        p<-plotstepsurv(oritrack,surv,input$cex,ngroups=input$stepNum,ylab=paste0(g1()," expression"),title=paste0(input$tissue1," dataset"),plot=TRUE)
        p
      }
      else
      {
        validate(
          need(length(surv)>100, "Too few samples. Select another tissue")
        )
        validate(
          need(var(oritrack)!=0, "Variance too little across samples. Select another tissue")
        )
      }
    }
  })
  
  output$comboKMTitle <- renderPlot({
    if (input$selectMode == "Show combo survival study" & g1()!="" & g2()!="")
    {
      surv<-get(load(paste0(pathGeneList,input$tissue1,"-survival.rda")))
      if(input$selectDataType=="VST") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-expmat.rda")))
      }
      if(input$selectDataType=="FPKM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-fpkms.rda")))
      }
      if(input$selectDataType=="TPM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-tpms.rda")))
      }
      
      naind<-which(is.na(surv[,2]) | (surv[,1]==-Inf))
      if(length(naind)>0)
      {
        surv<-surv[-naind,]
      }
      common<-intersect(colnames(expSurv),rownames(surv))
      texpSurv<-expSurv[,common]
      surv<-surv[common,]
      class(surv)<-"Surv"
      
      par(mar=c(0,0,0,0))
      title<-paste0(input$tissue1," Dataset")
      pT<-plot(c(0,1),c(0,1),ann=FALSE,bty='n',type='n',axes=FALSE, cex=input$cex, cex.lab=input$cex,
           cex.axis=input$cex, cex.main=input$cex+0.75, cex.sub=input$cex)
      text(x=0.5,y=0.5,title,cex=3)
      text(x=0.5,y=0.2,paste0("Samples: ",nrow(surv)," - Deaths: ",sum(surv[,2])),cex=1.5)
      par(mar=c(5,4,3,1)+0.1)
      pT
    }
  })
  
  output$comboKM1 <- renderPlot({
    if (input$selectMode == "Show combo survival study" & g1()!="" & g2()!="")
    {
      message(paste0("combo survival plot on ",input$tissue1, " with genes ",g1(), " and ", g2()))
      surv<-get(load(paste0(pathGeneList,input$tissue1,"-survival.rda")))
      if(input$selectDataType=="VST") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-expmat.rda")))
      }
      if(input$selectDataType=="FPKM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-fpkms.rda")))
      }
      if(input$selectDataType=="TPM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-tpms.rda")))
      }
      
      naind<-which(is.na(surv[,2]) | (surv[,1]==-Inf))
      if(length(naind)>0)
      {
        surv<-surv[-naind,]
      }
      common<-intersect(colnames(expSurv),rownames(surv))
      texpSurv<-expSurv[,common]
      surv<-surv[common,]
      class(surv)<-"Surv"
      
      gene1track<-log2(texpSurv[g1(),])
      gene2track<-log2(texpSurv[g2(),])
      par(mar=c(4.8,5.1,4.8,2.1))
      p1<-plot(gene1track,gene2track,xlab=paste0(g1()," expression"),ylab=paste0(g2()," expression"),main="Joint Distribution",type="n", cex=input$cex, cex.lab=input$cex,
               cex.axis=input$cex, cex.main=input$cex+0.75, cex.sub=input$cex)
      gene1quant<-quantile(gene1track,probs=c(0,0.5,1))
      gene2quant<-quantile(gene2track,probs=c(0,0.5,1))
      icol<-0
      icols<-c("dodgerblue4","chartreuse4","yellow2","red3")
      track<-setNames(rep(NA,nrow(surv)),rownames(surv))
      for(i in 1:(length(gene1quant)-1)){
        for(j in 1:(length(gene2quant)-1)){
          icol<-icol+1
          rgb<-col2rgb(icols[icol])
          transpcol<-rgb(rgb[1,1]/255,rgb[2,1]/255,rgb[3,1]/255,alpha=0.5,names=NULL,maxColorValue=1)
          rect(gene1quant[i],gene2quant[j],gene1quant[i+1],gene2quant[j+1],col=transpcol)
          hits<-intersect(
            names(gene1track)[gene1track>=gene1quant[i]&gene1track<gene1quant[i+1]],
            names(gene2track)[gene2track>=gene2quant[j]&gene2track<gene2quant[j+1]]
          )
          track[hits]<-icol
        }
      }
      points(gene1track,gene2track,pch=20,col="black")
      p1
    }
  })
  
  output$comboKM2 <- renderPlot({
    if (input$selectMode == "Show combo survival study" & g1()!="" & g2()!="")
    {
      surv<-get(load(paste0(pathGeneList,input$tissue1,"-survival.rda")))
      if(input$selectDataType=="VST") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-expmat.rda")))
      }
      if(input$selectDataType=="FPKM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-fpkms.rda")))
      }
      if(input$selectDataType=="TPM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-tpms.rda")))
      }
      
      naind<-which(is.na(surv[,2]) | (surv[,1]==-Inf))
      if(length(naind)>0)
      {
        surv<-surv[-naind,]
      }
      common<-intersect(colnames(expSurv),rownames(surv))
      texpSurv<-expSurv[,common]
      surv<-surv[common,]
      class(surv)<-"Surv"
      
      gene1track<-log2(texpSurv[g1(),])
      gene2track<-log2(texpSurv[g2(),])
      
      gene1quant<-quantile(gene1track,probs=c(0,0.5,1))
      gene2quant<-quantile(gene2track,probs=c(0,0.5,1))
      icol<-0
      icols<-c("dodgerblue4","chartreuse4","yellow2","red3")
      track<-setNames(rep(NA,nrow(surv)),rownames(surv))
      for(i in 1:(length(gene1quant)-1)){
        for(j in 1:(length(gene2quant)-1)){
          icol<-icol+1
          rgb<-col2rgb(icols[icol])
          transpcol<-rgb(rgb[1,1]/255,rgb[2,1]/255,rgb[3,1]/255,alpha=0.5,names=NULL,maxColorValue=1)
          rect(gene1quant[i],gene2quant[j],gene1quant[i+1],gene2quant[j+1],col=transpcol)
          hits<-intersect(
            names(gene1track)[gene1track>=gene1quant[i]&gene1track<gene1quant[i+1]],
            names(gene2track)[gene2track>=gene2quant[j]&gene2track<gene2quant[j+1]]
          )
          track[hits]<-icol
        }
      }
      
      sfit<-survfit(surv~track)
      par(mar=c(4.8,5.1,4.8,2.1))
      p2<-plot(sfit,mark="+",conf.int=FALSE,mark.time=sfit$time[sfit$n.censor>0],
           col=icols,main="Joint Kaplan Meier",
           xlab="Time (days)",ylab="% Survival",lwd=3, cex=input$cex, cex.lab=input$cex,
           cex.axis=input$cex, cex.main=input$cex+0.75, cex.sub=input$cex
      )
      
      subtrack<-track
      subsurv<-surv[names(subtrack),]
      sdiff<-survdiff(subsurv~subtrack)
      p<-1-pchisq(sdiff$chisq,df=2)
      mtext(paste("p =",signif(p,5)," (Samples: ",nrow(subsurv)," - Deaths: ",sum(subsurv[,2]),")",sep=""),cex=input$cex)
      message(p2)
      p2
    }
  })
  
  output$comboKM3 <- renderPlot({
    if (input$selectMode == "Show combo survival study" & g1()!="" & g2()!="")
    {
      surv<-get(load(paste0(pathGeneList,input$tissue1,"-survival.rda")))
      if(input$selectDataType=="VST") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-expmat.rda")))
      }
      if(input$selectDataType=="FPKM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-fpkms.rda")))
      }
      if(input$selectDataType=="TPM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-tpms.rda")))
      }
      
      common<-intersect(colnames(expSurv),rownames(surv))
      expSurva<-expSurv[,common]
      surv<-surv[common,]
      
      oritrack<-expSurva[input$g1,]
      
      if( !(length(surv)<100) & !(var(oritrack)==0) ){
        p<-plotclassicsurv(oritrack,surv,title=paste0(g1()," survival"),plot=TRUE,mygene=g1(),input$cex)
        p
      }
    }
  })
  
  output$comboKM4 <- renderPlot({
    if (input$selectMode == "Show combo survival study" & g1()!="" & g2()!="")
    {
      surv<-get(load(paste0(pathGeneList,input$tissue1,"-survival.rda")))
      if(input$selectDataType=="VST") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-expmat.rda")))
      }
      if(input$selectDataType=="FPKM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-fpkms.rda")))
      }
      if(input$selectDataType=="TPM") {
        expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-tpms.rda")))
      }
      
      common<-intersect(colnames(expSurv),rownames(surv))
      expSurva<-expSurv[,common]
      surv<-surv[common,]
      
      oritrack<-expSurva[g2(),]
      
      if( !(length(surv)<100) & !(var(oritrack)==0) ){
        p<-plotclassicsurv(oritrack,surv,title=paste0(g2()," survival"),plot=TRUE,mygene=g2(),input$cex)
        p
      }
    }
  })
  
  plotModal <- function() {
    modalDialog(
      title = "important",
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
  
  output$downloadDataClas1p<- downloadHandler(
    filename = function() {
      paste0("survClassic_",g1(),"_",input$tissue1,"_.png")
    },
    content = function(file) {
      png(file,w=2000,h=1000,p=30)
        surv<-get(load(paste0(pathGeneList,input$tissue1,"-survival.rda")))
        if(input$selectDataType=="VST") {
          expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-expmat.rda")))
        }
        if(input$selectDataType=="FPKM") {
          expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-fpkms.rda")))
        }
        if(input$selectDataType=="TPM") {
          expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-tpms.rda")))
        }
        
        common<-intersect(colnames(expSurv),rownames(surv))
        expSurva<-expSurv[,common]
        surv<-surv[common,]
        
        oritrack<-expSurva[g1(),]
        if( !(length(surv)<100) & !(var(oritrack)==0) ){
          plotclassicsurv(oritrack,surv,title=paste0(input$tissue1," dataset"),plot=TRUE,mygene=g1(),0.6)
        }
        else {
          plot(0,lwd=3,
               main=paste0(input$tissue1," dataset"),
               xlab="days",ylab="Percent Survival",
               cex=0.6, cex.lab=0.6,
               cex.axis=0.6, cex.main=0.6+0.75, cex.sub=0.6
          )
          mtext(paste("p ="," (Samples: ",nrow(surv)," - Deaths: ",sum(surv[,2],na.rm=TRUE),")",sep=""),cex=0.65)
        }
      
      dev.off()
    }
  )
  
  output$downloadDataClas<- downloadHandler(
    filename = function() {
      paste0("survClassic_",g1(),"_.png")
    },
    content = function(file) {
      png(file,w=6000,h=6000,p=60)
      par(mfrow=c(6,6))
      for(tis1 in allTissue) {
        surv<-get(load(paste0(pathGeneList,tis1,"-survival.rda")))
        if(input$selectDataType=="VST") {
          expSurv<-get(load(paste0(pathGeneList,tis1,"-expmat.rda")))
        }
        if(input$selectDataType=="FPKM") {
          expSurv<-get(load(paste0(pathGeneList,tis1,"-fpkms.rda")))
        }
        if(input$selectDataType=="TPM") {
          expSurv<-get(load(paste0(pathGeneList,tis1,"-tpms.rda")))
        }
        
        common<-intersect(colnames(expSurv),rownames(surv))
        expSurva<-expSurv[,common]
        surv<-surv[common,]
        
        oritrack<-expSurva[g1(),]
        if( !(length(surv)<100) & !(var(oritrack)==0) ){
          plotclassicsurv(oritrack,surv,title=paste0(tis1," dataset"),plot=TRUE,mygene=g1(),0.6)
        }
        else {
          plot(0,lwd=3,
               main=paste0(tis1," dataset"),
               xlab="days",ylab="Percent Survival",
                cex=0.6, cex.lab=0.6,
               cex.axis=0.6, cex.main=0.6+0.75, cex.sub=0.6
          )
          mtext(paste("p ="," (Samples: ",nrow(surv)," - Deaths: ",sum(surv[,2],na.rm=TRUE),")",sep=""),cex=0.65)
        }
      }
      dev.off()
    }
  )
  
  output$downloadDataStep1p<- downloadHandler(
    filename = function() {
      paste0("survStep_",g1(),"_",input$tissue1,"_.png")
    },
    content = function(file) {
      png(file,w=2000,h=1000,p=30)
      
        surv<-get(load(paste0(pathGeneList,input$tissue1,"-survival.rda")))
        if(input$selectDataType=="VST") {
          expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-expmat.rda")))
        }
        if(input$selectDataType=="FPKM") {
          expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-fpkms.rda")))
        }
        if(input$selectDataType=="TPM") {
          expSurv<-get(load(paste0(pathGeneList,input$tissue1,"-tpms.rda")))
        }
        
        common<-intersect(colnames(expSurv),rownames(surv))
        expSurva<-expSurv[,common]
        surv<-surv[common,]
        
        oritrack<-expSurva[g1(),]
        if( !(length(surv)<100) & !(var(oritrack)==0) ){
          plotstepsurv(oritrack,surv,0.6,ngroups=input$stepNum,ylab=paste0(g1()," expression"),title=paste0(input$tissue1," dataset"),plot=TRUE)
        }
        else {
          layout(t(matrix(c(1,2))),width=c(1,2))
          plot(0,ylab=paste0(g1()," expression"),pch=20, cex=0.6, cex.lab=0.6,
               cex.axis=0.6, cex.main=0.6+0.75, cex.sub=0.6)
          mtext(paste0(input$tissue1," dataset"),side=3,line=-2,outer=TRUE,cex=1.5,font=2)
          survcols<-colorpanel(input$stepNum-1,"blue","gray90","red")
          plot(0,lwd=6,
               xlab="Survival time (days)",ylab="Percent Survival",
               col=survcols,main="", cex=0.6, cex.lab=0.6,
               cex.axis=0.6, cex.main=0.6+0.75, cex.sub=0.6
          )
        }
      
      dev.off()
    }
  )
  
  output$downloadDataStep<- downloadHandler(
    filename = function() {
      paste0("survStep_",g1(),"_.png")
    },
    content = function(file) {
      png(file,w=6000,h=6000,p=60)
      par(mfrow=c(6,6))
      for(tis2 in allTissue) {
        surv<-get(load(paste0(pathGeneList,tis2,"-survival.rda")))
        if(input$selectDataType=="VST") {
          expSurv<-get(load(paste0(pathGeneList,tis2,"-expmat.rda")))
        }
        if(input$selectDataType=="FPKM") {
          expSurv<-get(load(paste0(pathGeneList,tis2,"-fpkms.rda")))
        }
        if(input$selectDataType=="TPM") {
          expSurv<-get(load(paste0(pathGeneList,tis2,"-tpms.rda")))
        }
        
        common<-intersect(colnames(expSurv),rownames(surv))
        expSurva<-expSurv[,common]
        surv<-surv[common,]
        
        oritrack<-expSurva[g1(),]
        if( !(length(surv)<100) & !(var(oritrack)==0) ){
          plotstepsurv(oritrack,surv,0.6,ngroups=input$stepNum,ylab=paste0(g1()," expression"),title=paste0(tis2," dataset"),plot=TRUE)
        }
        else {
          layout(t(matrix(c(1,2))),width=c(1,2))
          plot(0,ylab=paste0(g1()," expression"),pch=20, cex=0.6, cex.lab=0.6,
               cex.axis=0.6, cex.main=0.6+0.75, cex.sub=0.6)
          mtext(paste0(tis2," dataset"),side=3,line=-2,outer=TRUE,cex=1.5,font=2)
          survcols<-colorpanel(input$stepNum-1,"blue","gray90","red")
          plot(0,lwd=6,
               xlab="Survival time (days)",ylab="Percent Survival",
               col=survcols,main="", cex=0.6, cex.lab=0.6,
               cex.axis=0.6, cex.main=0.6+0.75, cex.sub=0.6
          )
        }
      }
      dev.off()
    }
  )
  
})