packages <- c("shiny","ggplot2","DT","ggvis","dplyr","shinyjs","shinyBS",
              "shinydashboard","shinyjqui","parallel","gridExtra","shinybusy")
sapply(packages, function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})

cotfArray<-read.delim("/srv/shiny-server/Tucana/data/cotfgenes_2018_08_06.txt",as.is=TRUE,sep="\t",header=FALSE)
signalingArray<-read.delim("/srv/shiny-server/Tucana/data/signaling_2018_08_06.txt",as.is=TRUE,sep="\t",header=FALSE)
tfArray<-read.delim("/srv/shiny-server/Tucana/data/tfgenes_2018_08_06.txt",as.is=TRUE,sep="\t",header=FALSE)
surfArray<-read.delim("/srv/shiny-server/Tucana/data/surfProt.txt",as.is=TRUE,sep="\t",header=FALSE)
cotfArray<-cotfArray[,-1]
signalingArray<-signalingArray[,-1]
tfArray<-tfArray[,-1]
DomRec <- read.csv(file = "/srv/shiny-server/Tucana/data/Census_allFri_Feb_1_14_27_40_2019.csv")
finalDTTF<-data.frame()
pathGeneList<-"/srv/shiny-server/DATA/"



### r2p function
r2p<-function(r,n){
  t<-(r*sqrt(n-2)) / sqrt(1-(r^2))
  p<-2*pt(t,df=n-2,lower=FALSE)
  return(p)
}

### calcPath function
calcPath<-function(partPath, dataType, tisNum){
  message(paste0(Sys.time()," calculating path"))
  if(dataType == "VST")
  {
    pathTis = paste(partPath,tisNum,"-expmat.rda",sep='',collapse='')
  }
  else if(dataType == "FPKM")
  {
    pathTis = paste(partPath,tisNum,"-fpkms.rda",sep='',collapse='')
  }
  else if(dataType == "TPM")
  {
    pathTis = paste(partPath,tisNum,"-tpms.rda",sep='',collapse='')
  }
  return(pathTis)
}

### calcCorr function
calcCorr<-function(corrType,typeOfGenes, gene1, pathTis, inpTis){
  sTis <- get(load(pathTis))
  scccorArray <- list()
  sccpValueArray <- list()
  
  if(gene1 %in% rownames(sTis))
  {
    message(paste0(Sys.time()," Calculating correlations"))
    vec<-sTis[gene1, ]
    genelist <- read.csv(paste0(pathGeneList,"geneList",inpTis,"-expmat.csv"),stringsAsFactors = FALSE)
    genelisttfArray <- genelist[genelist[,] %in% tfArray,]
    genelistcotfArray <- genelist[genelist[,] %in% cotfArray,]
    genelistsignalingArray <- genelist[genelist[,] %in% signalingArray,]
    genelistsurfArray <- genelist[genelist[,] %in% surfArray[,],]
    
    index <- 1
    for (z in rownames(sTis)) {
      if(corrType == "Pearson") {
        suppressWarnings(scccorZ <- signif(cor(vec,sTis[z,],method = "p"), 3))
        suppressWarnings(scc<-cor.test(vec,sTis[z,],method="p"))
      }
      else if(corrType == "Spearman") {
        suppressWarnings(scccorZ <- signif(cor(vec,sTis[z,],method = "s"), 3))
        suppressWarnings(scc<-cor.test(vec,sTis[z,],method="s"))
      }
      sccp<-signif(scc$p.value,3)
      sccpValueArray[index] <- sccp
      scccorArray[index] <- scccorZ
      setNames(scccorArray[index], z)
      setNames(sccpValueArray[index], z)
      index<-index+1
    }
    DTTF<<-cbind(rep(gene1,nrow(sTis)),rownames(sTis),as.numeric(scccorArray),as.numeric(sccpValueArray))
    colnames(DTTF)<<-c("Gene1", "Gene2", "Correlation","pValue")
    DTTF=as.data.frame(DTTF)

    
    DTTF$Correlation <- as.numeric(as.character(DTTF$Correlation))
    DTTF$pValue <- as.numeric(as.character(DTTF$pValue))
    if(typeOfGenes == "All"){
      finalDTTF<<-subset(DTTF,DTTF$Gene2 %in% genelist[,])
    }
    else if(typeOfGenes == "Transcription Factors"){
      finalDTTF<<-subset(DTTF,DTTF$Gene2 %in% genelisttfArray)
    }
    else if(typeOfGenes == "Co-Transcription factors"){
      finalDTTF<<-subset(DTTF,DTTF$Gene2 %in% genelistcotfArray)
    }
    else if(typeOfGenes == "Signaling proteins"){
      finalDTTF<<-subset(DTTF,DTTF$Gene2 %in% genelistsignalingArray)
    }
    else if(typeOfGenes == "Surface proteins"){
      finalDTTF<<-subset(DTTF,DTTF$Gene2 %in% genelistsurfArray)
    }
    finalDTTF$GeneType<<-rep("",nrow(finalDTTF))
    finalDTTF$CancerGeneStatus<<-rep("",nrow(finalDTTF))
    
    boollistTF<-finalDTTF$Gene2%in%genelisttfArray
    boollistCOTF<-finalDTTF$Gene2%in%genelistcotfArray
    boollistSIGN<-finalDTTF$Gene2%in%genelistsignalingArray
    boollistSURF<-finalDTTF$Gene2%in%genelistsurfArray
    boollistDom<-finalDTTF$Gene2%in%DomRec$Gene.Symbol[DomRec$Molecular.Genetics=="Dom"]
    boollistRec<-finalDTTF$Gene2%in%DomRec$Gene.Symbol[DomRec$Molecular.Genetics=="Rec"]
    
    finalDTTF$GeneType[boollistTF]<<-"TF"
    finalDTTF$GeneType[boollistCOTF]<<-"CO-TF"
    finalDTTF$GeneType[boollistSIGN]<<-"SIGNALING"
    finalDTTF$GeneType[boollistSURF]<<-"SURFACE"
    finalDTTF$CancerGeneStatus[boollistDom]<<-"Dom"
    finalDTTF$CancerGeneStatus[boollistRec]<<-"Rec"
    finalDTTF$Correlation<<-DTTF[DTTF$Gene2 %in% finalDTTF$Gene2,"Correlation"]
    finalDTTF$pValue<<-DTTF[DTTF$Gene2 %in% finalDTTF$Gene2,"pValue"]
    
    message(paste0(Sys.time()," Finished calculate correlation"))
    return(finalDTTF[order(finalDTTF$Correlation, decreasing = TRUE),])
  }
  else {
    message(paste0(Sys.time()," Errors during correlation calc in tables, retry ;)"))
    validate(
      need(gene1 %in% rownames(sTis),
           "Incorrect genes for the tissue. Select another gene or another tissue")
    )
  }
}

### insertPath function
insertPath<-function(path){
  tissues <- list()
  indexTis<-1
  for(el in dir(path)) {
    if(grepl("expmat", el) & !(grepl("geneList", el)))
    {
      fileZ <- gsub("-expmat.rda","",el)
      tissues[[indexTis]] <- fileZ
      indexTis<-indexTis+1
    }
  }
  allTissueFunc <- sort(unlist(tissues))
  return(allTissueFunc)
}

### outPlot function
outPlot<-function(pathTis, tis1, gen1, sndGen, corrType, optcex){
  mTis = get(load(pathTis))
  message(paste0(Sys.time()," Doing plots about ",gen1," and ",sndGen," in ",tis1))
  title<-paste0(gsub("_","",tis1)," (",ncol(mTis)," samples)")
  codeFemales=list()
  if (!(gen1 %in% rownames(mTis)) | !(sndGen %in% rownames(mTis)))
  {
    validate(
      need(gen1 %in% rownames(mTis), "Incorrect first gene for the tissue. Select another gene or another tissue")
    )
    validate(
      need(sndGen %in% rownames(mTis), "Incorrect second gene for the tissue. Select another gene or another tissue")
    )
  }
  else
  {
    plotcoso=plot(mTis[gen1, ],mTis[sndGen, ],xlab=gen1,ylab=sndGen, pch=20, col="black",
                  main=title, cex=optcex, cex.lab=optcex,
                  cex.axis=optcex, cex.main=optcex+0.75, cex.sub=optcex)
    
    if(corrType == "Pearson") {
      suppressWarnings(scccor <- signif(cor(mTis[gen1, ],mTis[sndGen, ],method = "p"), 3))
      suppressWarnings(scc <- cor.test(mTis[gen1, ],mTis[sndGen, ],method="p"))
      sccp<-signif(scc$p.value,3)
      mtext(paste0("PearsonCoefficient=",scccor," (pValue=",sccp,")"),cex=optcex)
    }
    else if(corrType == "Spearman") {
      suppressWarnings(scccor <- signif(cor(mTis[gen1, ],mTis[sndGen, ],method = "s"), 3))
      suppressWarnings(scc <- cor.test(mTis[gen1, ],mTis[sndGen, ],method="s"))
      sccp<-signif(scc$p.value,3)
      mtext(paste0("SpearmanCoefficient=",scccor," (pValue=",sccp,")"),cex=optcex)
    }
    if(scccor>=0){bgcol<-"#FF000033"}else{bgcol<-"#0000FF33"}
    if(sccp>0.01){bgcol<-"#FFFFFF00"}
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4],col=bgcol)
    grid(col="grey")
    lm1<-lm(mTis[sndGen, ]~mTis[gen1, ])
    abline(lm1$coef)
    return (plotcoso)
  }
}

### DEGsTable function
DEGsTab<-function(DFF){
  
  DFFF<-data.frame(matrix(ncol=9, nrow=0))
  colnames(DFFF)=c("Gene1", "Gene2","DeltaCoeX","pValue","GeneType","CancerGeneStatus","CoeXTissue1/CoeXTissue2","T1CoeX","T2CoeX")
  i2<-1
  nexEl = 1
  for (i in 1:(nrow(DFF)-1) )
  {
    nexEl = i+1
    if(DFF[i,"Gene2"] == DFF[nexEl,"Gene2"])
    {
      DFFF[i2,]<-DFF[i,]
      DFFF[i2,"Gene2"]<-as.character(DFF[i,"Gene2"])
      DFFF[i2,"Gene1"]<-as.character(DFF[i,"Gene1"])
      DFFF[i2,"DeltaCoeX"]<-abs(DFF[i,"Correlation"]-DFF[nexEl,"Correlation"])
      DFFF[i2,"T1CoeX"]<-DFF[i,"Correlation"]
      DFFF[i2,"T2CoeX"]<-DFF[nexEl,"Correlation"]
      if(!(is.na(DFF[i,"Correlation"])) & !(is.na(DFF[nexEl,"Correlation"])))
      {
        if(DFF[i,"Correlation"]<0 & DFF[nexEl,"Correlation"]<0)
        {
          DFFF[i2,"CoeXTissue1/CoeXTissue2"]<-"-/-"
        }
        else if(DFF[i,"Correlation"]>0 & DFF[nexEl,"Correlation"]>0)
        {
          DFFF[i2,"CoeXTissue1/CoeXTissue2"]<-"+/+"
        }
        else if(DFF[i,"Correlation"]<0 & DFF[nexEl,"Correlation"]>0)
        {
          DFFF[i2,"CoeXTissue1/CoeXTissue2"]<-"-/+"
        }
        else if(DFF[i,"Correlation"]>0 & DFF[nexEl,"Correlation"]<0)
        {
          DFFF[i2,"CoeXTissue1/CoeXTissue2"]<-"+/-"
        }
      }
      i<-i+1
    }
    i2<-i2+1
  }
  DFFF<-DFFF%>%select(1,2,3,5,6,7,8,9)
  return(DFFF)
}


