packages <- c("Biostrings","seqinr","shiny","stringi","data.table","googleVis")
sapply(packages, function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
source("annotator.R")



function(input, output) {
  message(paste0("Server started at ",Sys.time()))
  options(shiny.maxRequestSize=200*1024^2) # Upload limit to 200MB
  
  # Random string for temp files
  rand<-stringi::stri_rand_strings(1,20)
  
  # Global variable declaration
  results<-c()
  headers<-c()
  
  # Reference genome TODO: input by user
  genomefasta<-"data/NC_045512.2.fa"
  genomegff3<-"data/NC_045512.2_annot.gff3"
  gff3<-read.delim(genomegff3,as.is=TRUE,skip=2,header=FALSE)
  niceproteins<-setNames(gff3[,10],gff3[,9])
  
  
  ### Current data
  load("data/metadata.rda") # Sample metadata
  load("data/results.rda") # Annotated mutations
  load("data/headers.rda") # All sequences tested (including those without mutations)
  
  
  
  
  # Show/hide blocks
  shinyjs::hideElement("userfasta",anim = TRUE,animType = "slide")
  observeEvent(input$fasta,{
    if(is.null(input$fasta)){
      shinyjs::hideElement("userfasta",anim = TRUE,animType = "slide")
    }else{
      shinyjs::showElement("userfasta",anim = TRUE,animType = "slide")
    }
  })
  
  shinyjs::showElement("worldwide",anim = TRUE,animType = "slide")
  observeEvent(input$fasta,{
    if(is.null(input$fasta)){
      shinyjs::showElement("worldwide",anim = TRUE,animType = "slide")
    }else{
      shinyjs::hideElement("worldwide",anim = TRUE,animType = "slide")
    }
  })
  
  
  
  ### Functions ----
  # Function for UNIX preprocessing
  unix01<-function(path){
    nseq<-system(paste0("grep '>' ",path," | wc -l"),intern=TRUE)
    return(paste0("Number of sequences in the input: ",nseq))
  }
  
  # Function for NUCMER processing
  unix02<-function(path){
    prefix<-paste0("tmp/nuc_",rand)
    nucmer.delta<-paste0("tmp/nuc_",rand,".delta")
    nucmer.coords<-paste0("tmp/nuc_",rand,".coords")
    nucmer.snps<-paste0("tmp/nuc_",rand,".snps")
    nucmer.headers<-paste0("tmp/nuc_",rand,".headers")
    system(paste0('grep ">" ',path,' | sed "s/>//" > ',nucmer.headers))
    system(paste0("dos2unix ",path))
    system(paste0("nucmer --forward -p ",prefix," ",genomefasta," ",path))
    system(paste0("show-coords -r -c -l ",nucmer.delta," > ",nucmer.coords))
    system(paste0("show-snps ",nucmer.delta," -T -l > ",nucmer.snps))
    headers<<-read.delim(nucmer.headers,as.is=TRUE,header=FALSE)[,1]
    
    #message(getwd())
    #message(system("ls tmp"))
    system(paste0("rm ",nucmer.coords," ",nucmer.delta))
    message(system("ls tmp"))
    nevents<-system(paste0("wc -l ",nucmer.snps),intern=TRUE)
    nevents<-strsplit(nevents," ")[[1]][1]
    nevents<-as.numeric(nevents)-4
    phrase02<-paste0("Number of mutated loci in the input: ",nevents)
    return(phrase02)
  }
  
  # R part: annotation
  r01<-function(){
    nucmerfile<-paste0("tmp/nuc_",rand,".snps")
    results<-annotator(nucmerfile,genomefasta,genomegff3)
    unlink(nucmerfile)
    #save(results,file="tmp/myresults.rda")
    results<<-results
    return(paste0("Number of mutations in the input (after merging neighboring loci): ",nrow(results)))
  }
  
  # R part: plotting
  r02<-function(){
    par(mfrow=c(2,3))
    
    # Most mutated samples
    occ<-sort(table(results$sample),dec=TRUE)[1:10]
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of mutations",main="Most mutated samples",col=heat.colors(length(occ)))
    
    # Mutations per sample
    occ<-table(table(results$sample))
    par(las=2,mar=c(5,5,5,1))
    barplot(occ,xlab="nr of mutations",main="Overall mutations per sample",col="cornflowerblue")
    
    # Variant classes
    occ<-sort(table(results$varclass),dec=TRUE)
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of events",main="Most frequent events per class",col=heat.colors(length(occ)))
    
    # Variant class (A/T, etc)
    occ<-sort(table(apply(results[,c("refvar","qvar")],1,paste0,collapse=">")),dec=TRUE)[1:10]
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of samples",main="Most frequent events per type",col=heat.colors(length(occ)))
    
    # Nucleotide events
    occ<-sort(table(apply(results[,c("refvar","refpos","qvar")],1,paste0,collapse="")),dec=TRUE)[1:10]
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of samples",main="Most frequent events (nucleotide)",col=heat.colors(length(occ)))
    
    # Protein events
    occ<-sort(table(apply(results[,c("protein","variant")],1,paste0,collapse=":")),dec=TRUE)[1:10]
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of samples",main="Most frequent events (protein)",col=terrain.colors(length(occ)))
  }
  
  ### Rendering blocks ----
  output$nseq<-renderText({
    # Input file with name, size, type, datapath
    if (!(is.null(input$fasta))){
      inputFasta<-input$fasta
      path<-inputFasta$datapath
      unix01(path)
    }
  })
  
  output$nloci<-renderText({
    if (!(is.null(input$fasta))){
      inputFasta<-input$fasta
      path<-inputFasta$datapath
      unix02(path)
    }
  })
  
  output$nevents<-renderText({
    if (!(is.null(input$fasta))){
      r01() # preprocessing + merge neighboring events + annotation
    }
  })
  
  output$plot01<-renderPlot({
    # Input file with name, size, type, datapath
    if (!(is.null(input$fasta))){
      r02() # basic plotting
    }
  })
  
  output$wwplot01<-renderPlot({
    r02() # basic plotting
  })
  
  
  output$wwnseq<-renderText({
    paste0("Number of samples: ",length(headers))
  })
  output$wwnloci<-renderText({
    paste0("Number of distinct mutated loci: ",length(unique(results$variant)))
  })
  output$wwnevents<-renderText({
    paste0("Total number of mutational events: ",nrow(results))
  })

  
  
  output$contents <- renderDataTable({
    if (!(is.null(input$fasta))){
      return(results)
    }
  })
  
  output$wwcontents <- renderDataTable({
    countries<-setNames(metadata$country,metadata$gisaid_epi_isl)
    toshow<-cbind(results[,1],countries[results$sample],results[,2:ncol(results)])
    colnames(toshow)[1:2]<-c("sample","country")
    return(toshow)
  })
  
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("results.csv", sep = "")
    },
    content = function(file) {
      write.csv(results, file, row.names = FALSE)
    }
  )
  
  output$wwdownloadCSV<-downloadHandler(
    filename=function(){
      return("results.csv")
    },
    content=function(file){
      file.copy("data/results.csv",file)
    }
  )
  
  
  ### Lists for selects
  output$uiproteins<-renderUI({
    selectizeInput("protein","Select Protein: ",
                   choices=as.list(names(niceproteins)),
                   selected="S")
  })
  output$wwuiproteins<-renderUI({
    selectizeInput("protein","Select Protein: ",
                   choices=as.list(names(niceproteins)),
                   selected="S")
  })
  output$uicountries<-renderUI({
    selectizeInput("country","Select Country: ",
                   choices=as.list(unique(c("World",metadata$country))),
                   selected="World")
  })
  
  
  ### Visualize mutations over protein length
  output$googlevis <- renderGvis({
    ### Parameters
    protein<-input$protein
    log10<-input$log10
    percentage<-input$percentage
    
    ### Process parameters
    # Select Protein
    niceprotein<-niceproteins[protein]
    tomap<-results[results$protein==protein,]
    csamples<-headers
    # Further processing
    occ<-table(tomap$variant)
    labs<-names(occ)
    occ<-as.numeric(occ)
    occloc<-gsub("\\D|\\*","",labs)
    coords<-as.numeric(gff3[gff3[,9]==protein,4:5])
    plen<-(coords[2]-coords[1]+1)/3
    ylab<-"Occurrence of event"
    maxValue<-max(occ)+1
    if(log10){
      occ<-log10(occ+0.1)
      maxValue<-max(occ)+1
      ylab<-"Occurrence of event (Log10)"
    }else if(percentage){
      occ<-as.numeric(round(100*occ/length(csamples),3))
      ylab<-"Occurrence of event (%)"
      maxValue<-100
    }
    
    # Set up object for plot
    df<-data.frame(
      aa=occloc,
      occurrence=occ,
      effect=labs,
      stringsAsFactors=FALSE
    )
    
    # Google vis
    gvisBubbleChart(df,idvar="effect",xvar="aa",yvar="occurrence",
                    options=list(
                      title=paste0("Mutation frequency for protein ",protein," (",niceprotein,") in user-provided dataset"),
                      hAxis=paste0('{viewWindowMode: "maximized", title: "aa coordinate", minValue:0, maxValue: ',plen,'}'),
                      vAxis=paste0('{viewWindowMode: "maximized", title: "',ylab,'", minValue:0, maxValue: ',maxValue,'}')   ,
                      bubble='{textStyle: {fontSize: 11, color: "black", bold: true}}',
                      sizeAxis='{maxSize: 5, maxValue: 100}',
                      height=600
                    )
    )
  })
  
  output$wwgooglevis <- renderGvis({
    ### Parameters
    protein<-input$protein
    log10<-input$wwlog10
    percentage<-input$wwpercentage
    country<-input$country
    
    ### Process parameters
    # Select Protein
    niceprotein<-niceproteins[protein]
    tomap<-results[results$protein==protein,]
    # Select Country
    csamples<-union(headers,metadata$gisaid_epi_isl)
    if(country!="World"){
      csamples<-metadata$gisaid_epi_isl[metadata$country==country]
      tomap<-tomap[tomap$sample%in%csamples,]
    }
    # Further processing
    occ<-table(tomap$variant)
    labs<-names(occ)
    occ<-as.numeric(occ)
    occloc<-gsub("\\D|\\*","",labs)
    coords<-as.numeric(gff3[gff3[,9]==protein,4:5])
    plen<-(coords[2]-coords[1]+1)/3
    ylab<-"Occurrence of event"
    maxValue<-max(occ)+1
    if(log10){
      occ<-log10(occ+0.1)
      maxValue<-max(occ)+1
      ylab<-"Occurrence of event (Log10)"
    }else if(percentage){
      occ<-as.numeric(round(100*occ/length(csamples),3))
      ylab<-"Occurrence of event (%)"
      maxValue<-100
    }
    
    # Set up object for plot
    df<-data.frame(
      aa=occloc,
      occurrence=occ,
      effect=labs,
      stringsAsFactors=FALSE
    )
    
    # Google vis
    gvisBubbleChart(df,idvar="effect",xvar="aa",yvar="occurrence",
                    options=list(
                      title=paste0("Mutation frequency for protein ",protein," (",niceprotein,") in ",country),
                      hAxis=paste0('{viewWindowMode: "maximized", title: "aa coordinate", minValue:0, maxValue: ',plen,'}'),
                      vAxis=paste0('{viewWindowMode: "maximized", title: "',ylab,'", minValue:0, maxValue: ',maxValue,'}')   ,
                      bubble='{textStyle: {fontSize: 11, color: "black", bold: true}}',
                      sizeAxis='{maxSize: 5, maxValue: 100}',
                      height=600
                    )
    )
  })
  
}




