packages<-c(
  "Biostrings","seqinr","dplyr",
  "shiny","shinyjs","shinythemes","shinycssloaders","shinydashboard",
  "stringi","data.table","googleVis","DT"
)
sapply(packages, function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
source("annotator.R")
kmgformat<-function (input, roundParam = 1) {
  signs <- sign(input)
  signs[signs == 1] <- ""
  signs[signs == -1] <- "-"
  absinput <- abs(input)
  output <- c()
  for (i in absinput) {
    if (i < 1000) {
      output <- c(output, i)
    }
    else if (i < 1e+06) {
      i <- round(i/1000, roundParam)
      i <- paste0(i, "K")
      output <- c(output, i)
    }
    else if (i < 1e+09) {
      i <- round(i/1e+06, roundParam)
      i <- paste0(i, "M")
      output <- c(output, i)
    }
    else if (i < 1e+12) {
      i <- round(i/1e+09, roundParam)
      i <- paste0(i, "G")
      output <- c(output, i)
    }
    else if (i < 1e+15) {
      i <- round(i/1e+12, roundParam)
      i <- paste0(i, "T")
      output <- c(output, i)
    }
    else if (i < 1e+18) {
      i <- round(i/1e+15, roundParam)
      i <- paste0(i, "P")
      output <- c(output, i)
    }
    else {
      output <- c(output, i)
    }
  }
  output <- paste0(signs, output)
  return(output)
}

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
    system(paste0("rm ",nucmer.coords," ",nucmer.delta," ",nucmer.headers))
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
    subheaders<-headers
    country<-input$country
    if(is.null(country)){country<-"World"}
    countries<-setNames(metadata$country,metadata$gisaid_epi_isl)
    toshow<-cbind(results[,1],countries[results$sample],results[,2:ncol(results)])
    colnames(toshow)[1:2]<-c("sample","country")
    if(country!="World"){
      toshow<-toshow[toshow$country==country,]
      subheaders<-names(countries)[countries[headers]==country]
      toshow<-toshow[!is.na(toshow$sample),]
      toshow$sample<-as.character(toshow$sample)
    }
    
    par(mfrow=c(2,3),cex=1.2)
    
    # Most mutated samples
    occ<-sort(table(toshow$sample),dec=TRUE)[1:10]
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of mutations",main="Most mutated samples",col=heat.colors(length(occ)))
    
    # Mutations per sample
    occ<-table(table(toshow$sample))
    zeroes<-length(setdiff(subheaders,unique(toshow$sample)))
    occ<-c(zeroes,occ)
    names(occ)[1]<-"0"
    par(las=2,mar=c(5,5,5,1))
    barplot(occ,xlab="nr of mutations",main="Overall mutations per sample",col="cornflowerblue",ylab="nr of samples",yaxt="n")
    kmg<-kmgformat(pretty(occ))
    axis(2,at=pretty(occ),labels=kmg)
    
    # Variant classes
    occ<-sort(table(toshow$varclass),dec=TRUE)[1:6]
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of events",main="Most frequent events per class",col=heat.colors(length(occ)),yaxt="n")
    kmg<-kmgformat(pretty(occ))
    axis(2,at=pretty(occ),labels=kmg)
    
    # Variant class (A/T, etc)
    occ<-sort(table(apply(toshow[,c("refvar","qvar")],1,paste0,collapse=">")),dec=TRUE)[1:10]
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of samples",main="Most frequent events per type",col=heat.colors(length(occ)),yaxt="n")
    kmg<-kmgformat(pretty(occ))
    axis(2,at=pretty(occ),labels=kmg)
    
    # Nucleotide events
    occ<-sort(table(apply(toshow[,c("refvar","refpos","qvar")],1,paste0,collapse="")),dec=TRUE)[1:10]
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of samples",main="Most frequent events (nucleotide)",col=heat.colors(length(occ)),yaxt="n")
    kmg<-kmgformat(pretty(occ))
    axis(2,at=pretty(occ),labels=kmg)
    
    # Protein events
    occ<-sort(table(apply(toshow[,c("protein","variant")],1,paste0,collapse=":")),dec=TRUE)[1:10]
    par(las=2,mar=c(8,5,5,1))
    barplot(occ,ylab="nr of samples",main="Most frequent events (protein)",col=terrain.colors(length(occ)),yaxt="n")
    kmg<-kmgformat(pretty(occ))
    axis(2,at=pretty(occ),labels=kmg)
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
    if (!(is.null(input$fasta))){
      r02()
    }
  })
  
  output$wwplot01<-renderPlot({
    r02()
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

  output$country<-renderText({
    paste0("Showing results for ",input$country)
  })
  output$mcountry<-renderText({
    paste0("Mutational overview for ",input$country)
  })
  
  
  output$contents <- renderDataTable({
    if (!(is.null(input$fasta))){
      return(results)
    }
  },rownames=FALSE)
  
  output$wwcontents <- renderDataTable({
    country<-input$country
    if(is.null(country)){country<-"World"}
    countries<-setNames(metadata$country,metadata$gisaid_epi_isl)
    toshow<-cbind(results[,1],countries[results$sample],results[,2:ncol(results)])
    colnames(toshow)[1:2]<-c("sample","country")
    if(country!="World"){
      toshow<-toshow[toshow$country==country,]
      toshow<-toshow[!is.na(toshow$sample),]
      toshow$sample<-as.character(toshow$sample)
    }
    return(toshow)
  },rownames=FALSE)
  
  
  output$downloadCSV <- downloadHandler(
    filename = function() {
      "myresults.csv"
    },
    content = function(file) {
      write.csv(results, file, row.names = FALSE)
    }
  )
  
  output$wwdownloadCSV<-downloadHandler(
    filename=function(){
      country<-input$country
      if(is.null(country)){country<-"World"}
      if(country=="World"){
        paste0("results_",country,".csv")
      } else {
        return("results.csv")
      }
    },
    content=function(file){
      country<-input$country
      if(is.null(country)){country<-"World"}
      countries<-setNames(metadata$country,metadata$gisaid_epi_isl)
      toshow<-cbind(results[,1],countries[results$sample],results[,2:ncol(results)])
      colnames(toshow)[1:2]<-c("sample","country")
      if(country!="World"){
        toshow<-toshow[toshow$country==country,]
        toshow<-toshow[!is.na(toshow$sample),]
        toshow$sample<-as.character(toshow$sample)
        write.csv(toshow, file, row.names = FALSE)
      } else {
        file.copy("data/results.csv",file)
      }
    }
  )


  
  ### Lists for selects
  output$uiproteins<-renderUI({
    selectizeInput("uiprotein","Select Protein: ",
                   choices=as.list(names(niceproteins)),
                   selected="S")
  })
  output$wwuiproteins<-renderUI({
    selectizeInput("protein","Select Protein: ",
                   choices=as.list(names(niceproteins)),
                   selected="S")
  })
  output$wwuicountries<-renderUI({
    selectizeInput("country","Select Country: ",
                   choices=as.list(unique(c("World",sort(metadata$country)))),
                   selected="World")
  })
  
  
  ### Visualize mutations over protein length
  output$googlevis <- googleVis::renderGvis({
    ### Parameters
    protein<-input$uiprotein
    log10<-input$uilog10
    percentage<-input$uipercentage
    
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
    # Variant class
    vc<-setNames(tomap$varclass,tomap$variant)
    vc<-vc[unique(names(vc))]

    # Distinguish silent from aa-changing
    # I wanna taste you but your lips are venomous poison
    status<-vc[labs]
    status[status%in%c("SNP","SNP_stop",
                       "insertion","insertion_frameshift","insertion_stop",
                       "deletion","deletion_frameshift","deletion_stop")]<-"aa change"
    status[status%in%c("SNP_silent","extragenic")]<-"silent"
    #save(df,status,file="df.rda")
    
    
    # Set up object for plot
    df<-data.frame(
      aa=occloc,
      occurrence=occ,
      effect=labs,
      status=status,
      stringsAsFactors=FALSE
    )
    
    
    # Google vis
    gvisBubbleChart(df,idvar="effect",xvar="aa",yvar="occurrence",colorvar="status",
                    options=list(
                      title=paste0("Mutation frequency for protein ",protein," (",niceprotein,") in user-provided dataset"),
                      hAxis=paste0('{viewWindowMode: "maximized", title: "aa coordinate", minValue:0, maxValue: ',plen,'}'),
                      vAxis=paste0('{viewWindowMode: "maximized", title: "',ylab,'", minValue:0, maxValue: ',maxValue,'}')   ,
                      bubble='{textStyle: {fontSize: 11, color: "black", bold: true}}',
                      sizeAxis='{maxSize: 5, maxValue: 100}',
                      colors='["red","cornflowerblue"]',
                      height=600
                    )
    )
  })
  
  output$wwgooglevis<-renderGvis({
    ### Parameters
    protein<-input$protein
    log10<-input$wwlog10
    percentage<-input$wwpercentage
    country<-input$country
    if(is.null(country)){country<-"World"}

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
    # Variant class
    vc<-setNames(tomap$varclass,tomap$variant)
    vc<-vc[unique(names(vc))]
    
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
    
    # Distinguish silent from aa-changing
    # I wanna taste you but your lips are venomous poison
    status<-vc[labs]
    status[status%in%c("SNP","SNP_stop",
                       "insertion","insertion_frameshift","insertion_stop",
                       "deletion","deletion_frameshift","deletion_stop")]<-"aa change"
    status[status%in%c("SNP_silent","extragenic")]<-"silent"
    #save(df,status,file="df.rda")
    
    # Set up object for plot
    df<-data.frame(
      aa=occloc,
      occurrence=occ,
      effect=labs,
      status=status,
      stringsAsFactors=FALSE
    )
   
    # Google vis
    gvisBubbleChart(df,idvar="effect",xvar="aa",yvar="occurrence",colorvar="status",
                    options=list(
                      title=paste0("Mutation frequency for protein ",protein," (",niceprotein,") in ",country),
                      hAxis=paste0('{viewWindowMode: "maximized", title: "aa coordinate", minValue:0, maxValue: ',plen,'}'),
                      vAxis=paste0('{viewWindowMode: "maximized", title: "',ylab,'", minValue:0, maxValue: ',maxValue,'}')   ,
                      bubble='{textStyle: {fontSize: 11, color: "black", bold: true}}',
                      sizeAxis='{maxSize: 5, maxValue: 100}',
                      colors='["red","cornflowerblue"]',
                      height=600
                    )
    )
  })
  # Time stuff
  output$timecountries<-renderUI({
    selectizeInput("timecountry","Select Country: ",
                   choices=as.list(unique(c("World",sort(metadata$country)))),
                   selected="World")
  })
  
  output$timemuts<-renderUI({
    country<-input$timecountry
    if(is.null(country)){country<-"World"}
    load(paste0("data/times/times-",country,".rda"))
    selectizeInput("timemut","Select Mutation: ",
                   choices=as.list(rownames(times)),
                   selected="S:D614G")
  })
  
  output$timeplot<-renderPlot({
    country<-input$timecountry
    mut<-input$timemut
    if(is.null(country)){country<-"World"}
    load(paste0("data/times/times-",country,".rda"))
    
    # Start plotting
    par(las=2,mar=c(8,5,5,1),mfrow=c(1,2))
    
    # Nr of mut
    track<-times[mut,]
    par(cex=1.3)
    plot(track,xaxt="n",xlab="",ylab="nr of mutations detected",main=paste0(mut," abundance in ",country),pch=20)
    axis(1,at=1:length(track),labels=names(track))
    grid()
    
    # Percentage of mut
    perctrack<-track/times["nrsamples",]*100
    plot(perctrack,xaxt="n",xlab="",ylab="% of samples sequenced",main=paste0(mut," frequency in ",country),pch=20)
    axis(1,at=1:length(perctrack),labels=names(perctrack))
    grid()
  })
  
  
}




