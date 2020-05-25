packages <- c("shiny","seqinr")
sapply(packages, function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})


function(input, output) {
  message(paste0("Server started at ",Sys.time()))
  options(shiny.maxRequestSize=200*1024^2) # Upload limit to 200MB
  
  # Global variables
  path<-NULL
  hits<-NULL
  hitnames<-NULL
  multifasta<-NULL
  nhits<-NULL
  
  
  
  # Show/hide blocks
  shinyjs::hideElement("report",anim = TRUE,animType = "slide")
  shinyjs::hideElement("report2",anim = TRUE,animType = "slide")
  observeEvent(input$fasta,{
    if(is.null(input$fasta)){
      shinyjs::hideElement("report",anim = TRUE,animType = "slide")
      shinyjs::hideElement("report2",anim = TRUE,animType = "slide")
    }else{
      shinyjs::showElement("report",anim = TRUE,animType = "slide")
      shinyjs::showElement("report2",anim = TRUE,animType = "slide")
    }
  })
  observeEvent(nhits,{
   if(nhits==0){
     shinyjs::hideElement("report2",anim = TRUE,animType = "slide")
   }
  })
  
  
  ### Functions ----
  # Function for UNIX preprocessing
  unix01<-function(){
    system(paste0("dos2unix ",path))
    nseq<-system(paste0("grep '>' ",path," | wc -l"),intern=TRUE)
    return(paste0("Number of sequences in the input: ",nseq))
  }
  
  # R function for pattern matching
  r01<-function(){
    nhits<-0
    pattern<-input$pattern
    multifasta<<-read.fasta(path,as.string= TRUE,forceDNAtolower=FALSE)
    anal<-sapply(multifasta,function(x){
      xx<-grep(pattern,x[1],perl=TRUE)
      if(length(xx)>0){
        return(TRUE)
      } else {
        return(FALSE)
      }
    })
    hits<<-names(anal)[anal]
    hitnames<<-gsub("^>","",unlist(getAnnot(multifasta)[anal]))
    nhits<-length(hits)
    return(paste0("Number of sequences matching pattern: ",nhits))
    nhits<<-nhits
  }
  
  # R function that returns hit names
  r02<-function(){
    df<-data.frame(hitnames)
    hitfasta<-multifasta[hits]
    hitlengths<-sapply(hitfasta,function(x){nchar(x[1])})
    df<-cbind(df,hitlengths)
    colnames(df)<-c("Matching sequence","Length")
    return(df)
  }
  
  
  ### Rendering blocks ----
  output$nseq<-renderText({
    # Input file with name, size, type, datapath
    if (!(is.null(input$fasta))){
      inputFasta<-input$fasta
      path<<-inputFasta$datapath
      unix01()
    }
  })
  
  output$nhits<-renderText({
    if (!(is.null(input$fasta))){
      r01()
    }
  })
  
  output$hitnames<-renderDataTable({
    if (!(is.null(input$fasta))){
      r02()
    }
  })
  
  output$download<-downloadHandler(
    filename = function() {
      paste0("hits.fasta")
    },
    content = function(file) {
      outfasta<-multifasta[hits]
      write.fasta(outfasta,names=hitnames,file.out=file,as.string=TRUE)
    }
  )
  output$downloadtxt<-downloadHandler(
    filename = function() {
      paste0("hits.txt")
    },
    content = function(file) {
      write.table(hitnames,row.names=FALSE,col.names=FALSE,quote=FALSE,file=file)
    }
  )
  
  
}




