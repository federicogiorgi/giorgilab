### Loading packages and defining functions
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

ui<-dashboardPage(
    skin="black",
    dashboardHeader(
        title="COVID-19 genome annotator",
        titleWidth=300
    ),
    dashboardSidebar(
        width=300,
        actionButton(inputId='ab1',label="Switch to Global Analysis",icon=icon("globe"),onclick ="location.href='/coronapp';"),
        HTML('
        <p style="margin-left:15px;
        margin-right:15px;
        margin-top:10px;
        text-align:justify;
        font-size:14px;
        color:white;
        ">
        <i>coronapp</i> is a web application written in Shiny with a double purpose:
        <ul style="padding:10; margin:0;">
        <li>Monitor SARS-CoV-2 worldwide mutations</li>
        <li>Annotate user-provided mutations</li>
        </ul>
        </p>
        <p style="margin-left:15px;
        margin-right:15px;
        margin-top:10px;
        text-align:justify;
        font-size:14px;
        color:white;
        ">
        A full descripion of the app is available on <a href=https://www.biorxiv.org/content/10.1101/2020.05.31.124966v1>our preprint manuscript</a>.
        <br>
        <p style="margin-left:15px;
        margin-right:15px;
        margin-top:10px;
        margin-bottom:-30px;
        text-align:justify;
        font-size:16px;
        color:white;
        font-weight:bold;
        ">
        Provide your own (multi)FASTA file
        </p>
        '),
        fileInput("fasta","",
                  accept=c(".fasta",".fa",".fas","text/plain")
        ),
        HTML('
        <p style="margin-left:15px;
        margin-right:15px;
        margin-top:-20px;
        text-align:justify;
        font-size:14px;
        color:white;">
        
        <a style= "font-size:16px;" href="example.fasta">Download example input multiFASTA</a>
        <br>
        <br>
        The FASTA annotator will discover and annotate every mutation present in the uploaded SARS-CoV-2 genomic sequences, even partial.
        The GFF3 genome annotation file is available <a href="NC_045512.2_annot.gff3">here</a>
        </p>
        
        <p style="margin-left:15px;
        margin-right:15px;
        margin-top:10px;
        text-align:center;
        font-size:14px;
        color:white;">
        <img src="logowhite.png" width=160>
        </p>
        ')
    ),
    dashboardBody(
        shinyjs::useShinyjs(),
        tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
        tags$head(
            tags$link(rel="stylesheet",type="text/css",href="style.css")
        ),
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        shinyjs::hidden(
            wellPanel(
                id="userfasta",
                textOutput("nseq"),
                textOutput("nloci"),
                textOutput("nevents"),
                plotOutput("plot01",click="plot_click",width="95%",height="700px") %>% withSpinner(color="black"),
                hr(),
                fluidRow(
                    downloadButton(outputId="downloadCSV",
                                   label="Download Full table (CSV format)",class="bbutton")
                ),
                br(),
                fluidRow(
                    DT::dataTableOutput("contents")
                ),
                hr(),
                fluidRow(
                    column(2,
                           uiOutput("uiproteins")
                    ),
                    column(2,
                           radioButtons("gvisradio", "Data:",
                                        c("Original"="orig",
                                          "Log10"="log10",
                                          "Percentage" = "perc"
                                        ))
                    )
                ),
                fluidRow(
                    htmlOutput("googlevis")
                )
            )
        ),
        HTML('
        <footer id="colophon" role="contentinfo" align="center">
        <div class="site-info">
        <p>App developed by Federico M. Giorgi, Eleonora Fornasari, Daniele Mercatelli & Luca Triboli</p>
        <p><a href = "mailto: federico.giorgi@unibo.it">Write us</a> if you have questions and suggestions suggestions</p>
        <h5><a href="/"><img class="itsus" src="itsus.png" alt="It is us - Coronavirus Genome Annotator" width=100></a></h5>
        <br>
        </div>
        </footer>
        '
        )
        
    )
)

server <- function(input,output,server){
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
    
    # Show/hide blocks
    shinyjs::hideElement("userfasta",anim = TRUE,animType = "slide")
    observeEvent(input$fasta,{
        if(is.null(input$fasta)){
            shinyjs::hideElement("userfasta",anim = TRUE,animType = "slide")
        }else{
            shinyjs::showElement("userfasta",anim = TRUE,animType = "slide")
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
        toshow<-results
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
    output$contents <- renderDataTable({
        if (!(is.null(input$fasta))){
            return(results)
        }
    },rownames=FALSE)
    output$downloadCSV <- downloadHandler(
        filename = function() {
            "myresults.csv"
        },
        content = function(file) {
            write.csv(results, file, row.names = FALSE)
        }
    )
    ### Lists for selects
    output$uiproteins<-renderUI({
        selectizeInput("uiprotein","Select Protein: ",
                       choices=as.list(names(niceproteins)),
                       selected="S")
    })
    ### Visualize mutations over protein length
    output$googlevis <- googleVis::renderGvis({
        ### Parameters
        protein<-input$uiprotein
        transform<-input$gvisradio
        
        
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
        if(transform=="log10"){
            occ<-log10(occ+0.1)
            maxValue<-max(occ)+1
            ylab<-"Occurrence of event (Log10)"
            
        }
        if(transform=="perc"){
            occ<-as.numeric(round(100*occ/length(headers),3))
            ylab<-"Occurrence of event (%)"
            maxValue<-100
        }
        # Variant class
        vc<-setNames(tomap$varclass,tomap$variant)
        vc<-vc[unique(names(vc))]
        
        # Distinguish silent from aa-changing
        status<-vc[labs]
        status[status%in%c("SNP","SNP_stop",
                           "insertion","insertion_frameshift","insertion_stop",
                           "deletion","deletion_frameshift","deletion_stop")]<-"aa change"
        status[status%in%c("SNP_silent","extragenic")]<-"silent"
        
        # Set up object for plot
        df<-data.frame(
            aa=occloc,
            occurrence=occ,
            effect=labs,
            status=status,
            stringsAsFactors=FALSE
        )
        
        
        # Google vis
        Sys.sleep(0.3) # bug fix for googleVis not visualizing
        gvisBubbleChart(
            df,idvar="effect",xvar="aa",yvar="occurrence",colorvar="status",
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
}

shinyApp(ui,server)
