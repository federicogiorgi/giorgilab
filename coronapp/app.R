


updated<-"updated June 30, 2020"


### Loading packages and defining functions
packages<-c(
    "Biostrings","seqinr","dplyr",
    "shiny","shinyjs","shinythemes","shinycssloaders","shinydashboard",
    "stringi","data.table","googleVis","DT"
)
sapply(packages, function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
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
        actionButton(inputId='ab1',label="Switch to Annotating your FASTA",icon=icon("dna"),onclick ="location.href='/coronannotator';"),
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
        The GFF3 genome annotation file is available <a href="NC_045512.2_annot.gff3">here</a>
        </p>
        
        <p style="margin-left:15px;
        margin-right:15px;
        margin-top:10px;
        text-align:center;
        font-size:14px;
        color:white;">
        <img src="logoglobal.png" width=170>
        </p>'
        )
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
        wellPanel(
            id="worldwide",
            h1("Current Status of SARS-CoV-2 mutational data"),
            h5(updated),
            fluidRow(
                downloadButton(
                    outputId="world_downloadCSV",
                    label="Download Full World Results (CSV format)",class="bbutton")
            ),
            textOutput("wwnseq"),
            textOutput("wwnloci"),
            textOutput("wwnevents"),
            hr(),
            h3(textOutput("country")),
            fluidRow(
                column(2,
                       uiOutput("tablecountries")
                )
            ),
            fluidRow(
                downloadButton(
                    outputId="downloadCSV",
                    label="Download Visualized Results (CSV format)",class="bbutton")
            ),
            br(),
            fluidRow(
                DT::dataTableOutput("wwcontents")
            ),
            h3(textOutput("mcountry")),
            plotOutput("wwplot01",click="plot_click",width="95%",height="700px"),
            hr(),
            h2("Analysis of mutations over time"),
            fluidRow(
                column(2,
                       uiOutput("timecountries")
                ),
                column(2,
                       uiOutput("timemuts")
                )
            ),
            plotOutput("timeplot",width="95%",height="1400px"),
            hr(),
            h3("Individual Mutation Frequency"),
            fluidRow(
                column(2,
                       uiOutput("gviscountries")
                ),
                column(2,
                       uiOutput("gvisproteins")
                ),
                column(2,
                       checkboxInput("gvislog10", "Log10", TRUE),
                       checkboxInput("gvispercentage", "Percentage", FALSE)
                )
            ),
            fluidRow(
                htmlOutput("wwgooglevis") %>% withSpinner(color="black"),
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
    # Global variable declaration
    results<-c()
    headers<-c()
    # Reference genome TODO: input by user
    genomegff3<-"www/NC_045512.2_annot.gff3"
    gff3<-read.delim(genomegff3,as.is=TRUE,skip=2,header=FALSE)
    niceproteins<-setNames(gff3[,10],gff3[,9])
    
    
    ### Current data
    load("data/extras/metadata.rda") # Sample metadata
    load("data/results/results-Italy.rda") # Annotated mutations
    load("data/extras/headers.rda") # All sequences tested (including those without mutations)
    
    ### Functions ----
    # R part: plotting
    r02<-function(){
        subheaders<-headers
        country<-input$tablecountry
        load(paste0("data/results/results-",country,".rda"))
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
    output$wwplot01<-renderPlot({
        r02()
    })
    output$wwnseq<-renderText({
        paste0("Number of samples: ",length(headers))
    })
    output$wwnloci<-renderText({
        paste0("Number of distinct mutated loci: ",nrloci)
    })
    output$wwnevents<-renderText({
        paste0("Total number of mutational events: ",nrevents)
    })
    output$country<-renderText({
        paste0("Mutation Table for ",input$tablecountry)
    })
    output$mcountry<-renderText({
        paste0("Mutational overview for ",input$tablecountry)
    })
    output$wwcontents <- renderDataTable({
        country<-input$tablecountry
        if(is.null(country)){country<-"Italy"}
        load(paste0("data/results/results-",country,".rda"))
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
            country<-input$tablecountry
            return(paste0("results-",country,".csv"))
        },
        content = function(file) {
            country<-input$tablecountry
            #if(is.null(country){country<-"Italy"})
            load(paste0("data/results/results-",country,".rda"))
            #save(results,country,file="tmp.rda")
            write.csv(results,file=file,row.names=FALSE,quote=FALSE)
        }
    )
    
    output$world_downloadCSV <- downloadHandler(
        filename = function() {
            return("results-World.csv")
        },
        content = function(file) {
            file.copy("data/results/results-World.csv",file)
        }
    )
    
    
    ### Lists for selects
    output$tablecountries<-renderUI({
        selectizeInput("tablecountry",'Select Country (or "World"): ',
                       choices=as.list(unique(c("World",sort(metadata$country)))),
                       selected="Italy")
    })
    output$uiproteins<-renderUI({
        selectizeInput("uiprotein","Select Protein: ",
                       choices=as.list(names(niceproteins)),
                       selected="S")
    })
    output$gvisproteins<-renderUI({
        selectizeInput("protein","Select Protein: ",
                       choices=as.list(names(niceproteins)),
                       selected="S")
    })
    output$gviscountries<-renderUI({
        selectizeInput("country",'Select Country (or "World"): ',
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
        Sys.sleep(0.3)
        googleVis::gvisBubbleChart(df,idvar="effect",xvar="aa",yvar="occurrence",colorvar="status",
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
        log10<-input$gvislog10
        percentage<-input$gvispercentage
        country<-input$country
        if(is.null(country)){country<-"World"}
        
        ### Process parameters
        # Select Country
        load(paste0("data/results/results-",country,".rda"))
        # Select Protein
        niceprotein<-niceproteins[protein]
        tomap<-results[results$protein==protein,]
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
        googleVis::gvisBubbleChart(df,idvar="effect",xvar="aa",yvar="occurrence",colorvar="status",
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
        par(las=2,mar=c(8,5,5,1),mfrow=c(2,1))
        
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

shinyApp(ui,server)




