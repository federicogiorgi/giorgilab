packages<-c(
  "Biostrings","seqinr","dplyr",
  "shiny","shinyjs","shinythemes","shinycssloaders","shinydashboard",
  "stringi","data.table","googleVis","DT"
)
sapply(packages, function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
refgenomes<-c("SARS-CoV-2 (NC_045512.2)"="NC_045512.2","Parvo B19 (NC_000883.2)"="NC_000883.2")

shinyUI(
  dashboardPage(
    skin="black",
    dashboardHeader(
      title="COVID-19 genome annotator",
      titleWidth=300
    ),
    dashboardSidebar(
      # Enter input fasta to annotate
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
      # Select reference genome from list refgenomes
      # selectInput("refgenome","Select reference genome:",refgenomes,selected="NC_045512.2"),
      # Tool description
      HTML('
                <p style="margin-left:15px;
                margin-right:15px;
                margin-top:-20px;
                text-align:justify;
                font-size:14px;
                color:white;">
                
                <a href="example.fasta">Example input multiFASTA</a>
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
                <img src="logowhite.png" width=150>
                </p>'
      ),
      width = 300
    ),
    
    
    dashboardBody(
      #add_busy_spinner(spin = "fading-circle"),
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
                   checkboxInput("uilog10", "Log10", FALSE),
                   checkboxInput("uipercentage", "Percentage", FALSE)
            )
          ),
          fluidRow(
            htmlOutput("googlevis")
          )
          
        )
      ),
      
      wellPanel(
        id="worldwide",
        h1("Current Status of SARS-CoV-2 mutational data"),
        h5("updated May 30, 2020"),
        textOutput("wwnseq"),
        textOutput("wwnloci"),
        textOutput("wwnevents"),
        fluidRow(
          column(2,
                 uiOutput("wwuicountries")
          ),
          column(2,
                 uiOutput("wwuiproteins")
          ),
          column(2,
                 checkboxInput("wwlog10", "Log10", TRUE),
                 checkboxInput("wwpercentage", "Percentage", FALSE)
          )
        ),
        fluidRow(
          htmlOutput("wwgooglevis") %>% withSpinner(color="black")
        ),
        hr(),
        h3(textOutput("country")),
        fluidRow(
          downloadButton(outputId="wwdownloadCSV",
                         label="Download Full table (CSV format)",class="bbutton")
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
        plotOutput("timeplot",width="95%",height="700px")
      ),
      
      
      HTML('<footer id="colophon" role="contentinfo" align="center">
        			<div class="site-info">
        			   <p>App developed by Federico M. Giorgi, Eleonora Fornasari, Daniele Mercatelli & Luca Triboli</p>
            	   <h5><a href="/"><img class="itsus" src="itsus.png" alt="It is us - Coronavirus Genome Annotator" width=100></a></h5>
        			   <br>
        			</div>
        		</footer>')
    )
    
  )
)
