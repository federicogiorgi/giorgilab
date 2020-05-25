packages<-c("shiny","shinyjs","shinythemes","shinydashboard","DT","googleVis")
sapply(packages, function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})

refgenomes<-c("SARS-CoV-2 (NC_045512.2)"="NC_045512.2","Parvo B19 (NC_000883.2)"="NC_000883.2")

shinyUI(
  dashboardPage(skin="black",
                shinydashboard::dashboardHeader(
                  title="COVID-19 genome annotator",
                  titleWidth=300
                ),
                shinydashboard::dashboardSidebar(
                  # Enter input fasta to annotate
                  HTML('
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
                
                
                shinydashboard::dashboardBody(
                  shinyjs::useShinyjs(),
                  #add_busy_spinner(spin = "fading-circle"),
                  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
                  tags$head(
                    tags$link(rel="stylesheet",type="text/css",href="style.css")
                  ),
                  shinyjs::hidden(
                    wellPanel(id="userfasta",
                              textOutput("nseq"),
                              textOutput("nloci"),
                              textOutput("nevents"),
                              plotOutput("plot01",click="plot_click",width="95%",height="500px"),
                              hr(),
                              fluidRow(
                                downloadButton(outputId="downloadCSV",
                                               label="Download Full table (CSV format)",class="bottone"),
                                DT::dataTableOutput("contents")
                              ),
                              hr(),
                              fluidRow(
                                column(2,
                                       uiOutput("uiproteins")
                                ),
                                column(2,
                                       checkboxInput("log10", "Log10", FALSE),
                                       checkboxInput("percentage", "Percentage", FALSE)
                                )
                              ),
                              fluidRow(
                                htmlOutput("googlevis")
                              )
                              
                    )
                  ),
                  
                  wellPanel(id="worldwide",
                            h1("Current Status of SARS-CoV-2 mutational data"),
                            h5("updated May 25, 2020"),
                            textOutput("wwnseq"),
                            textOutput("wwnloci"),
                            textOutput("wwnevents"),
                            fluidRow(
                              column(2,
                                     uiOutput("wwuiproteins")
                              ),
                              column(2,
                                     uiOutput("uicountries")
                              ),
                              column(2,
                                     checkboxInput("wwlog10", "Log10", TRUE),
                                     checkboxInput("wwpercentage", "Percentage", FALSE)
                              )
                            ),
                            fluidRow(
                              htmlOutput("wwgooglevis")
                            ),
                            hr(),
                            fluidRow(
                              downloadButton(outputId="wwdownloadCSV",
                                             label="Download Full table (CSV format)",class="bottone"),
                              DT::dataTableOutput("wwcontents")
                            ),
                            hr(),
                            plotOutput("wwplot01",click="plot_click",width="95%",height="500px")
                            
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

# add_busy_bar(color = "red", height = "8px")
# https://cran.r-project.org/web/packages/shinybusy/vignettes/shinybusy-usage.html
# https://shiny.rstudio.com/articles/progress.html


