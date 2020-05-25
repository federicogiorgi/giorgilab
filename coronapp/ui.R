packages<-c("shiny","shinyjs","shinythemes","shinydashboard","DT")
sapply(packages, function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})

refgenomes<-c("SARS-CoV-2 (NC_045512.2)"="NC_045512.2","Parvo B19 (NC_000883.2)"="NC_000883.2")

shinyUI(
  shinydashboard::dashboardPage(skin="black",
                                shinydashboard::dashboardHeader(
                                  title="COVID-19 genome annotator",
                                  titleWidth=300
                                ),
                                shinydashboard::dashboardSidebar(
                                  # Enter input fasta to annotate
                                  fileInput("fasta","Choose FASTA File",
                                            accept=c(".fasta",".fa",".fas","text/plain")
                                  ),
                                  # Select reference genome from list refgenomes
                                  # selectInput("refgenome","Select reference genome:",refgenomes,selected="NC_045512.2"),
                                  # Tool description
                                  HTML('
                                  <p style="margin-left:15px;
                                  margin-right:15px;
                                  margin-top:10px;
                                  text-align:justify;
                                  font-size:14px;
                                  color:white;">
                                  
                                  <a href="example.fasta">Example input multiFASTA</a>
                                  <br>
                                  This app will discover and annotate every mutation present in the uploaded SARS-CoV-2 genomic sequences, even partial.
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
                                  tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
                                  tags$head(
                                    tags$link(rel="stylesheet",type="text/css",href="style.css")
                                  ),
                                  fluidRow(
                                    textOutput("nseq"),
                                    textOutput("nloci"),
                                    textOutput("nevents"),
                                    plotOutput("plot01",click = "plot_click")
                                  ),
                                  fluidRow(
                                    shinyjs::hidden(
                                      wellPanel(id="panDow",
                                                # Download Table Button
                                                downloadButton(outputId="downloadCSV",label="Download Full table (CSV format)",class="bottone"),
                                      )
                                    )
                                  ),
                                  fluidRow(
                                    DT::dataTableOutput("contents")
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


