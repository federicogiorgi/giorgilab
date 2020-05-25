packages<-c("shiny","shinyjs","shinythemes","shinydashboard","DT")
sapply(packages, function(x){suppressPackageStartupMessages(library(x,character.only=TRUE))})
shinyUI(
  dashboardPage(
    skin="black",
    dashboardHeader(
      title="Sequence Pattern Quantum findeR",
      titleWidth=400
    ),
    dashboardSidebar(
      # Enter input fasta to search
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
        Provide a FASTA file
        </p>'
      ),
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
        <a href="example.fasta">Example input multiFASTA</a>
        <br>
        SPQR will search for a pattern in the provided multiFASTA file.
        </p>'
      ),
      textInput("pattern", "Specify a pattern", "^MC"),
      HTML('
      <p style="margin-left:15px;
        margin-right:15px;
        margin-top:-20px;
        text-align:justify;
        font-size:14px;
        color:white;">
        <br>
        Example sequence patterns:
        <ul>
        <li><strong>^CDE</strong> - <i>starting with CDE</i></li>
        <li><strong>FMG$</strong> - <i>ending with FMG</i></li>
        <li><strong>ALGT</strong> - <i>containing the sequence ALGT</i></li>
        <li><strong>^M...C</strong> - <i>starting with M, any aminoacid three times (...), then C</i></li>
        </ul>
        <br>
        Please refer to <a href=https://perlmaven.com/regex-cheat-sheet>PERL regex</a> for a full list of supported patterns.
      </p>'
      ),
      width = 400
    ),
    
    
    dashboardBody(
      tags$head(
        tags$link(rel="stylesheet",type="text/css",href="style.css")
      ),
      shinyjs::useShinyjs(),
      shinyjs::hidden(
        wellPanel(
          id="report",
          textOutput("nseq"),
          textOutput("nhits"),
          br()
        ),
        wellPanel(
          id="report2",
          DT::dataTableOutput("hitnames"),
          downloadButton(outputId="download",label="Download matching sequences in FASTA format",class="bbutton"),
          downloadButton(outputId="downloadtxt",label="Download matching sequence titles",class="bbutton")
        )
      ),
      
      
      HTML(
        '<footer id="colophon" role="contentinfo" align="center">
      <div class="site-info">
      <p>App developed by Federico M. Giorgi & Francesco Licausi</p>
      <br>
      </div>
      </footer>'
      )
    )
    
  )
)

