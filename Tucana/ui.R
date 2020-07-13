Packages <- c("shiny","ggplot2","DT","shinythemes","ggvis","shinyjs","shinyBS","shinydashboard",
              "shinymaterial","shinycssloaders","shinybusy")
lapply(Packages, function(x){
  suppressPackageStartupMessages(library)
  }
)
source("/srv/shiny-server/Tucana/data/LucaFunct.R")
library(shinybusy)
library(shinydashboardPlus)

allTissue = insertPath("/srv/shiny-server/DATA/")
choicesVec <- list("Show 2 genes correlation in a tissue",
                   "Compare 2 genes correlation in two tissues",
                   "Search the main coexpressor genes for a gene in a tissue",
                   "Search the main DEGs for the couples healthy/tumor")

# tissueCouple <- list("tcga_LGG,gtex_Brain",
#                      "tcga_BRCA,gtex_Breast",
#                      "tcga_COAD,gtex_Colon",
#                      "tcga_ESCA,gtex_Esophagus",
#                      "tcga_KICH,gtex_Kidney",
#                      "tcga_KIRP,gtex_Kidney",
#                      "tcga_KIRC,gtex_Kidney",
#                      "tcga_LIHC,gtex_Liver",
#                      "tcga_LUAD,gtex_Lung",
#                      "tcga_LUSC,gtex_Lung",
#                      "tcga_OV,gtex_Ovary",
#                      "tcga_PAAD,gtex_Pancreas",
#                      "tcga_PRAD,gtex_Prostate",
#                      "tcga_STAD,gtex_Stomach",
#                      "tcga_TGCT,gtex_Testis",
#                      "tcga_THCA,gtex_Thyroid",
#                      "tcga_UCS,gtex_Uterus",
#                      "tcga_UCEC,gtex_Uterus")

shinyjs::useShinyjs()
# Define UI for application
shinyUI(
  shinydashboardPlus::dashboardPagePlus(
    shinydashboardPlus::dashboardHeaderPlus(title = "Tucana"),
    dashboardSidebar(
      add_busy_spinner(spin="atom",position = "bottom-right",
                       height = "300px",width = "300px",margins = c(530,50)),
      shinyjs::useShinyjs(),
      
      sidebarMenu(
        menuItem("Input", tabName = "Input", selected = TRUE, startExpanded = TRUE,
          #Select the research
          selectInput(inputId = "selectMode",
                     label = "Select the type of analysis:",
                     choices = choicesVec,
                     selected = "Show 2 genes correlation in a tissue"),
          bsTooltip("selectMode", "The type of analysis you want make",placement = "top",
            options = list(container = "body")),
          # Select variable for gene1
          selectizeInput(inputId = "g1",
                         label = "Gene 1:",
                         choices = NULL,
                         selected = "MYC",
                         options = list(highlight = TRUE)),
          bsTooltip("g1", "First gene to be selected",placement = "top",
                    options = list(container = "body")),
          # Select variable for gene2
          selectizeInput(inputId = "g2",
                         label = "Gene 2:",
                         choices = NULL,
                         selected = "E2F3",
                         options = list(highlight = TRUE)),
          bsTooltip("g2", "Second gene to be selected",placement = "top",
                    options = list(container = "body")),
          # Select variable for tissue
          selectInput(inputId = "tissue1",
                      label = "First tissue selection:",
                      choices = list(
                        "GTEX" = c(allTissue[1:30]),
                        "NBL" = c(allTissue[31:33]),
                        "TCGA" = c(allTissue[34:66])
                      ),
                      selected = "gtex_Colon"),
          bsTooltip("tissue1", "First tissue to be selected",placement = "top",
                    options = list(container = "body")),
          # Select variable for tumors
          selectInput(inputId = "tissue2",
                      label = "Second tissue selection:",
                      choices = list(
                        "GTEX" = c(allTissue[1:30]),
                        "NBL" = c(allTissue[31:33]),
                        "TCGA" = c(allTissue[34:66])
                      ),
                      selected = "tcga_COAD"),
          bsTooltip("tissue2", "Second tissue to be selected",placement = "top",
                    options = list(container = "body")),
          # Abbreviation  Tables
          actionButton("showTable","TCGA CODE TABLES"),
          #Select type of input data
          radioButtons(inputId = "selectDataType",
                       label = "Select Data Type:",
                       choices <- list("VST", "FPKM","TPM"),
                       selected = "VST"),
          bsTooltip("selectDataType", "The type of data you want to analyze",placement = "top",
                    options = list(container = "body")),
          # Select the correlation type you want to use
          radioButtons(inputId = "corrType",
                       label = "Correlation type:",
                       choices <- list("Pearson", "Spearman")),
          bsTooltip("corrType", "Correlation type to be selected",placement = "top",
                    options = list(container = "body")),
          # Select the genes you want to show
          radioButtons(inputId = "typeOfGenes",
                       label = "Select the filter for the type of genes:",
                       choices <- list("All",
                                       "Transcription Factors",
                                       "Co-Transcription factors",
                                       "Signaling proteins",
                                       "Surface proteins"))
          ,icon=icon("mouse-pointer")),
        menuItem("Info", tabName = "infos",
          HTML('<span style="margin-left:30px;
                 margin-right:15px;
                 margin-top:10px;
                 text-align:justify;
                 font-size:14px;
                 color:white;
                 white-space: normal;">
          Tucana is a tool that provides information about genes coexpression
          in different types of tissues. Input datasets are available in three
          formats: VST, FPKM and TPM. There are four possible analysis: a single
          correlation analysis that shows the Pearson or Sperman correlation value
          between two genes in a tissue, a comparison of correlation between two 
          selected tissues, a table of the top co-expressed genes for the gene in
          input and a tissue and a table of the top differentialy co-expressed genes for
          the gene selected in the input in two tissues.
          </span>')
          ,icon=icon("info")),
        menuItem("Data", tabName = "data",
          HTML('<span style="margin-left:15px;
                 margin-right:15px;
                 margin-top:10px;
                 text-align:justify;
                 font-size:14px;
                 color:white;
                 white-space: normal;">
                 <br>
          Expression datasets comes from GTeX and TCGA online databases. Healthy tissues
          data come from the first one while tumoral ones come from the second. Moreover,
          three datasets containing NBL data have been retrieved from KOCAK, NRC and TARGET
          databases.
          </span>')
          ,icon=icon("database"))
      )
    ),
    dashboardBody(
      tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="style.css")
      ),
      
      tags$head(
        tags$style(
          HTML('
            /* logo */
            .skin-blue .main-header .logo {
            background-color: #2c3b41;
            }

            /* logo when hovered */
            .skin-blue .main-header .logo:hover {
            background-color: #2c3b41;
            }
            
            /* navbar (rest of the header) */
            .skin-blue .main-header .navbar {
            background-color: #2c3b41;
            }
            
            /* main sidebar */
            .skin-blue .main-sidebar {
            background-color:#2c3b41;
            }
            
            /* active selected tab in the sidebarmenu */
            .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
            background-color: white;
            color: black;
            }

            /* other links in the sidebarmenu */
            .skin-blue .main-sidebar .sidebar .sidebar-menu a{
            background-color: black;
            color: white;
            }
            
            /* toggle button when hovered  */
            .skin-blue .main-header .navbar .sidebar-toggle:hover{
            background-color: #2c3b41;
            }

            /* body */
            .content-wrapper, .right-side {
            background-color: white;
            }   
               
               '
          )
        )
      ),
      
      shinyjs::useShinyjs(),
      fluidRow(
        column(
          plotOutput('firstPlot', width = "600px", height = "600px"),
          dataTableOutput('topCoexTis'),
          dataTableOutput('DEGTable'),width = 5),
        column(
          plotOutput('secondPlot',width = "600px", height = "600px"),
          plotOutput('plotRow', width = "600px", height = "600px"),width = 5),
        column(
          wellPanel(
            HTML('<p style="margin-left:40px;
                      font-size:25px;
                      font-weight: bold;">
            Graphics edits
           </p>'),
            # Select the point size
            sliderInput(inputId = "cex",
                       label = "Point Size (cex)",                            
                       min = 0.25, max = 2, step = 0.25, value = 1),
            #Download Table Button
            downloadButton(outputId = "downloadCSV",label = "Download CSV",class = "bottone"),
            #Download PDF Button
            downloadButton(outputId = "downloadPDF",label = "Download PDF shown plot/s",class = "bottone"),
            #Download GTEX PNG Button
            downloadButton(outputId = "downloadAllPlotsG",label = "Download PNG GTEX tissues",class = "bottone"),
            #Download TCGA PNG Button
            downloadButton(outputId = "downloadAllPlotsT",label = "Download PNG TCGA tissues",class = "bottone"),
            #Download NBL PNG Button
            downloadButton(outputId = "downloadAllPlotsN",label = "Download PNG NBL tissues",class = "bottone")
          ),
          wellPanel(
            HTML('<p style="margin-left:40px;
                      font-size:25px;
                      font-weight: bold;">
            Other Tools
           </p>'),
            HTML('<a href="/KaMeR" style="font-size:20px;
                                          color:black;
                                          text-decoration: underline;">KaMeR</a>')
          ),width = 2
        )
      ),
      fluidRow(
        column(
          plotOutput('plotRowDEGT1', width = "400px", height = "400px"),width = 3
        ),
        column(
          plotOutput('plotRowDEGT2', width = "400px", height = "400px"),width = 3
        )
      )
    ),
    HTML('<footer id="colophon" role="contentinfo" align="center">
        			<div class="site-info">
        			   <p style="color:white;">Copyright (c) 2020 Luca Triboli All rights reserved</p>
        			   <h3><a href="/">Giorgilab</a></h3>
        			   <br>
        			</div>
        		</footer>')
  )
)