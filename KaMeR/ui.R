source("/srv/shiny-server/KaMeR/data/stepsurvival.R")
library(shinybusy)
library(shinydashboardPlus)

allTissue = insertPath("/srv/shiny-server/DATA/")
choicesVec <- list("Show classic survival study",
                   "Show step survival study",
                   "Show combo survival study")

shinyjs::useShinyjs()
# Define UI for application
shinyUI(
  shinydashboardPlus::dashboardPagePlus(
    shinydashboardPlus::dashboardHeaderPlus(title = "KaMeR"),
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
                  selected = "Show step survival study"),
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
                  choices = allTissue,
                  selected = "tcga_COAD"),
      bsTooltip("tissue1", "Tissue to be selected",placement = "top",
                options = list(container = "body")),
      # Abbreviation Tables
      actionButton("showTable","TCGA CODE TABLES"),
      # Select type of input data
      radioButtons(inputId = "selectDataType",
                   label = "Select Data Type:",
                   choices <- list("VST", "FPKM","TPM"),
                   selected = "VST"),
      bsTooltip("selectDataType", "The type of data you want to analyze",placement = "top",
                options = list(container = "body")),
      # Select the steps number
      sliderInput(inputId = "stepNum",
                  label = "Steps number",                            
                  min = 3, max = 10, step = 1, value = 10)
      ,icon=icon("mouse-pointer")),
      menuItem("Info", tabName = "infos",
               HTML('<span style="margin-left:30px;
                    margin-right:15px;
                    margin-top:10px;
                    text-align:justify;
                    font-size:14px;
                    color:white;
                    white-space: normal;">
                    KaMeR is a tool that perform different survival analysis
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
                    Expression and clinical datasets comes from GTeX and TCGA online databases. Healthy
                    tissues data come from the first one while tumoral ones come from the second. Moreover,
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
          plotOutput('classicKM', width = "1000px", height = "500px"),
          plotOutput('stepKM', width = "1000px", height = "500px"),
          plotOutput('comboKMTitle', width = "800px", height = "100px"),
        width = 8),
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
            #Download Classic 1p Button
            downloadButton(outputId = "downloadDataClas1p",label = "Download this plot", class="bottone"),
            #Download Classic Button
            downloadButton(outputId = "downloadDataClas",label = "Download all tissues plots", class="bottone"),
            #Download Step 1p Button
            downloadButton(outputId = "downloadDataStep1p",label = "Download this plot", class="bottone"),
            #Download Step Button
            downloadButton(outputId = "downloadDataStep",label = "Download all tissues plots", class="bottone")
          ),
          width = 2
        )
      ),
      fluidRow(
        column(
               plotOutput('comboKM1', width = "400px", height = "400px"),width = 3),
        column(
               plotOutput('comboKM2', width = "400px", height = "400px"),width = 3),
        column(
          wellPanel(
            HTML('<p style="margin-left:40px;
                        font-size:25px;
                        font-weight: bold;">
              Other Tools
             </p>'),
            HTML('<a href="/Tucana" style="font-size:20px;
                                            color:black;
                                            text-decoration: underline;">Tucana</a>')
          ),offset = 8,width = 2
        )
      ),
      fluidRow(
        column(
               plotOutput('comboKM3', width = "400px", height = "400px"),width = 3),
        column(
               plotOutput('comboKM4', width = "400px", height = "400px"),width = 3)
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
)