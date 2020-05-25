source("/srv/shiny-server/KaMeR/data/stepsurvival.R")

allTissue = insertPath("/srv/shiny-server/DATA/")
choicesVec <- list("Show classic survival study",
                   "Show step survival study",
                   "Show combo survival study")

shinyjs::useShinyjs()
# Define UI for application
shinyUI(
  dashboardPage(skin = "black",
    dashboardHeader(title = "KaMeR"),
    dashboardSidebar(
      shinyjs::useShinyjs(),
      HTML('<p style="margin-left:15px;
                        margin-right:15px;
                        margin-top:10px;
                        font-style:italic;
                        text-align:justify;
                        font-size:18px;
                        color:white;">
              KaMeR is a tool that perform different survival analysis
             </p>'),
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
    ),
    dashboardBody(
      tags$head(
        tags$link(rel="stylesheet", type="text/css",href="bootstrap.css")
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
        			   <p>Copyright (c) 2020 Luca Triboli All rights reserved</p>
        			   <h3><a href="/">Giorgilab</a></h3>
        			   <br>
        			</div>
        		</footer>')
    )
  )
)