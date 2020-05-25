suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(ggvis))
suppressPackageStartupMessages(library(shinyjs))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(shinymaterial))
suppressPackageStartupMessages(library(shinycssloaders))


shinyjs::useShinyjs()
# Define UI for application that plots random distributions 
shinyUI(
  
  dashboardPage(skin = "black",
                dashboardHeader(title = "SurfaceR"),
                dashboardSidebar(
                  shinyjs::useShinyjs(),
                  p("This is the solution to surface analysis of human cells")
                  # those are the inputs
                  # use selectInput() to create a tendina dove scegliere un input alla volta da una lista
                  # selectInput(inputId = "selectMode",
                  #             label = "Select the research:",
                  #             choices = choicesVec,
                  #             selected = "Show 2 genes correlation in a tissue"),
                  #
                  # use radioButtons() to create a bottoni da scegliere mutualmente esclusivi
                  # radioButtons(inputId = "selectDataType",
                  #              label = "Select Data Type:",
                  #              choices <- list("Expmat", "FPKM","TPM"),
                  #              selected = "Expmat"),
                  #
                  # use selectizeInput() to create a tendina dove scegliere un input alla volta ma più personalizzabile nel server.R
                  # selectizeInput(inputId = "g1",
                  #                label = "Gene 1:",
                  #                choices = NULL,
                  #                selected = "MYC",
                  #                options = list(highlight = TRUE)),
                  #
                  # use sliderInput() to create a slider with min, max, step and default initial value
                  # sliderInput(inputId = "cex",
                  #             label = "Point Size (cex)",                            
                  #             min = 0, max = 2, step = 0.25, value = 1),
                  #
                  # use downloadButton() to create a button that can download table or pdf
                  # downloadButton(outputId = "downloadCSV",label = "Download CSV",class = "button"),
                  #
                  # use bsTooltip() to create ballons with help text for users
                  # bsTooltip(id="corrType", "Correlation type to be selected",placement = "top",
                  #           options = list(container = "body")),
                ),
                dashboardBody(
                  # those are the outputs
                  # use this line to insert an icon on the browser tab
                  # tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
                  shinyjs::useShinyjs()
                  # use fluidRow() to create uno spazio equamente diviso in colonne per gli output
                  # fluidRow(
                  #   column(5,
                  #          plotOutput('firstPlot',click = "plot_click", width = "600px", height = "600px")),
                  #   column(5,
                  #          plotOutput('secondPlot',click = "plot_click",width = "600px", height = "600px"))
                  # ),
                  #
                  # use dataTableOutput() to show as output a table
                  # dataTableOutput('topCoexTis'),
                  #
                  # use plotOutput() to show as output a plot
                  # plotOutput('plotRow',click = "plot_click")
                )
  )
)
