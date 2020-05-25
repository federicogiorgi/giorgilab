library(shiny)
library(ggplot2)
library(DT)
library(shinythemes)
library(ggvis)
library(shinyjs)
library(shinyBS)
library(shinydashboard)
library(shinymaterial)
library(shinycssloaders)

mycss <- "
#plot-container {
  position: relative;
}
#loading-spinner {
  position: absolute;
  left: 50%;
  top: 50%;
  z-index: -1;
  margin-top: -33px;  /* half of the spinner's height */
  margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
  z-index: -2;
}
"

Genomes = c("Homo sapiens","Drosophila melanogaster")

shinyjs::useShinyjs()
tabPanel(
  "GeneCiR",
  shinyjs::useShinyjs(),
  sidebarLayout(
    ################################          Inputs
    div(
      id ="SidebarGeneCiR",
      sidebarPanel(
        fluidRow(
          helpText("GeneCiR Help"),

          # Select variable for genome
          selectInput(inputId = "genome1",
                      label = "First genome selection:",
                      choices = Genomes,
                      selected = "Homo sapiens"),
          # Select variable for genome
          selectInput(inputId = "genome2",
                      label = "Second genome selection:",
                      choices = Genomes,
                      selected = "Drosophila melanogaster"),
          # Select variable for gene1
          fileInput(inputId = "Genesfile", label = "Genes file input:"),
          #Download Button
          downloadButton(outputId = "downloadImage",label = "Download")
        )
      )
    ),
    mainPanel(
      bsButton("showpanelGeneCiR", "Show/hide sidebar", type = "toggle", value = TRUE),
      tabsetPanel(
        tabPanel("GeneOutput",
                 textOutput('HomeGCR'),
                 plotOutput('genomesPlot',click = "plotCirc_click")
        )
      )
    )
  )
  
)

