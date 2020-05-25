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
source("/srv/shiny-server/Corto/data/LucaFunct.R")

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

allTissue = insertPath("/srv/shiny-server/Corto/data/")
choicesVec <- list("Show 2 genes correlation in a tissue",
                   "Compare 2 genes correlation in two tissues",
                   "Search the main coexpressor genes for a gene in a tissue",
                   "Search the main DEGs for the couples healthy/tumor",
                   "Perform a KaplanMeier study",
                   "---")

tissueCouple <- list("tcga_LGG,gtex_Brain",
                     "tcga_BRCA,gtex_Breast",
                     "tcga_COAD,gtex_Colon",
                     "tcga_ESCA,gtex_Esophagus",
                     "tcga_KICH,gtex_Kidney",
                     "tcga_KIRP,gtex_Kidney",
                     "tcga_KIRC,gtex_Kidney",
                     "tcga_LIHC,gtex_Liver",
                     "tcga_LUAD,gtex_Lung",
                     "tcga_LUSC,gtex_Lung",
                     "tcga_OV,gtex_Ovary",
                     "tcga_PAAD,gtex_Pancreas",
                     "tcga_PRAD,gtex_Prostate",
                     "tcga_STAD,gtex_Stomach",
                     "tcga_TGCT,gtex_Testis",
                     "tcga_THCA,gtex_Thyroid",
                     "tcga_UCS,gtex_Uterus",
                     "tcga_UCEC,gtex_Uterus")

shinyjs::useShinyjs()
# Define UI for application that plots random distributions 
shinyUI(
  dashboardPage(skin = "black",
    dashboardHeader(title = "Tucana"),
    dashboardSidebar(
      shinyjs::useShinyjs(),
      p("Tucana is a tool that provide informations about genes coexpression in different types of tissues"),
      #Select the research
      selectInput(inputId = "selectMode",
                  label = "Select the research:",
                  choices = choicesVec,
                  selected = "---"),
      #Select type of input data
      radioButtons(inputId = "selectDataType",
                   label = "Select Data Type (expmat/rpms):",
                   choices <- list("Expmat", "RPMS"),
                   selected = "Expmat"),
      # Select variable for tissue
      selectInput(inputId = "tissue1",
                  label = "First tissue selection:",
                  choices = allTissue,
                  selected = "tcga_COAD"),
      # Select variable for tumors
      selectInput(inputId = "tissue2",
                  label = "Second tissue selection:",
                  choices = allTissue,
                  selected = "gtex_Colon"),
      # Select variable for tissue
      selectInput(inputId = "coupletissue",
                  label = "Select a couple of tissues:",
                  choices = tissueCouple,
                  selected = "tcga_BRCA,gtex_Breast"),
      # Select variable for gene1
      selectizeInput(inputId = "g1",
                     label = "Gene 1:",
                     choices = NULL,
                     selected = "BRCA1",
                     options = list(highlight = TRUE)),
      # Select variable for gene2
      selectizeInput(inputId = "g2",
                     label = "Gene 2:",
                     choices = NULL,
                     selected = "SDF4",
                     options = list(highlight = TRUE)),
      # Select the correlation type you want to use
      radioButtons(inputId = "corrType",
                   label = "Select the correlation type you want to use:",
                   choices <- list("Pearson", "Spearman")),
      #Download Button
      downloadButton(outputId = "downloadData",label = "Download"),
      # Select the genes you want to show
      radioButtons(inputId = "typeOfGenes",
                   label = "Select the type of genes you want to see:",
                   choices <- list("All", "Transcription Factors", "co-Transcription factors", "signaling proteins"))
    ),
    dashboardBody(
      shinyjs::useShinyjs(),
                 plotOutput('firstPlot',click = "plot_click"), #%>% withSpinner(color="#0dc5c1"),
                 plotOutput('secondPlot',click = "plot_click"), #%>% withSpinner(color="#0dc5c1"),
                 dataTableOutput('topCoexTis'), #%>% withSpinner(color="#850506"),
                 dataTableOutput('DEGTable'), #%>% withSpinner(color="#850506"),
                 plotOutput('plotRow',click = "plot_click"), #%>% withSpinner(color="#0dc5c1"),
                 plotOutput('plotRowDEGTum',click = "plot_click"), #%>% withSpinner(color="#0dc5c1"),
                 plotOutput('plotRowDEGHeal',click = "plot_click"), #%>% withSpinner(color="#0dc5c1"),
                 textOutput('Home')
    )
  )
)