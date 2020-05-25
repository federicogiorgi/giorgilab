library(shiny)

tabPanel(
  "MutDif",
  shinyjs::useShinyjs(),
  sidebarLayout(
    ################################          Inputs
    sidebarPanel(
      fluidRow(
        #Select item list 1
        textInput(inputId = "selectInLi1", label = "Insert something:",
                  value="")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Output",
                 plotOutput('MutDif')
        )
      )
    )
  )
)