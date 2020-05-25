library(shiny)
library(venn)

tabPanel(
  "Vennar",
  shinyjs::useShinyjs(),
  sidebarLayout(
    ################################          Inputs
    sidebarPanel(
      fluidRow(
        helpText("Vennar is a tool that manage to calculate intersection between sets of items"),
        selectInput(inputId = "numList", label= "Insert number of sets:", choices=as.character(c(2:7)),selected="2"),
        helpText("Insert the below lists of elements comma-separated"),
        #Select item list 1
        textAreaInput(inputId = "selectInLi1", label = "Insert item list 1:",
                      value=""),
        #Select item list 2
        textAreaInput(inputId = "selectInLi2", label = "Insert item list 2:",
                      value=""),
        #Select item list 3
        textAreaInput(inputId = "selectInLi3", label = "Insert item list 3:",
                      value=""),
        #Select item list 4
        textAreaInput(inputId = "selectInLi4", label = "Insert item list 4:",
                      value=""),
        #Select item list 5
        textAreaInput(inputId = "selectInLi5", label = "Insert item list 5:",
                      value=""),
        #Select item list 6
        textAreaInput(inputId = "selectInLi6", label = "Insert item list 6:",
                      value=""),
        #Select item list 7
        textAreaInput(inputId = "selectInLi7", label = "Insert item list 7:",
                      value="")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("VenOutput",
          plotOutput('Vennar')
        )
      )
    )
  )
  
)