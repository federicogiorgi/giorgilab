library(shiny)
library(venn)

observe({
  x<-input$numList
  if(x=="2"){
    shinyjs::hide("selectInLi3")
    shinyjs::hide("selectInLi4")
    shinyjs::hide("selectInLi5")
    shinyjs::hide("selectInLi6")
    shinyjs::hide("selectInLi7")
  }
  else if(x=="3"){
    shinyjs::show("selectInLi3")
    shinyjs::hide("selectInLi4")
    shinyjs::hide("selectInLi5")
    shinyjs::hide("selectInLi6")
    shinyjs::hide("selectInLi7")
  }
  else if(x=="4"){
    shinyjs::show("selectInLi3")
    shinyjs::show("selectInLi4")
    shinyjs::hide("selectInLi5")
    shinyjs::hide("selectInLi6")
    shinyjs::hide("selectInLi7")
  }
  else if(x=="5"){
    shinyjs::show("selectInLi3")
    shinyjs::show("selectInLi4")
    shinyjs::show("selectInLi5")
    shinyjs::hide("selectInLi6")
    shinyjs::hide("selectInLi7")
  }
  else if(x=="6"){
    shinyjs::show("selectInLi3")
    shinyjs::show("selectInLi4")
    shinyjs::show("selectInLi5")
    shinyjs::show("selectInLi6")
    shinyjs::hide("selectInLi7")
  }
  else if(x=="7"){
    shinyjs::show("selectInLi3")
    shinyjs::show("selectInLi4")
    shinyjs::show("selectInLi5")
    shinyjs::show("selectInLi6")
    shinyjs::show("selectInLi7")
  }
})


output$Vennar <- renderPlot({
  req(input$selectInLi1)
  req(input$selectInLi2)
  if(input$numList=="2"){
    
    # message(input$selectInLi1)
    # message(input$selectInLi2)
    firstList=unlist(strsplit(input$selectInLi1,","))
    secondList=unlist(strsplit(input$selectInLi2,","))
    mirnas<-list(firstList, secondList)
    names(mirnas) <- c("firstList", "secondList")
    venn(mirnas,ilab=TRUE,zcolor="style")
  }
  else if(input$numList=="3"){
    req(input$selectInLi3)
    
    # message(input$selectInLi1)
    # message(input$selectInLi2)
    # message(input$selectInLi3)
    firstList=unlist(strsplit(input$selectInLi1,","))
    secondList=unlist(strsplit(input$selectInLi2,","))
    thirdList=unlist(strsplit(input$selectInLi3,","))
    mirnas<-list(firstList, secondList, thirdList)
    names(mirnas) <- c("firstList", "secondList", "thirdList")
    venn(mirnas,ilab=TRUE,zcolor="style")
  }
  else if(input$numList=="4"){
    req(input$selectInLi4)
    
    # message(input$selectInLi1)
    # message(input$selectInLi2)
    # message(input$selectInLi3)
    # message(input$selectInLi4)
    firstList=unlist(strsplit(input$selectInLi1,","))
    secondList=unlist(strsplit(input$selectInLi2,","))
    thirdList=unlist(strsplit(input$selectInLi3,","))
    fourthList=unlist(strsplit(input$selectInLi4,","))
    mirnas<-list(firstList, secondList, thirdList,fourthList)
    names(mirnas) <- c("firstList", "secondList", "thirdList", "fourthList")
    venn(mirnas,ilab=TRUE,zcolor="style")
  }
  else if(input$numList=="5"){
    req(input$selectInLi5)
    
    # message(input$selectInLi1)
    # message(input$selectInLi2)
    # message(input$selectInLi3)
    # message(input$selectInLi4)
    # message(input$selectInLi5)
    firstList=unlist(strsplit(input$selectInLi1,","))
    secondList=unlist(strsplit(input$selectInLi2,","))
    thirdList=unlist(strsplit(input$selectInLi3,","))
    fourthList=unlist(strsplit(input$selectInLi4,","))
    fifthList=unlist(strsplit(input$selectInLi5,","))
    mirnas<-list(firstList, secondList, thirdList, fourthList, fifthList)
    names(mirnas) <- c("firstList", "secondList", "thirdList", "fourthList", "fifthList")
    venn(mirnas,ilab=TRUE,zcolor="style")
  }
  else if(input$numList=="6"){
    req(input$selectInLi6)
    
    # message(input$selectInLi1)
    # message(input$selectInLi2)
    # message(input$selectInLi3)
    # message(input$selectInLi4)
    # message(input$selectInLi5)
    # message(input$selectInLi6)
    firstList=unlist(strsplit(input$selectInLi1,","))
    secondList=unlist(strsplit(input$selectInLi2,","))
    thirdList=unlist(strsplit(input$selectInLi3,","))
    fourthList=unlist(strsplit(input$selectInLi4,","))
    fifthList=unlist(strsplit(input$selectInLi5,","))
    sixthList=unlist(strsplit(input$selectInLi6,","))
    mirnas<-list(firstList, secondList, thirdList, fourthList, fifthList, sixthList)
    names(mirnas) <- c("firstList", "secondList", "thirdList", "fourthList", "fifthList", "sixthList")
    venn(mirnas,ilab=TRUE,zcolor="style")
  }
  else if(input$numList=="7"){
    req(input$selectInLi7)
    
    # message(input$selectInLi1)
    # message(input$selectInLi2)
    # message(input$selectInLi3)
    # message(input$selectInLi4)
    # message(input$selectInLi5)
    # message(input$selectInLi6)
    firstList=unlist(strsplit(input$selectInLi1,","))
    secondList=unlist(strsplit(input$selectInLi2,","))
    thirdList=unlist(strsplit(input$selectInLi3,","))
    fourthList=unlist(strsplit(input$selectInLi4,","))
    fifthList=unlist(strsplit(input$selectInLi5,","))
    sixthList=unlist(strsplit(input$selectInLi6,","))
    seventhList=unlist(strsplit(input$selectInLi6,","))
    mirnas<-list(firstList, secondList, thirdList, fourthList, fifthList, sixthList, seventhList)
    names(mirnas) <- c("firstList", "secondList", "thirdList", "fourthList", "fifthList", "sixthList", "seventhList")
    venn(mirnas,ilab=TRUE,zcolor="style")
  }
})


# output$content2 <- renderUI({
#   "Tab 2 content"
# })