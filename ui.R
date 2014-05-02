library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Tatoeba benchmark viewer"),
  
  sidebarPanel(
    selectInput("dataset1", "First dataset", choices=c("base-vm", "base-vm_from-ram")),
    selectInput("dataset2", "Second dataset", choices=c("base-vm_from-ram", "base-vm")),
    
    uiOutput("pagesUI")
  ),
  
  mainPanel(
    textOutput("message"),
    plotOutput("distPlot", height="100%")
  )
))
