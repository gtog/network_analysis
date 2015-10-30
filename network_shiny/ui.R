# Network Analysis Shiny App
# ui.R
# Adam Ducan, 2014

# Load libraries...
library(shiny)

shinyUI(pageWithSidebar(
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Network Analysis App"),
  
  # and to specify whether outliers should be included
  sidebarPanel(
    HTML("Use the 'Choose File' button to load a .csv file. See the 'Help' tab for format instructions."),
    #tags$hr(),
    
    fileInput('userfile', '',
              accept=c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
    
    numericInput("numHoldings", "Number of holdings to include: ",value=25),
    
    helpText("Note: Enter the number of stock holdings to include. 25 is usually
             a good number to start. Larger numbers can lead to overly complicated 
             displays."),
    #tags$hr(),
    
    selectInput("userVertexColor", "Color Meaning: ",choices=as.list(colorTypes),""),
    
    helpText("Note: For example, deep red in Percent Uniqe indicates that the manger
             has few unique positions. Light Green implies many unique holdings. Percent Min Overlap
             will yield all the same color, provided each manager has at least one unique position.
             I suggest Percent Avg Overlap for Stock View graphs."),
    #tags$hr(),
    
    selectInput("userLayout", "Graph Layout: ",choices=as.list(layoutTypes),""),
    bsTooltip(id = "userLayout", title = "", placement = "", trigger = ""),
    
    radioButtons("userGraphType", "Choose Graph Type: ",choices=as.list(graphTypes),selected=NULL),
    bsTooltip(id = "userGraphType", title = "", placement = "", trigger = ""),
    
    radioButtons("userEdgeType","Choose Edge Type: ",choices=as.list(edgeTypes),selected="Straight"),
    bsTooltip(id = "userEdgeType", title = "", placement = "", trigger = ""),
    
    uiOutput("alphaSlider"),
    bsTooltip(id = "alpha", title = "", placement = "", trigger = ""),
    
    uiOutput("labelCexSlider"),
    bsTooltip(id = "labelCex", title = "", placement = "", trigger = ""),
    
    uiOutput("vertexSizeSlider"),
    bsTooltip(id = "vertexSize", title = "", placement = "", trigger = "")
    
    #submitButton("Update View")
    
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Network Graph",plotOutput("networkPlot",height="800px"),value=1),
      tabPanel("Data",dataTableOutput("thedata"),value=2),
      tabPanel("Statistics",verbatimTextOutput("statsOut"),value=3),
      tabPanel("Help",verbatimTextOutput("helpOut"),value=4),
      id="condPanels"
    )
  )
))

