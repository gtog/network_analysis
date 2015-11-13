# Network Analysis Shiny App
# ui.R
# Adam Ducan, 2014


shinyUI(fluidPage(theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Network Analysis App"),
  
  # and to specify whether outliers should be included
  sidebarLayout(
    
    sidebarPanel(
      width = 2,
      
      fileInput('userfile', 'Holdings File',
                accept = c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
      
      checkboxInput('addAUMYesNo', 'Add AUM File?', value = FALSE),
      
      fileInput('userAUM', label = 'Vertex Variable File', accept = c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
      
      checkboxInput('addEdgeYesNo', 'Add Edge File?', value = FALSE),
      
      fileInput('userEdge', label = 'Edge Variable File', accept = c('text/csv', 'text/comma-separated-values,text/plain','.csv')),
      
      uiOutput("numHoldings"),
      
      uiOutput("userVertexColor"),
      helpText("Yeah, the color meaning is tough."),
      
      uiOutput("userLayout"),
      # bsTooltip(id = "userLayout", title = "", placement = "", trigger = ""),
      
      uiOutput("userGraphType"),
      # bsTooltip(id = "userGraphType", title = "", placement = "", trigger = ""),
      
      uiOutput("userEdgeType"),
      # bsTooltip(id = "userEdgeType", title = "", placement = "", trigger = ""),
      
      uiOutput("alphaSlider"),
      # bsTooltip(id = "alpha", title = "", placement = "", trigger = ""),
      
      uiOutput("labelCexSlider"),
      # bsTooltip(id = "labelCex", title = "", placement = "", trigger = ""),
      
      uiOutput("vertexSizeSlider")
      # bsTooltip(id = "vertexSize", title = "", placement = "", trigger = ""),
      
      
    ), # end SidebarPanel
    
    mainPanel(
      
      width = 10,
      
      tabsetPanel(
        
        tabPanel("Network Graph", plotOutput("networkPlot", height = "800px"), value = 1),
        
        tabPanel("Data", dataTableOutput("thedata"), value = 2),
        
        tabPanel("Statistics", verbatimTextOutput("statsOut"), value = 3),
        
        tabPanel("Help", verbatimTextOutput("helpOut"), value = 4),
        
        id = "condPanels"
      )
    )
  )
)
)
