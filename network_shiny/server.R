# Basic Shiny App
# server.R
# Adam Duncan, 2013

# global variable to hold user supplied AUM...
myaum <- NULL

shinyServer(function(input,output,session) {
  

  
  
  #### RENDER UI ELEMENTS ####

  output$numHoldings <- renderUI({
    numericInput("numHoldings", "Number of holdings to include: ",min = 5, max = 200, value = 25, step = 5)
  })
  
  output$userVertexColor <- renderUI({
    selectInput("userVertexColor", "Color Meaning: ", choices = colorTypes, selected = colorTypes[[1]])
  })
  
  output$userLayout <- renderUI({
    selectInput("userLayout", "Graph Layout: ", choices = layoutTypes, selected = layoutTypes[[1]])
  })
  
  output$userGraphType <- renderUI({
    radioButtons("userGraphType", "Choose Graph Type: ", choices = graphTypes, selected = graphTypes[[1]])
  })
  
  output$userEdgeType <- renderUI({
    radioButtons("userEdgeType","Choose Edge Type: ", choices = edgeTypes, selected = edgeTypes[[2]])
  })
  
  output$alphaSlider <- renderUI({
    sliderInput("alpha", "Alpha:", min = 0, max = 1, value = .80, step = .1, animate = FALSE)
  })
  
  output$labelCexSlider <- renderUI({
    sliderInput("labelCex", "Label Size:", min = 0.01, max = 2.0, value = .8, step = .05, animate = FALSE)
  })
  
  output$vertexSizeSlider <- renderUI({
    sliderInput("vertexSize","Vertex Size:", min = 5, max = 50, value = 20, step = 5, animate = FALSE)
  })
  
  
  
  
  #### REACTIVE FUNCTIONS ####
  
  getUserData <- eventReactive(input$userfile$datapath, {
    
      inFile_holdings <- input$userfile$datapath
      
      if (is.null(inFile_holdings)) {
        return("Waiting for user to select a file.")
      } else {
        mydata <- read.csv(inFile_holdings, header =  TRUE, row.names = 1)
        return(mydata)
      }
  })
  
  getUserAUM <- eventReactive(input$userAUM$datapath, {
    
    inFile_AUM <- input$userAUM$datapath
    
    if (is.null(inFile_AUM)) {
      return("Waiting for user to select a file.")
    } else {
      myaum <<- read.csv(inFile_AUM, header = FALSE)
      # return(myaum)
    }
  })
  
  chartData <- reactive({
    
    mydata <- getUserData()
    
    # Check to see if the user wants to add an AUM file...
    if (input$addAUMYesNo) {
      aum <- getUserAUM()
    }
    
    nh <-  input$numHoldings
    
    if (is.null(mydata)) {
      return(NULL)
    } else {
      
      data <- as.matrix(mydata)
      
      # data[is.na(data)] <- 0 # Make sure any NA's are set to zeros.
      
      temp <- apply(data, MARGIN = 1, FUN = sum) # sum holdings by stock...
      
      data.adj <- cbind(data, temp)
      data.adj <- data.adj[order(-temp),] #sort descending on most common positions
      data <- subset(data.adj, select = c(-temp)) #drop the sum column we just added...
      data <- data[1:input$numHoldings, ] # Just the top x most commonely held positions please...
      
      #Create the networks from the matrix of data we imported...
      stock.net <- data %*% t(data) # Multiply the data matrix with it's transpose. ie. make the network (adjacency matrix)...
      fund.net <- t(data) %*% data  # Multiply the transpose with the data matrix. 
      # diag(stock.net) <- NA # set the diagonals to NA
      # diag(fund.net) <- NA
      
      # Create the graphs...
      stock.g <- graph.adjacency(stock.net, mode = "undirected", weighted = TRUE, diag = FALSE)
      fund.g <- graph.adjacency(fund.net, weighted = TRUE, mode = "undirected", diag = FALSE)
      delete.vertices(stock.g, which(degree(stock.g) == 0))
      delete.vertices(fund.g, which(degree(fund.g) == 0))
     
      # Return a list of values...
      return(list(
        thedata = data, #1
        numHoldings = input$numHoldings, #2
        stockGraph = stock.g, #3
        fundGraph = fund.g, #4
        stockNetwork = stock.net, #5
        fundNetwork = fund.net, #6
        aumData = myaum #7
      ))
    }
  })
  
  getGraph <- reactive({
    
    g <- input$userGraphType
    d <- chartData()
    
    gr <- NULL
    
    if (length(d) == 0) {
      return(0)
    } else {
      if (g == "Fund View") {
        gr <- d$fundGraph
      } else {
        gr <- d$stockGraph
      }
      return(gr)
    }
  })
  
  getNetwork <- reactive({
    g <- input$userGraphType
    d <- chartData()
    
    net <- NULL
    
    ifelse(g == "Fund View", net <- d[[6]], net <- d[[5]])
    return(net)
  })
  
  getAlpha <- reactive({
    a <- input$alpha
    return(a)
  })
  
  getVertexSize <- reactive({
    v <- input$vertexSize
    return(v)
  })
  
  getColorMeaning <- reactive({
    
    a <- input$userVertexColor
    
    return(a)
  })
  
  
  
  
  #### PANEL SPECIFIC FUNCTIONS ####
  
  output$thedata <- renderDataTable({
    d <- getUserData()
    if (is.null(d)) { return(NULL) } 
    else {
      d
    }
  })
  
  output$statsOut <- renderPrint({
    d <- chartData()
    gr <- getGraph()
    net <- getNetwork()
    
    total.posn <- apply(net,1,sum)
    unique.posn <- diag(net)
    tot.posn.matrix <- matrix(rep(t(total.posn),nrow(net)),ncol = ncol(net))
    pct.overlap.matrix <- net/tot.posn.matrix # The rows here represent the amount of overlap between the managers. 
    pct.unique <- diag(pct.overlap.matrix) 
    temp <- pct.overlap.matrix
    diag(temp) <- 0
    pct.max.overlap <- apply(temp,1,max)
    diag(temp) <- 1
    pct.min.overlap <- apply(temp,1,min)
    diag(temp) <- NA
    pct.avg.overlap <- apply(temp,1,mean,na.rm = TRUE)
    
    cat("Total Number of Connections in Network: ","\n")
    print(total.posn)
    cat("\n")
    cat("Percent Overlap Matrix (Read by row only. Diagonal = Unique %): ","\n")
    print(pct.overlap.matrix)
    cat("\n")
    cat("Maximum Overlap (pct): ","\n")
    print(sort(pct.max.overlap,decreasing = TRUE))
    cat("\n")
    cat("Minimum Overlap (pct): ","\n")
    print(sort(pct.min.overlap,decreasing = FALSE))
    cat("\n")
    cat("Average Overlap (pct): ","\n")
    print(sort(pct.avg.overlap,decreasing = TRUE))
    
  })
  
  output$helpOut <- renderPrint({
    cat("Your input file should be a .csv file with 1s and 0s in the cells indicating a position in the security or not.","\n")
    cat("The column headings should be the fund names. The row names should be the security tickers or names.","\n")
    cat("\n")
    cat("The Total Number of Connections in the Statistics tab is the sum of all the overlapping positions plus the unique","\n")
    cat("positions a manager holds. For example, even with only 30 stocks, a manager might have more than 30 connections: 15 unique,","\n")
    cat("7 overlapping positions with manager X, 9 with manager Y, 11 with manager Z, and so on...","\n")
    cat("The total number of connections is the denominator for the percentage overlap calculations.","\n")
    cat("\n")
    cat("Percent Unique => Of the total number of connections, the number that are unique to the manager.","\n")
    cat("Percent Max Overlap => A ranking of the managers based on maximum percentage overlap.","\n")
    cat("Percent Min Overlap => A ranking of the managers based on minimum percentage overlap.","\n")
    cat("Percent Avg Overlap => A ranking of the managers based on average percentage overlap.","\n")
    cat("\n")
    cat("Percent Max or Percent Avg is usually the best way to visualize the Stock network graphic.")
    
  })
  
  output$networkPlot <- renderPlot({
    # Make the network plot based on user input.
    
    d <- chartData()
    gr <- getGraph()
    
    color.meaning <- getColorMeaning()
    
    cm <- switch(color.meaning,
      "Betweeness" = betweenness(gr),
      "Degree Centrality" = degree(gr, mode = "all"),
      "Closeness" = closeness(gr, mode = "all")
    )
    
    #Create appropriate color scales based on centrality scores...
    # Credits to OganM and joran stackoverflow.com...
    
    fine = 1000 # this will adjust the resolving power.
    pal = colorRampPalette(c('light blue','red'), alpha = FALSE)
    
    #this gives you the colors you want for every point
    graphCol = pal(fine)[as.numeric(cut(cm, breaks = fine))]
    
    graphCol <- addAlpha(graphCol, getAlpha())
    
    # Draw the network plot...
    la <- switch(input$userLayout,
               Auto = layout.auto(gr),
               Random = layout.random(gr),
               Circle = layout.circle(gr),
               Sphere = layout.sphere(gr),
               FruchtermanReingold = layout.fruchterman.reingold(gr),
               KamadaKawai = layout.kamada.kawai(gr))
    
    e.wt <- get.edge.attribute(gr, "weight")
    ec <- ifelse(input$userEdgeType == "Straight", FALSE, TRUE)
    
    if (!is.null(myaum) && input$userGraphType == "Fund View" && input$addAUMYesNo == TRUE) {
      vs <- abs(scale(as.numeric(myaum[,2]),center = 0)*15)
      print(vs)
    } else {
      vs <- getVertexSize()
    }
    
    plot(gr, 
         layout = la,
         vertex.size = vs,
         vertex.label.cex = input$labelCex,
         vertex.color = graphCol,
         vertex.label.color = rgb(0,0,0,max = 255),
         vertex.frame.color = "white",
         edge.curved = ec,
         edge.color = "darkgrey",
         edge.width = e.wt*1.50,
         vertex.label = V(gr)$name,# See ?V for more...
         main = ifelse(input$userGraphType == "Fund View", "Fund Network Mapping", "Stock Network Mapping")
    )
  })
})