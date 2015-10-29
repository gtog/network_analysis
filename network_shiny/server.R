# Basic Shiny App
# server.R
# Adam Duncan, 2013

# Load libraries...
library(shiny)
library(igraph)
#library(rgl)
library(plyr)
library(reshape2)
library(scales)
library(devtools)

shinyServer(function(input,output,session){
  
  observe({
    inFile<-input$userfile
    ifelse(is.null(inFile),return(NULL),mydata <- read.csv(inFile$datapath, header=TRUE, sep=","))
    
    updateSelectInput(session, "userVertexColor", choices = colorTypes,selected=colorTypes[1])
    
    updateSelectInput(session, "userLayout", choices = layoutTypes,selected="Auto")
    addTooltip(session, id="userLayout", title = "Define the layout for the graph.",placement = "right", trigger = "hover")
    
    updateNumericInput(session,"userNumHoldings",value=nrow(mydata),min=0,max=nrow(mydata))
    
    updateRadioButtons(session, inputId="userGraphType", choices=graphTypes,selected="Fund View")
    addTooltip(session, id="userGraphType", title = "Select Fund or Stock view.",placement = "right", trigger = "hover")
    
    updateRadioButtons(session, inputId="userEdgeType", choices=edgeTypes,selected="Straight")
    addTooltip(session, id="userEdgeType", title = "Select curved or straight edges.",placement = "right", trigger = "hover")
    
    updateSliderInput(session,inputId="alpha")
    addTooltip(session, id="alpha", title = "Slider to adjust transparency of nodes.",placement = "right", trigger = "hover")
    
    updateSliderInput(session,inputId="labelCex")
    addTooltip(session, id="labelCex", title = "Slider to adjust size of label text.",placement = "right", trigger = "hover")
    
    updateSliderInput(session,inputId="vertexSize")
    addTooltip(session, id="vertexSize", title = "Slider to update size of nodes.",placement = "right", trigger = "hover")
  })
  
  # First, create a reactive function to hold all the chart data that is sensitive to user input.
  chartData<-reactive({
    # Reactive funtion that returns a list of variables sensitive to user changes.
    # The list contains all the reactive components used in the various charting routines.
    # Note, this returns ALL of the data. The slider input changes are reflected at the panel level.
    
    inFile<-input$userfile
    if(is.null(inFile)) {return(NULL)}
    mydata = read.csv(inFile$datapath, quote="",row.names=1)
    
    # Clean up the data a bit...
    stock.holdings<-as.matrix(mydata)
    data<-stock.holdings #stock_holdings
    data[is.na(data)]<-0 # Make sure any NA's are set to zeros.
    temp<-apply(data,MARGIN=1,FUN=sum)
    data.adj<-cbind(data,temp)
    data.adj<-data.adj[order(-temp),] #sort descending on most common positions
    data<-data.adj[,-length(colnames(data.adj))] #drop the sum column we just added...
    data<-data[1:input$numHoldings,] # Just the top x holdings please...
    
    #Create the networks from the matrix of data we imported...
    stock.net <- data %*% t(data) # Multiply the data matrix with it's transpose. ie. make the network (adjacency matrix)...
    fund.net <- t(data) %*% data  # Multiply the transpose with the data matrix. 
    #diag(stock.net) <- NA # set the diagonals to NA
    #diag(fund.net) <- NA
    
    # Create the graphs...
    stock.g <- graph.adjacency(stock.net,mode="undirected",weighted=TRUE,diag=FALSE)
    fund.g <- graph.adjacency(fund.net, weighted=TRUE,mode="undirected", diag=FALSE)
    delete.vertices(stock.g, which(degree(stock.g)==0))
    delete.vertices(fund.g, which(degree(fund.g)==0))
    
    # Now create the list that holds all the variables and return...
    return(list(
      thedata=data, #1
      num.holdings<-input$num_holdings, #2
      stockGraph<-stock.g, #3
      fundGraph<-fund.g, #4
      stockNetwork<-stock.net, #5
      fundNetwork<-fund.net #6
      ))
  })
  
  getGraph<-reactive({
    g<-input$userGraphType
    d<-chartData()
    gr<-NULL
    
    if(length(d)==0){
      return(0)
    } else {
      ifelse(g=="Fund View",gr<-d[[4]],gr<-d[[3]])
      return(gr)
    }
  })
  
  getNetwork<-reactive({
    g<-input$userGraphType
    d<-chartData()
    net<-NULL
    
    ifelse(g=="Fund View",net<-d[[6]],net<-d[[5]])
    return(net)
  })
  
  getAlpha<-reactive({
    a<-input$alpha
    return(a)
  })
  
  getVertexSize<-reactive({
    v<-input$vertexSize
    return(v)
  })
  
  # Reactive function to respond to user Centrality measure selection...
  getColorMeaning<-reactive({
    a<-input$userVertexColor
    gr<-getGraph()
    net<-getNetwork()
    
    
    total.posn<-apply(net,1,sum)
    unique.posn<-diag(net)
    tot.posn.matrix<-matrix(rep(t(total.posn),nrow(net)),ncol=ncol(net))
    pct.overlap.matrix<-net/tot.posn.matrix # The rows here represent the amount of overlap between the managers. 
    pct.unique<-diag(pct.overlap.matrix) 
    temp<-pct.overlap.matrix
    diag(temp)<-0
    pct.max.overlap<-apply(temp,1,max)
    pct.min.overlap<-apply(temp,1,min)
    pct.avg.overlap<-apply(temp,1,mean)
    
    ret<-switch(a,
                "Percent Unique"=sort(pct.unique,decreasing=TRUE),
                "Percent Max Overlap"=sort(pct.max.overlap,decreasing=TRUE),
                "Percent Min Overlap"=sort(pct.min.overlap,decreasing=TRUE),
                "Percent Avg Overlap"=sort(pct.avg.overlap,decreasing=TRUE))
    #print(ret)
    return(ret)
  })
  
  output$alphaSlider <- renderUI({
      sliderInput("alpha","Alpha:",min=0,max=1,value=.80,step=.1,format="#.##",animate=FALSE)
  })
  
  output$labelCexSlider <- renderUI({
            sliderInput("labelCex","Label Size:",min=.5,max=2.0,value=1,step=.1,format="#.##",animate=FALSE)
  })
  
  output$vertexSizeSlider <- renderUI({
    sliderInput("vertexSize","Vertex Size:",min=5,max=50,value=20,step=5,format="#",animate=FALSE)
  })
  
  output$thedata<-renderDataTable({
    if (is.null(input$userfile)) { return() } 
    else {
      read.csv(input$userfile$datapath)
    }
  })
  
  output$statsOut<-renderPrint({
    d<-chartData()
    gr<-getGraph()
    net<-getNetwork()
    
    
    total.posn<-apply(net,1,sum)
    unique.posn<-diag(net)
    tot.posn.matrix<-matrix(rep(t(total.posn),nrow(net)),ncol=ncol(net))
    pct.overlap.matrix<-net/tot.posn.matrix # The rows here represent the amount of overlap between the managers. 
    pct.unique<-diag(pct.overlap.matrix) 
    temp<-pct.overlap.matrix
    diag(temp)<-0
    pct.max.overlap<-apply(temp,1,max)
    diag(temp)<-1
    pct.min.overlap<-apply(temp,1,min)
    diag(temp)<-NA
    pct.avg.overlap<-apply(temp,1,mean,na.rm=TRUE)
    
    cat("Total Number of Connections in Network: ","\n")
    print(total.posn)
    cat("\n")
    cat("Percent Overlap Matrix (Read by row only. Diagonal = Unique %): ","\n")
    print(pct.overlap.matrix)
    cat("\n")
    cat("Maximum Overlap (pct): ","\n")
    print(sort(pct.max.overlap,decreasing=TRUE))
    cat("\n")
    cat("Minimum Overlap (pct): ","\n")
    print(sort(pct.min.overlap,decreasing=FALSE))
    cat("\n")
    cat("Average Overlap (pct): ","\n")
    print(sort(pct.avg.overlap,decreasing=TRUE))
    
  })
  
  output$helpOut<-renderPrint({
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
  output$networkPlot<-renderPlot({
    ### Begin Chart Code
    gr<-getGraph()
    color.meaning<-getColorMeaning()
    
    #Create appropriate color scales based on centrality scores...
    meaning.scale<-rescale(color.meaning, to = c(0, 1), from = range(color.meaning, na.rm = TRUE))
    col.scale<-cscale(meaning.scale, seq_gradient_pal("light green", "red"))
    col.scale<-add.alpha(col.scale,alpha=input$alpha)
    col.scale<-col.scale[match(V(gr)$name,names(col.scale))]
    
    # Draw the network plot...
    la<-switch(input$userLayout,
               Auto = layout.auto(gr),
               Random = layout.random(gr),
               Circle = layout.circle(gr),
               Sphere = layout.sphere(gr),
               FruchtermanReingold = layout.fruchterman.reingold(gr),
               KamadaKawai = layout.kamada.kawai(gr),
               Spring = layout.spring(gr))
    
    e.wt <- get.edge.attribute(gr, "weight")
    ec<-ifelse(input$userEdgeType=="Straight",FALSE,TRUE)
    
    plot(gr, 
         layout=la,
         vertex.size=input$vertexSize, #15 default
         vertex.label.cex=input$labelCex,
         vertex.color=col.scale,
         vertex.label.color=rgb(0,0,0,max=255),
         vertex.frame.color=NA,
         edge.curved=ec,
         edge.color="darkgrey",
         edge.width=e.wt*1.50,
         vertex.label=V(gr)$name,# See ?V for more...
         main=ifelse(input$userGraphType=="Fund View","Fund Network Mapping","Stock Network Mapping")
    )
  })
})