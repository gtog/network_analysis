# Network Analysis Example of Hedge Fund Holdings
library(igraph)
library(rgl)
library(plyr)
library(reshape2)
library(scales)
library(devtools)
install_github('arcdiagram', username = 'gastonstat')
library(arcdiagram)

# Helper function defs...
prettyScores<-function(x){
  # Takes a vector of centrality scores and prints the vector in 
  # a reader friendly format.
  for (i in 1:length(x)){
    cat(names(x)[i]," ",round(x[i],3),"\n")
  }
}

# Data...
stock_holdings <- as.matrix(read.csv("~/Network Analysis/13FFilingsV3_top10.csv", quote="",row.names=1))

ca_exposure<-read.csv("~/Network Analysis/ca_exposure_LS.csv")
data.exp<-ca_exposure[order(-ca_exposure$ca.inv.amt),]
data.exp<-data.exp[1:10,]

#ytd_perf_LS <- read.csv("~/Network Analysis/ytd_perf_LS.csv")

# Some other variables of interest...(Skip these if you have not collected the data)
#firm_size<-c(1769,3805,2559,6313,2812,2294)
#num.clients<-top10$num.clients[1:10] #ca_exposure$num.clients#top10$num.clients[1:10] 
inv.amt<-data.exp$ca.inv.amt #ca_exposure$ca.inv.amt
#perf<-as.vector(apply(ytd_perf_LS,MARGIN=1,function(x) abs(x*100)))

# Sort the data according to largest positions and trim for say, the top x pos.
data<-stock_holdings #stock_holdings
data[is.na(data)]<-0 # Make sure any NA's are set to zeros.
#rownames(data)<-data$ticker # Make sure if you have long stock names to cut them.
temp<-apply(data,MARGIN=1,FUN=sum)
data.adj<-cbind(data,temp)
data.adj<-data.adj[order(-temp),] #sort descending on most common positions
data<-data.adj[,-length(colnames(data.adj))] #drop the sum column
#data<-data[1:50,] # Just the top x holdings please...

#Create the networks from the matrix of data we imported...
stock.net <- data %*% t(data) # Multiply the data matrix with it's transpose. ie. make the network (adjacency matrix)...
fund.net <- t(data) %*% data  # Multiply the transpose with the data matrix. 
total.posn<-apply(fund.net,1,sum)
unique.posn<-diag(fund.net)
tot.posn.matrix<-matrix(rep(t(total.posn),nrow(fund.net)),ncol=ncol(fund.net))
pct.overlap.matrix<-fund.net/tot.posn.matrix # The rows here represent the amount of overlap between the managers. 
pct.unique<-diag(pct.overlap.matrix) 

#diag(stock.net) <- NA # set the diagonals to NA
#diag(fund.net) <- NA

# Create the graphs...
stock.g <- graph.adjacency(stock.net,mode="undirected",weighted=TRUE,diag=FALSE)
fund.g <- graph.adjacency(fund.net, weighted=TRUE,mode="undirected", diag=FALSE)

# Calculate betweeness measure for stock holdings:
gr<-fund.g
gr<-delete.vertices(gr, which(degree(gr)==0)) #remove vertices with no edges. Usually relevant for stock.gr

## Betweeness Centrality
b<-betweenness(gr, v=V(gr), directed = TRUE, weights = NULL,
               nobigint = TRUE, normalized = FALSE)
names(b) <- V(gr)$name
b<-sort(b,decreasing=TRUE)
prettyScores(b)

# Degree centrality
d<-degree(gr, v=V(gr), mode = "all", loops = TRUE, normalized = FALSE)
names(d) <- V(gr)$name
d<-sort(d,decreasing=TRUE)
prettyScores(d)

## Eigenvector centrality
cent.eig <- evcent(gr)
names(cent.eig$vector) <- V(gr)$name
ind <- order(-cent.eig$vector)
ce<-round(cent.eig$vector[ind],3)
ce<-sort(ce,decreasing=TRUE)
prettyScores(ce)

## Closeness centrality
cl<-closeness(gr, vids=V(gr), mode = c("all"),
              weights = NULL, normalized = FALSE) #c("out", "in", "all", "total")
names(cl)<-V(gr)$name
cl<-sort(cl,decreasing=TRUE)
prettyScores(cl)


#Create appropriate color scales based on centrality scores...
cen.scale<-rescale(cl, to = c(1, 0), from = range(cl, na.rm = TRUE))
unique.scale<-rescale(unique.posn,to=c(1,0),from=range(unique.posn,na.rm=TRUE))

col.scale<-cscale(cen.scale, seq_gradient_pal("red", "light grey"))
unique.col.scale<-cscale(unique.scale,seq_gradient_pal("red","light green"))

# Draw the network plot...
# Push the graphs to a png or pdf file. We can turn that off and push to current graphics device.
# la <- layout.auto(gr)#layout.fruchterman.reingold(gr),# #layout.auto(gr) # Set the layout
la<-layout.auto(gr)
e.wt <- get.edge.attribute(gr, "weight") # Set the edge width for the graph...
#png(file="~/Network Analysis/figures/fund-view.png", width=1000, height=1000, res=150)

# Edit these plot parameters to suit the graph you are trying to draw.
plot(gr, 
     layout=la,
     vertex.size= 15,#abs(scale(inv.amt,center=0)*20), #15 default
     vertex.label.cex=1.0,
     vertex.color=unique.col.scale,#"light blue",
     vertex.label.color=rgb(0,0,0,max=255),
     vertex.frame.color=NA, #rgb(0,0,0,max=255),
     edge.curved=FALSE,
     edge.color="darkgrey",
     edge.width=e.wt*1.75,
     vertex.label=V(gr)$name,# See ?V for more...
     main="Firm Network Analysis",
     #sub="Size of circle is proportional to the number of managers holding positions.",
     )
#dev.off()

################# Experimental stuff below #####################

#Creating an Arc diagram from the network...

# get edgelist
edgelist = get.edgelist(gr)
vlabels = get.vertex.attribute(gr, "label")
values = get.edge.attribute(gr, "value")

arcplot(edgelist, 
        V(gr)$name, 
        sorted = FALSE,
        decreasing = FALSE, 
        ordering = NULL, 
        labels = vlabels,
        horizontal = TRUE, 
        above = NULL,
        col.arcs = "light grey", 
        lwd.arcs = 1*e.wt, 
        lty.arcs = 1,
        lend = 1, 
        ljoin = 2, 
        lmitre = 1, 
        show.nodes = TRUE,
        pch.nodes = 19, 
        cex.nodes = 1, 
        col.nodes = "light blue",
        bg.nodes = "gray80", 
        lwd.nodes = 1, 
        show.labels = TRUE,
        col.labels = "gray55", 
        cex.labels = 0.9, 
        las = 2,
        font = 1, 
        line = 0, 
        outer = FALSE, 
        adj = NA, 
        padj = NA,
        axes = FALSE)

# 3D Plot?

# Integrate this code...
small.frys <- which(V(g)$coreness.all < 4)
g <- delete.vertices(g, small.frys)

gr<-fund.g
la.3<-layout.fruchterman.reingold(gr, dim=3)
e.wt <- get.edge.attribute(gr, "weight") # Set the edge width for the graph...
#png(file="~/Network Analysis/figures/fund-view.png", width=1000, height=1000, res=150)

# Edit these plot parameters to suit the graph you are trying to draw.
vsize<-abs(scale(inv.amt,center=0)*35)

rgl.bg(color="white")
rglplot.igraph(gr, 
     layout=la.3,
     vertex.size=vsize,
     vertex.color="white",
     #label.color=rgb(0,0,0,max=255),
     #frame.color=NA, #rgb(0,0,0,max=255),
     #shape=c("sphere"),
     vertex.label.dist=1,
     edge.curved=.8,
     edge.color="red",
     edge.width=e.wt*2.0)
     #vertex.label=V(gr)$name)# See ?V for more...
     #main="Firm Network Analysis",
     #sub="Size of circle is proportional to the number of managers holding positions.",
)

writeSTL("toptenlongshort_thick.stl")
