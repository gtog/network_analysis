# Network Analysis Example of Hedge Fund Holdings
library(igraph)
library(dplyr)
library(reshape2)
library(scales)
library(magrittr)

# Data...
stock_holdings <- read.csv("~/Network Analysis/data/ca_hf13F_fall2015.csv", quote = "",row.names = 1)

# ca_exposure <- read.csv("~/Network Analysis/ca_exposure_LS.csv")
# ytd_perf_LS <- read.csv("~/Network Analysis/ytd_perf_LS.csv")

# Some other variables of interest...
# firm_size <- c(1769,3805,2559,6313,2812,2294)
# num.clients <- top10$num.clients[1:10] #ca_exposure$num.clients#top10$num.clients[1:10] 
# inv.amt <- ca_exposure$ca.inv.amt #ca_exposure$ca.inv.amt
# perf <- as.vector(apply(ytd_perf_LS,MARGIN = 1, function(x) abs(x*100)))

# Sort the data according to largest positions and trim for say, the top x pos.
use_holdings <- 20
data <- stock_holdings

temp <- apply(data, MARGIN = 1, FUN = sum)

data.adj <- cbind(data,temp)
data.adj <- data.adj[order(-temp),] #sort descending on most common positions
data <- subset(data.adj, select = c(-temp)) #drop the sum column

data <- data[1:use_holdings, ] # Just the top x holdings please...

#Create the networks from the matrix of data we imported...

data <- as.matrix(data)
stock.net <- data %*% t(data) # Multiply the data matrix with it's transpose. ie. make the network (adjacency matrix)...
fund.net <- t(data) %*% data  # Multiply the transpose with the data matrix. 

diag(stock.net) <- NA # set the diagonals to NA
diag(fund.net) <- NA

# Create the graphs...
stock.g <- graph.adjacency(stock.net, mode = "undirected", weighted = NULL, diag = FALSE)
fund.g <- graph.adjacency(fund.net, weighted = NULL, mode = "undirected", diag = FALSE)

# Calculate betweeness measure for stock holdings:
# Draw the network plot...
gr <- stock.g %>% 
  delete.vertices(which(degree(.) == 0)) #remove vertices with no edges. Usually relevant for stock.gr

# Push the graphs to a png or pdf file. We can turn that off and push to current graphics device.
# layout.fruchterman.reingold(gr),# #layout.auto(gr) # Set the layout
la <- layout.auto(gr)
e.wt <- get.edge.attribute(gr, "weight") # Set the edge width for the graph...
#png(file="~/Network Analysis/figures/fund-view.png", width=1000, height=1000, res=150)

# Betweeness Centrality
b <- betweenness(gr, v = V(gr), directed = TRUE, weights = NULL,
               nobigint = TRUE, normalized = FALSE)
names(b) <- V(gr)$name
ind <- order(-b) #sort desceding
b[ind][1:10] #Top 10 positions on betweeness measure (most shortest edges)

# Degree centrality
d <- degree(gr, v = V(gr), mode = "all", loops = TRUE, normalized = FALSE)
names(d) <- V(gr)$name

# Eigenvector centrality
cent.eig <- evcent(gr)
names(cent.eig$vector) <- V(gr)$name
ind <- order(-cent.eig$vector)
round(cent.eig$vector[ind][1:10],3)
ce <- round(cent.eig$vector[ind][1:10],3)

# Closeness centrality
cl <- closeness(gr, vids = V(gr), mode = c("all"),
              weights = NULL, normalized = FALSE) #c("out", "in", "all", "total")
names(cl) <- V(gr)$name
cl <- sort(-cl)
cl[1:10]


#Create appropriate color scales based on centrality scores...
cen.scale <- rescale(d, to = c(1, 0), from = range(d, na.rm = TRUE))
col.scale <- cscale(cen.scale, seq_gradient_pal("red", "light grey"))
#ramp<-colorRamp(c("red","white"))
#mycol<-rgb( ramp(cen.scale), max = 255)

# Edit these plot parameters to suit the graph you are trying to draw.
plot(gr, 
     layout = la,
     vertex.size = 15,#abs(scale(firm_size,center=0)*15), #15 default
     vertex.label.cex = .6,
     vertex.color = col.scale,#rgb(204,204,255,max=255),#col.scale,rgb(ramp(seq(0,1,length=30)),max=255), #204,204,255,max=255),
     vertex.label.color = rgb(0,0,0,max = 255),
     vertex.frame.color = NA, #rgb(0,0,0,max=255),
     edge.curved = FALSE,
     edge.color = "darkgrey",
     edge.width = e.wt,
     vertex.label = V(gr)$name,# See ?V for more...
     main = "Top 20 Holings"
     #sub="Size of circle is proportional to the number of managers holding positions.",
     )
#dev.off()