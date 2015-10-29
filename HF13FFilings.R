### Data on long/short equity fund holdings.
### Code adapted from kjhealy's blog post on identifying
### Paul Revere via metadata.
### Karl U. Wichorek 9/9/2013

library(igraph)
data <- as.matrix(read.csv("13FFilingsV2.csv",row.names=1))

ticker.net <- data %*% t(data)
fund.net <- t(data) %*% data

diag(fund.net) <- NA
diag(ticker.net) <- NA

ticker.g <- graph.adjacency(ticker.net,mode="undirected", weighted=NULL, diag=FALSE)

fund.g <- graph.adjacency(fund.net, weighted=TRUE, mode="undirected", diag=FALSE)

la.f <- layout.fruchterman.reingold(fund.g)
e.wt.f <- get.edge.attribute(fund.g, "weight")

png(file="figures/fund-view.png", width=1000, height=1000, res=150)
plot(fund.g, layout=la.f, vertex.size=15,edge.width=e.wt.f, vertex.label=V(fund.g)$name)

la.t <- layout.fruchterman.reingold(ticker.g)
e.wt.t <- get.edge.attribute(ticker.g, "weight")

png(file="figures/ticker-view.png", width=1000, height=1000, res=150)
plot(ticker.g, layout=la.t, vertex.size=15,edge.width=e.wt.t, vertex.label=V(ticker.g)$name)