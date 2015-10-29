# Global definitions for Network Analysis Shiny App
# global.R
# Adam Duncan 2014

library(igraph)
#library(rgl)
library(plyr)
library(reshape2)
library(scales)
library(devtools)
library(shiny)
library(shinyBS)

# Helper function defs...
prettyScores<-function(x){
  # Takes a vector of centrality scores and prints the vector in 
  # a reader friendly format.
  for (i in 1:length(x)){
    cat(names(x)[i]," ",round(x[i],3),"\n")
  }
}
add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}


# Lists...

graphTypes<-list(
  "Stock View",
  "Fund View")
  

colorTypes<-list(
  "Percent Unique",
  "Percent Max Overlap",
  "Percent Min Overlap",
  "Percent Avg Overlap")

layoutTypes<-list(
  "Auto",
  "Random",
  "Circle",
  "Sphere",
  "FruchtermanReingold",
  "KamadaKawai",
  "Spring")

edgeTypes<-list(
  "Curved",
  "Straight")

