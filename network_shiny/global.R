# Global definitions for Network Analysis Shiny App
# global.R
# Adam Duncan 2014

library(igraph)
library(plyr)
library(reshape2)
library(scales)
library(shiny)
library(shinyBS)
library(shinythemes)
# library(dMisc)

# Helper function defs...
prettyScores <- function(x){
  # Takes a vector of centrality scores and prints the vector in 
  # a reader friendly format.
  for (i in 1:length(x)) {
    cat(names(x)[i]," ",round(x[i],3),"\n")
  }
}

# thanks to Markus Gesman (magesblog.com)
addAlpha <- function(col, alpha=1){
  if (missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha = alpha))  
}


# Lists...

graphTypes <- list(
  "Stock View",
  "Fund View")
  

colorTypes <- list(
  "Betweeness",
  "Degree Centrality",
  "Closeness")

layoutTypes <- list(
  "Auto",
  "Random",
  "Circle",
  "Sphere",
  "FruchtermanReingold",
  "KamadaKawai")

edgeTypes <- list(
  "Curved",
  "Straight")

