#!/usr/bin/Rscript

#Visualizing networking data

#Load the data
edges = read.csv('edges.csv')
users = read.csv('users.csv')

library(igraph)

#Creating a network
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
sort(degree(g))
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

#Coloring vertices
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

