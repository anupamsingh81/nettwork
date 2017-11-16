library(devtools)
install_github("DougLuke/UserNetR")
library(UserNetR)
data("Moreno")
data(Bali)

help(package='UserNetR')

library(statnet)
library(igraph)
library(tidyverse)

network.size(Moreno)

?network.size

a = read.table(text= "node1,node2
551,548
510,512
548,553
505,504
510,512
552,543
512,510
512,510
551,548
548,543
543,547
543,548
548,543
548,542",header=TRUE,sep=",")

names(a)= c("source","target")

g <- igraph::graph.data.frame(a, directed=TRUE)
plot(g)

netmat = as_adjacency_matrix(g)

b=as.matrix(as_adjacency_matrix(g))


net1 <- network(b,matrix.type="adjacency")
class(net1)

summary(net1)

#size
network.size(net1)
#density
network.density(net1)

#clustering
gtrans(net1,mode = "graph")

lgc <- component.largest(net1,result="graph")
gd <- geodist(lgc)
gd
max(gd$gdist)
components(netmat)

## ecneph
edges = edge %>% select(-X1)
unique(edges$source)
aa =unique(edges$target)
patt = "http|https|pic|7|10pm|MrpVankalakunti|AMCMA6"
library(stringr)
bb = aa[!str_detect(aa,patt)]

edges1 = edges %>% filter(target%in%bb)

g1 <- igraph::graph.data.frame(edges1, directed=TRUE)
plot(g1)

netmat = as_adjacency_matrix(g)

b1=as.matrix(as_adjacency_matrix(g1))


net2 <- network(b1,matrix.type="adjacency")
class(net2)

summary(net2)

#size
network.size(net2)
#density
network.density(net2)

#clustering
gtrans(net2,mode = "graph")

# diameter
lgc <- component.largest(net2,result="graph")
gd <- geodist(lgc)
gd
max(gd$gdist)


components(netmat)

gplot(net2, vertex.col = 2, displaylabels = TRUE)

edges2 = as.matrix(edges1)

net3<- network(edges2,matrix.type="edgelist")
class(net3)
summary(net3)

#size
network.size(net3)
#density
network.density(net3)

#clustering
gtrans(net3,mode = "graph")

gplot(net3, vertex.col = 2, displaylabels = TRUE)


