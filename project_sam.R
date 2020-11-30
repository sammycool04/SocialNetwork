library(statnet)
library(igraph)

library(sna)

require(igraph)
require(sna)
require(statnet)
require(intergraph)
net.edge <- read.csv(file="ComplexFormaMentis/filteredStudent.csv")
student_class <- network(net.edge, matrix.type="edgelist")

net.node <- read.csv(file="ComplexFormaMentis/valenceStudent.csv")
summary(net.node$Attitude)



#basic information
summary(student_class)
op <- par(mar = c(0,0,0,0))

plot(student_class, vertex.cex = 1.5,
     displaylabels = T, cex = 0.8)
par(op)

network.size(student_class)
gden(student_class)
components(student_class,connected="weak")


diameter(asIgraph(student_class))

gtrans(student_class, mode = "graph")

student_class$Attitude <- as.character(net.node$Attitude)
summary(student_class%v%"Attitude")


#one way to assign corresponding attributes
student_net <- asIgraph(student_class)
V(student_netI)$attitude <- net.node$Attitude
summary(student_net$attitude)

#another way to assign corresponding attributes
V(student_net)$attitude <- sapply(V(student_net)$Word, function(x) net.node$Attitude[net.node$Word == x])







