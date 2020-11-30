pacman::p_load(tidyverse, sna, intergraph, GGally, Merritt, ergm, igraph, ggraph, tidygraph)

fm_edges <-  read_csv("~/CGU/Classes/Fall 2020/Social Networks/Project/FM Edgelist_student_filterd.csv")

fm_network <- network(fm_edges)
summary(fm_network)

fm_nodes <-  read_csv("~/CGU/Classes/Fall 2020/Social Networks/Project/FM_stundets_valence.csv")

network_names <- fm_network %v% 'vertex.names' # get list of names from the network
fm_nodes <- fm_nodes[ which(fm_nodes$Name %in% network_names), ] # 
fm_nodes <- fm_nodes[order(fm_nodes$Name),] # in case the column isn't ordered correctly
stopifnot(network_names == fm_nodes$Name) # Code will throw an error if something isn’t right

#The code to assign the Ideology attribute is as follows: 
fm_netI <- asIgraph(fm_network)
V(fm_netI)$valence <- fm_nodes$Valence

my_pallete <- c("blue", "red", "purple")


ideo <- as.factor(fm_nodes$Valence)

fm_net <- asNetwork(fm_netI)

plot(fm_net, mode="fruchtermanreingold",
     vertex.cex= log(degree(asIgraph(fm_net), mode= "in") + 1), vertex.col = my_pallete[ideo])
legend("bottomleft",legend=c("Liberal", "Conservative"),
       col=my_pallete,pch=19,pt.cex=1.5,bty="n",
       title="Ideology")

hist((degree(asIgraph(fm_net), mode = "in")))
 

##########################
# Get in-degree and clusters
###########################
set.seed(1)
degree <- degree(asIgraph(fm_net), mode = "in")
degree
 
clus_walk <- cluster_walktrap(asIgraph(fm_net))
clus_walk

clus_eb <- cluster_edge_betweenness(asIgraph(fm_net))
clus_eb

clus_fg <- cluster_fast_greedy(asIgraph(fm_net))

clus_sg <- cluster_spinglass(asIgraph(fm_net))
clus_sg$membership

V(fm_netI)$group <- as.factor(clus_sg$membership)
V(fm_netI)$degree <- degree


routes_igraph_tidy <- as_tbl_graph(fm_netI)

###############################################################################
# Looking into differences between groups
#
#
################################################################################




group_degree <- routes_igraph_tidy %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  group_by(group) %>% 
  summarise(degree_mean = mean(degree, na.rm = T), max_degree = max(degree), min_degree = min(degree), median_degree = median(degree))

group_valence <- routes_igraph_tidy %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  group_by(group, valence) %>% 
  summarise(count = n())

View(group_degree)

View(group_valence)

valence <- group_valence %>%
  pivot_wider(names_from = valence, values_from = count) %>% 
  mutate(total = Negative + Neutral + Positive,
         Positive = Positive/total,
         Negative = Negative/total,
         Neutral = Neutral/total)
view(valence)


routes_igraph_tidy %>% 
  activate(nodes) %>% 
  as_tibble() %>% 
  jmv::ANOVA(dep = degree, factors = list('valence'))




