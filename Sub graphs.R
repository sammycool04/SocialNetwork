edges <- routes_igraph_tidy %>% 
  activate(edges) %>% 
  as_tibble() %>% 
  group_by(from,to) %>%
  mutate(weight = n()) 

nodes <- routes_igraph_tidy %>% 
  activate(nodes) %>% 
  as_tibble()

tidy_net <- tbl_graph(nodes = nodes, edges = edges)

filtered_graph <- function(data, name, listOfNames){
  
  list <- listOfNames
  list_number <- match(name, list)
  
  edges1 <- data %>% 
    activate(edges) %>% 
    as_tibble() %>% 
    filter(to == list_number) %>% 
    head()
  
  edges2 <-  data %>% 
    activate(edges) %>% 
    as_tibble() %>% 
    filter(from == list_number) %>% 
    head()
  
  numbers <- c(edges1$from, edges2$to)
  
  data %>% 
    activate(nodes) %>% 
    filter(vertex.names %in% c(list[numbers], name)) %>% 
    ggraph( layout = "linear", circular = T) + 
    geom_edge_arc(alpha = 0.8) + 
    scale_edge_width(range = c(0.2, 2)) +
    geom_node_text(aes(label = vertex.names, color = valence, size = degree+1)) +
    theme_graph()+
    theme(legend.position = "none")
  
}

filtered_graph(tidy_net, "matematica", V(fm_netI)$vertex.names)

filtered_graph(tidy_net, "fisica", V(fm_netI)$vertex.names)

filtered_graph(tidy_net, "tecnologia", V(fm_netI)$vertex.names)

filtered_graph(tidy_net, "scienza", V(fm_netI)$vertex.names)
