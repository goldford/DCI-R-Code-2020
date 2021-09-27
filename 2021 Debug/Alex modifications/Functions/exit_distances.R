get_exit_distances <- function(network = NULL, seg.edges = NULL, seg.dist.perm = NULL){
  
apply(network %>% activate(nodes) %>% data.frame(), 1, FUN = node_to_exit, seg.edges)
  
}

node_to_exit <- function(node, seg.edges){
  
  # Gather segment edge nodes for given node
  cur.edges <- 
  
}
