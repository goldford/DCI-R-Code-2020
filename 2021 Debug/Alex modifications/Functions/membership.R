# Node membership function
node_membership <- function(node, parent, ...){
  
  # When visitng the first (sink) node
  if(.N()$type[node] == "Sink"){
    membership <- "0"
    ex.nodes$membership[which(ex.nodes$name == .N()$name[node])] <<- membership
    return(membership)
  }
  
  # If the previous node is a sink or branch junction label with a new ID
  if(.N()$type[parent] %in% c("Sink", "Branch Junction")){
    
    # Get parent's membership from external table
    membership <- ex.nodes$membership[which(ex.nodes$name == .N()$name[parent])]
    
  }
  
  # If the previous node is a barrier then use the parent's ID
  if(.N()$type[parent] == "Barrier"){
    
    # Retrieve unique label
    membership <- member.labels[1]
    
    # Remove used label from available labels
    member.labels <<- member.labels[2:length(member.labels)]
    
  }
  
  # Write membership of current node to external table
  ex.nodes$membership[which(ex.nodes$name == .N()$name[node])] <<- membership
  
  # Return membership ID
  return(as.character(membership))
  
}

# Edge membership function
edge_membership <- function(graph = NULL){
  
  # Extract nodes
  nodes <- graph %>%
    activate(nodes) %>%
    data.frame() %>%
    # Add field containing row index
    mutate(row = 1:n())
  
  # Extract edges
  edges <- graph %>%
    activate(edges) %>%
    data.frame()
  
  # Join node table ('row') to edge table ('to')
  edges <- edges %>%
    left_join(select(nodes, membership, row), by = c("to" = "row"))
  
  # Return membership vector
  return(edges$membership)
}