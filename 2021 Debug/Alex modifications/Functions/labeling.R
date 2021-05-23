# Function to uniquely label nodes
# This function depends on an external 'ex_nodes' table of extracted node information from the graph object
# the external table ensures new labels inherit naming from older labels
node_labeller <- function(node, parent, ...){
  
  # Get type of current node
  cur.type <- .N()$type[node]
  
  # Get name of current node
  cur.name <- .N()$name[node]
  
  # Special condition for sink node
  if(cur.type == "Sink"){
    label <- "0"
    
    # Write to external table
    ex.nodes$label[which(ex.nodes$name == cur.name)] <<- label
    
    # Return label
    return(label)
  }
  
  # Get parent node name
  par.name <- .N()$name[parent] 
  
  # Get parent node label from external table
  par.label <- ex.nodes[ex.nodes$name == par.name,]$label
  
  # Check number of other nodes with same parent
  num.parents <- nrow(ex.nodes[ex.nodes$parent == par.name,])
  
  # Generate label
  label <- paste0(par.label, num.parents)
  
  # Write parent name to external table
  ex.nodes$parent[which(ex.nodes$name == cur.name)] <<- par.name
  
  # Write label to external table
  ex.nodes$label[which(ex.nodes$name == cur.name)] <<- label
  
  # Return label
  return(as.character(label))
}

# Function to uniquely label edges
# This function must run after the node labeller and simply joins the tables to retrieve upstream node labels
edge_labeller <- function(graph = NULL){
  
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
    left_join(select(nodes, label, row), by = c("to" = "row"))
  
  # Remove initia digit of label
  edges$label <- sub('.', '', edges$label)
  
  # Return label column
  return(edges$label)
  
}