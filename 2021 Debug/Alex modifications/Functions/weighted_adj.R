# This function takes as input a FIPEX table which includes from and to nodes as well as the distance of each segment
# There is also a direction argument which specifies how to code direction on the directed network
#     By default the direction of edges is bidirectional
#     "flow" will return an adjacency matrix where directionality is same as flow
#     "anti-flow" will return an adjacency matrix where directionality is opposite to flow
# Default direction is anti-flow as this produces a topologically correct tree network

adj_weighted <- function(FIPEX.table = NULL, direction = "anti-flow"){
  
  # Save sink row and set to 1
  FIPEX.sink.row <- FIPEX.table %>%
    filter(DownstreamEID %in% "sink") %>%
    mutate(NodeEID = NodeEID + 1) %>%
    mutate(DownstreamEID = 1)
  
  # Create edge list
  FIPEX.edgelist <- FIPEX.table %>%
    # Remove sink row
    filter(!(DownstreamEID %in% "sink")) %>%
    # Increment all labels by 1 to avoid duplicate node IDs
    mutate(NodeEID = NodeEID + 1) %>%
    mutate(DownstreamEID = as.numeric(DownstreamEID) + 1) %>%
    # Add reserved sink row
    bind_rows(FIPEX.sink.row) %>%
    select(NodeEID, DownstreamEID, DownstreamNeighDistance) %>%
    rename(from = NodeEID, to = DownstreamEID, weight = DownstreamNeighDistance) %>%
    # Do some type conversions
    mutate(from = as.character(from)) %>%
    mutate(to = as.character(ifelse(to == "Sink", "sink", to)))
  
  # Store downstream neighbors
  # This is the default relationship stored in the table
  edges.down <- FIPEX.edgelist
  
  # Store upstream neighbors
  # This requires flipping DownstreamEID and NodeEID
  edges.up <- FIPEX.edgelist %>%
    relocate(to) %>%
    rename(from = to, to = from)
  
  # Specify direction of edges
  if(direction == "flow"){
    edges.up <- edges.up %>%
      mutate(weight = 0)
  }

  if(direction == "anti-flow"){
    edges.down <- edges.down %>%
      mutate(weight = 0)
  }
  
  # Merge neighborhood lists
  edges.all <- edges.down %>%
    bind_rows(edges.up)
  
  # Convert to matrix
  adj_weight <- with(edges.all, tapply(weight, list(from, to), FUN = sum, default = 0))
  
  # This manipulation of the upper and lower triangle should work I think but it doesn't at the moment
  #
  # if(direction == "flow"){
  #   adj_weight[upper.tri(adj_weight)] <- 0
  # }
  # 
  # if(direction == "anti-flow"){
  #   adj_weight[lower.tri(adj_weight)] <- 0
  # }
  
  return(adj_weight)
}