# This function takes as input a FIPEX table which includes from and to nodes as well as the distance of each segment
# There is also a direction argument which specifies how to code direction on the directed network
#     By default the direction of edges is bidirectional
#     "flow" will return an adjacency matrix where directionality is same as flow
#     "anti-flow" will return an adjacency matrix where directionality is opposite to flow

adj_weighted <- function(FIPEX_table = NULL, direction = NULL){
  
  # Save sink row and set to 1
  FIPEX_sink_row <- FIPEX_table %>%
    mutate(NodeEID = NodeEID + 1) %>%
    filter(DownstreamEID %in% "sink") %>%
    mutate(DownstreamEID = 1)
  
  # Increment IDs by 1 for graphing purposes
  FIPEX_edgelist <- FIPEX_table %>%
    filter(!(DownstreamEID %in% "sink")) %>%
    mutate(NodeEID = NodeEID + 1) %>%
    mutate(DownstreamEID = as.numeric(DownstreamEID) + 1) %>%
    bind_rows(FIPEX_sink_row) %>%
    select(NodeEID, DownstreamEID, DownstreamNeighDistance) %>%
    rename(from = NodeEID, to = DownstreamEID, weight = DownstreamNeighDistance)
  
  # Get connectivity list & convert columns to character
  FIPEX_edgelist <- FIPEX_edgelist %>%
    select(from, to, weight) %>%
    mutate(from = as.character(from)) %>%
    mutate(to = as.character(to)) %>%
    mutate(to = ifelse(to == "Sink", "sink", to))
  
  # Store downstream neighbours
  # This is the default relationship stored in the table
  edges_down <- FIPEX_edgelist
  
  # Store upstream neighbours
  # This requires flipping DownstreamEID and NodeEID
  edges_up <- FIPEX_edgelist %>%
    relocate(to) %>%
    rename(from = to, to = from)
  
  # Specify direction of edges
  if(direction == "flow"){
    edges_up <- edges_up %>%
      mutate(weight = 0)
  }
  
  if(direction == "anti-flow"){
    edges_down <- edges_down %>%
      mutate(weight = 0)
  }
  
  # Merge neighbourhood lists
  edges_all <- edges_down %>%
    bind_rows(edges_up)
  
  # Convert to matrix
  adj_weight <- with(edges_all, tapply(weight, list(from, to), FUN = sum, default = 0))
  
  return(adj_weight)
}