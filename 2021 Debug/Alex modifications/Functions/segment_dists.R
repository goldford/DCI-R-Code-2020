# Main function to compute distances and passabilities between all segments
get_segments_distance <- function(network = NULL){
  
  # Gather all segments into a vector
  segments <- network %N>%
    pull(membership) %>%
    unique()
  
  # TODO write test for get_ends function
  # Find all edge nodes for segments
  s.edges <- lapply(segments, FUN = get_edges, network = g.sub)
  names(s.edges) <- segments
  
  # Create container for to/from segment
  seg.dists <- matrix(0, nrow = length(segments), ncol = length(segments),
                      dimnames = list(segments, segments))
  
  # Determine the paths between segments
  
  # TODO Write test to make sure segments are iterating correctly
  # Create segment copy to iterate over
  seg.copy <- segments
  
  # Create a counter to index ends list
  i <- 0
  
  for(seg.from in seg.copy){
    
    # If number of segments is 1 there are no more distances to compute
    if(length(seg.copy == 1)){
      break()
    }
    
    # Remove one segment to avoid calculating distances between same segments or repeating calculations
    seg.copy <- seg.copy[-1]
    
    # Increment counter
    i <- i + 1
    
    for(seg.to in seg.copy){
      
      # Gather from and to segment edges
      from.edges <- s.edges[[seg.from]]
      to.edges <- s.edges[[seg.to]]
      
      # Find pair of nodes to compute path between
      sel.nodes <- closest_nodes(from.edges, to.edges)
      
      # Calculate distance between selected nodes
      distance <- get_distance(sel.nodes)
      
      # Store distance
      dists[i] <- distance
      
    }

  }
  
  # Store distances into result matrix
  seg.dists[lower.tri(seg.dists, diag = FALSE)] <- dists
  seg.dists <- t(seg.dists)
  seg.dists[lower.tri(seg.dists), diag = FALSE] <- dists
  
}

get_distance <- function(node_pair){
  
  # Retrieve path between the two
  path <- path_between(node_pair[1], node_pair[2])
  
  # Sum distance of segments between the two
  distance <- 
  
  # Return distance
  return(distance)
  
}

closest_nodes <- function(from, to){
  
  # Convert to list of individual characters
  
  # Apply comparison over list
  
}