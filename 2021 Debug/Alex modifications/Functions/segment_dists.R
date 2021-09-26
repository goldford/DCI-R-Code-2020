# Main function to compute distances and passabilities between all segments
get_segments_distance <- function(network = NULL){
  
  ##### Loop setup #####
  
  # Gather all segments into a vector
  segments <- network %N>%
    pull(membership) %>%
    unique()
  
  # Find all edge nodes for segments 
  s.edges <- lapply(segments, FUN = get_edges, network = g.sub) # TODO write test for get_ends function
  names(s.edges) <- segments # Might not need this
  
  # Create distance container for to/from segment
  seg.dists <- matrix(0, nrow = length(segments), ncol = length(segments),
                      dimnames = list(segments, segments))
  
  # Create permeability container for  to/from segment
  seg.perms <- matrix(0, nrow = length(segments), ncol = length(segments),
                      dimnames = list(segments, segments))
  
  # Create distance and permeability vector container
  dists <- integer(length = length(rep(segments, 3:0)))
  perms <- integer(length = length(rep(segments, 3:0)))

  # Create segment copy to iterate over
  seg.copy <- segments   # TODO Write test to make sure segments are iterating correctly
  
  # Create a counter to index ends list
  i <- 0
  
  ##### Loop body #####
  
  for(seg.from in seg.copy){
    
    # If number of segments is 1 there are no more distances to compute
    if(length(seg.copy) == 1){
      break()
    }
    
    # Remove one segment to avoid calculating distances between same segments or repeating calculations
    seg.copy <- seg.copy[-1]
    
    for(seg.to in seg.copy){
      
      # Increment counter
      i <- i + 1
      
      # Gather from and to segment edges
      from.edges <- s.edges[[seg.from]]
      to.edges <- s.edges[[seg.to]]
      
      # Find pair of nodes to compute path between
      seg.path <- shortest_seg_path(from.edges, to.edges)
      
      # If path only contains 2 nodes, segments are neighbors and get 0 distance
      if(length(seg.path) == 2){
        dists[i] <- 0
        perms[i] <- 0
      } 
      # Else calculate distance between selected nodes
      else {
        dist.pass <- get_distance(seg.path, network)
        dists[i] <- sum(dist.pass$length)
        perms[i] <- prod(dist.pass$perm)
      }
      
    }
    
  }
  
  ##### Store results #####
  
  # Store distances into result matrix
  seg.dists[lower.tri(seg.dists, diag = FALSE)] <- dists
  seg.dists <- t(seg.dists)
  seg.dists[lower.tri(seg.dists, diag = FALSE)] <- dists
  
  # Store permeability into result matrix
  seg.perms[lower.tri(seg.perms, diag = FALSE)] <- perms
  seg.perms <- t(seg.perms)
  seg.perms[lower.tri(seg.perms, diag = FALSE)] <- perms
  
}

# For each target node, in each origin node determine character by character
# how far the matching goes
shortest_seg_path <- function(from, to){ # TODO Write test for this function
  
  # If both segments only have one node return path between them
  if(length(from) == 1 & length(to) == 1){
    return(path_between(from, to))
  }
  
  # Create results containers
  from.res <- rep(from, length(to))
  to.res <- rep(to, length(from))
  
  # Determine all possible paths between edge nodes
  paths <- mapply(from.res, FUN = path_between, to.res)
  
  # Select shortest path and return it
  path.lengths <- unlist(lapply(paths, FUN = length))
  return(paths[[which.min(path.lengths)]])
  
}

get_distance <- function(seg.path, network){ # TODO Write test for this function
  
  # If there are only 2 nodes then they are neighbouring segments
  if(length(seg.path) == 2){
    return(0)
  }
  
  # Remove first node as it holds length not required here
  seg.path <- seg.path[-1]
  
  # Retrieve node data from network
  nodes <- network %>%
    activate(nodes) %>%
    data.frame()
  
  # Gather permeability and length properties from matching nodes
  length.pass <- nodes[nodes$label %in% seg.path,][c("length","perm")]
  
  # Return result
  return(length.pass)
  
}
