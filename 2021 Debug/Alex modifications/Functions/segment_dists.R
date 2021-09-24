# Main function to compute distances and passabilities between all segments
get_segments_distance <- function(network = NULL){
  
  # Gather all segments into a vector
  segments <- network %N>%
    pull(membership) %>%
    unique()
  
  # Find entrances of all segments and hold in a vector
  # the entrance is the most downstream node in the segment
  entrances <- unlist(lapply(segments, FUN = get_downstream_end, network = g.sub))
  
  # Find exits of all segments and hold in a vector
  # segments can have none (NA), one, or multiple exits
  exits <- lapply(segments, FUN = get_upstream_ends, network = g.sub)
  
  # Create container for to/from segment
  seg.dists <- matrix(nrow = length(segments), ncol = length(segments),
                      dimnames = list(segments, segments))
  
  # Determine the paths between segments
  apply(cbind(segments, from, to), FUN = gather_property)
  
}

gather_property <- function(...){
  
  
  
}

# Note: These distance functions always proceed from the point closest to the
# sink even if the segment is labeled 'from' it does not always mean it is the 
# source
get_seg_path <- function(seg.table = NULL){
  
  # Create result container
  res.length <- sum(seq(nrow(seg.table)))
  from <- vector("character", length = res.length)
  to <- vector("character", length = res.length)
  exit <- vector("character", length = res.length)
  entrance <- vector("character", length = res.length)
  distances <- vector("numeric", length = res.length)
  pass <- vector("numeric", length = res.length)
  
  # Get list of segments
  segments <- seg.table$segment
  
  # Start iterator
  i <- 0
  
  for(seg.from in segments){
    
    for(seg.to in segments){
      
      # Store segment IDs
      from[i] <- seg.from
      to[i] <- seg.to
      
      # Check if both segments are the same
      if(seg.from == seg.to){
        distances[i] <- 0
        pass[i] <- 0
        
      } else {
        # if to segment is a child of from segment
        if (regexpr(paste0("^(", seg.from, ")"), seg.to)[1] == 1){
          entrance.loop <- segs[segs$segment == seg.to,]$entrance
          exit.loop <- unlist(segs[segs$segment == seg.from,]$exits)
          
        # if to segment is the parent of from segment
        } else {
          entrance.loop <- segs[segs$segment == seg.from,]$entrance
          exit.loop <- segs[segs$segment == seg.to,]$exits
        }
        
        # Find which exit is the closest (ancestor) to the entrance
        path.topo <- unlist(lapply(paste0("^", exit.loop), regexpr, entrance.loop))
        exit[i] <- exit.loop[which(a == 1)]
        entrance[i] <- entrance.loop
        
        # Use helper functions to calculate distance and passability
        path <- path_between(exit[i], entrance[i])
        
        # Need to write these, probably better to send this back up stack and 
        # Build new function from there rather than going deeper
        distance[i] <- gather_property(path, property = "distance")
        pass[i] <- gather_property(path, property = "passability")
      }
      
      # Iterate
      i <- i + 1
    }
    
    # Remove visited segment
    segments <- segments[segments != seg.from]
  }
}
