# Main function to compute distances and passabilities between all segments
get_segment_dists <- function(network = NULL){
  
  # Gather all segments into a vector
  segments <- network %N>%
    pull(membership) %>%
    unique()
  
  # Find entrance of all segment
  entrances <- unlist(lapply(segments, FUN = get_entrance, network = g.sub))
  
  # Get exits of all segments
  exits <- lapply(segments, FUN = get_exits, network = g.sub)
  
  # Put together in a dataframe
  segs <- data.frame(segment = segments,
                     entrance = entrances,
                     exits = I(exits))
  
  # Expand dataframe to include all interactions of segments
  # segs.expand <- segs %>%
  #   rename(from = segment) %>%
  #   mutate(to = from) %>%
  #   expand(from, to) %>%
  #   # Join from segment entrances and exits
  #   left_join(segs, by = c("from" = "segment")) %>%
  #   rename(entrance_from = entrance, exits_from = exits) %>%
  #   # Join to segments entrances and exits
  #   left_join(segs, by = c("to" = "segment")) %>%
  #   rename(entrance_to = entrance, exits_to = exits)
  
  # Calculate distance and cumulative passability between segments
  segs.expand$distance <- get_seg_dist_pass(segs)
  
}

# Note: These distance functions always proceed from the point closest to the
# sink even if the segment is labeled from it does not always mean it is the 
# source
get_seg_dist_pass <- function(seg.table = NULL){
  
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
        distance[i] <- gather_distance()
        pass[i] <- gather_passability()
        
      }
      
      # Iterate
      i <- i + 1
      
    }
    
    # Remove visited segment
    segments <- segments[segments != seg.from]
    
  }
  
}