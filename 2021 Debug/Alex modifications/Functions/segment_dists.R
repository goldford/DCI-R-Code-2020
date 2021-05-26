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
      
      # Check if both segments are the same
      if(seg.from == seg.to){
        distances[i] <- 0
        pass[i] <- 0
        
      } else {
        
        # First thing to check is if segment is upstream of target or downstream
        # Then can figure out whether to use segment entrance or exit
        
        # Then find exit from list which is closest to entrance
        
        # Use helper functions to calculate distance
        
        # Use helper functions to calculate sum of passability
        
      }
      
      # Iterate
      i <- i + 1
      
    }
    
    # Remove visited segment
    segments <- segments[segments != seg.from]
    
  }
  
}