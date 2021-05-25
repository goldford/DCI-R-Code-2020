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
  segs.expand <- segs %>%
    rename(from = segment) %>%
    mutate(to = from) %>%
    expand(from, to) %>%
    # Join from segment entrances and exits
    left_join(segs, by = c("from" = "segment")) %>%
    rename(entrance_from = entrance, exits_from = exits) %>%
    # Join to segments entrances and exits
    left_join(segs, by = c("to" = "segment")) %>%
    rename(entrance_to = entrance, exits_to = exits)
  
  # Calculate distance and cumulative passability between segments
  segs.expand$distance <- get_seg_dist()
  segs.expand$pass <- get_seg_pass()
  
}