# Get entrance node (downstream) of segment
get_downstream_end <- function(seg.membership, network){
  
  # Get segment's sub-segments
  member.segs <- network %N>% filter(membership == seg.membership) %>% data.frame()
  
  # Get downstream entrance node
  d.end <- member.segs[which.min(member.segs$label),]$label
  
  # Return entrance node label
  return(d.end)
  
}

# Get exit nodes (upstream) of segment
get_upstream_ends <- function(seg.membership, network){
  
  # Get segment's sub-segments
  member.segs <- network %N>% filter(membership == seg.membership) %>% data.frame()
  
  # Get upstream exit nodes
  u.ends <- member.segs %>%
    filter(type %in% "Barrier") %>%
    pull(label)
  
  # Return exit nodes labels
  if(length(u.ends) == 0){
    return(NA)
  } else {return(u.ends)}
  
}