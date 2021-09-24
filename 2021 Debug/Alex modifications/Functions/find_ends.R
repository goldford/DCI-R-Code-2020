get_edges <- function(seg.membership, network){
  
  # Get segment's sub-segments
  member.segs <- network %N>% filter(membership == seg.membership) %>% data.frame()
  
  # Get upstream edge nodes
  up.ends <- member.segs %>%
    filter(type %in% "Barrier") %>%
    pull(label)
  
  # Get downstream edge node
  down.end <- member.segs[which.min(member.segs$label),]$label
  
  # Return all edge nodes
  return(c(down.end, up.ends))
  
}