##### Create test subset #####

# Create subset of full graph
g.sub <- g.label %>%
  convert(to_local_neighborhood,
          node = which(.N()$type == "Sink"), 
          order = 4)

ggraph(g.sub) +
  geom_edge_link(aes(colour = forcats::fct_shuffle(as.factor(membership))), show.legend = FALSE, edge_width = 2) +
  geom_node_label(aes(label = label))

g.sub <- g.sub %N>%
  mutate(type = if_else(name %in% c("25", "20"), "Source Junction", type))

##### Source functions #####

source("2021 Debug/Alex modifications/Functions/find_ends.R")
source("2021 Debug/Alex modifications/Functions/find_path.R")
source("2021 Debug/Alex modifications/Functions/segment_dists.R")

##### Get segment-to-segment distances #####

seg.dist.perm <- get_segments_distance(g.sub)
seg.dists <- seg.dist.perm[[1]]
seg.perms <- seg.dist.perm[[2]]

##### Coding main loop #####

# Extract segment table
sub.segment.all <- g.sub %>%
  activate(nodes) %>%
  filter(type != "Sink") %>%
  pull(label)

# Extract nodes table
g.nodes <- g.sub %N>%
  data.frame()

# Main loop
for(sub.segment in sub.segment.all){
  
  # Get all the sub-segment's segment exits
  exits <- get_exits(sub.segment, g.sub)
  
  for(exit in exits){
    
    # Get distance to exit
    path.to.exit <- path_between(s1 = sub.segment, s2 = exit)
    dist.to.exit <- sum(g.nodes[g.nodes$label %in% path.to.exit,]$length)
    
  }
  
}
