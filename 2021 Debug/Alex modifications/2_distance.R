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

# Gather all segments into a vector
segments <- network %N>%
  pull(membership) %>%
  unique()

# Get edge nodes of segments
segment.edges <- lapply(segments, FUN = get_edges, network = g.sub) # TODO write test for get_ends function
names(segment.edges) <- segments 

# Calculate distance and cumulative permeability of segment-segment paths
segment.dist.perm <- get_segments_distance(g.sub, segment.edges)

##### Get distance to exit for each sub-segment #####

exit.distances <- get_exit_distances(g.sub, segment.edges, segment.dist.perm)
