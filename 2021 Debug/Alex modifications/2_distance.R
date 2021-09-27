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

segment.dist.perm <- get_segments_distance(g.sub)

##### Get distance to exit for each sub-segment #####

exit.distances <- get_exit_distances(g.sub)
