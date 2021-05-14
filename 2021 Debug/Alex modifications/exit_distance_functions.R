###############################################################################
# Three helper functions:
#
# 1. Retrieve distance from given subsegment to each exit
#    sub_segment, segment, exit_node, exit_type, exit_dist
#
#    -- To retrieve the barriers around the segment:
#         * Upstream: all barriers with same membership
#         * Downstream: from node of lowest edge
#
# 2. Find the distance and cumulative passability between all segments
#    from_segment, from_node, pass, dist, to_segment, to_node
#
# 3. Extract a table of all adjacent exit nodes that are barriers for a given segment
#    segment, exit_node
###############################################################################

##### Main loop #####

# For each sub-segment in the network (n_1)

  # Retrieve segment membership (m_1)

  # -- Run function 1 --

  # -- Run function 3 -- (Don't think this needs to be a seperate function, function 1 already returns necessary info)

  # Retrieve all exit nodes of the given segment (B) returned from function 1/3

    # For each exit node (b_1)

      # Get distance to exit (d_1) from function 1/3

      # 


##### Create test subset #####

# Create subset of full graph
g.sub <- g.label %>%
  convert(to_local_neighborhood,
          node = which(.N()$type == "Sink"), 
          order = 4)

ggraph(g.sub) +
  geom_edge_link(aes(colour = forcats::fct_shuffle(as.factor(membership))), show.legend = FALSE, edge_width = 2) +
  geom_node_label(aes(label = name))

g.sub <- g.sub %N>%
  mutate(type = if_else(name %in% c("25", "20"), "Source Junction", type))

##### Coding main loop #####

# -- Note g_sub should be replaced with actual full network in full workflow

# For each node in the network except the sink node
segment.all <- g.sub %>%
  activate(nodes) %>%
  data.frame() %>%
  filter(type != "Sink") %>%
  pull(label)

for(segment in segment.all){
  
  exits <- get_exits(sub.segment, g.sub)
  
}

##### Function to find exits #####

# Making example with segment #16 (label = 001)

# Get membership of given sub-segment
get_exits <- function(sub_segment, G){
  
  
  
}