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

##### Main loop pseudocode #####

# FOR each sub-segment in the network (n_1)

  # Retrieve segment membership (m_1)

  # Get segment exits (function 1/3)

  # FOR each exit node (b_1)

    # Get distance to exit (d_1) (function 1/3)

    # Filter segment distance table to from_segment = m_1 / from_node = b_1

    # FOR each segment-segment row

      # Get to_segment m_2, to_node n_2, passability p, distance d

      # Find row for exit_node n_2 and segment m_2

      # FOR each barrier exit b_2

        # Get exit distance d_2

# Return total distance d_1 + d_s + d_2

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

##### Coding main loop #####

# -- Note g_sub should be replaced with actual full network in full workflow

# For each node in the network except the sink node
segment.all <- g.sub %>%
  activate(nodes) %>%
  filter(type != "Sink") %>%
  pull(label)

for(segment in segment.all){
  
  exits <- get_exits(sub.segment, g.sub)
  
}

##### Function to get distance between 2 nodes #####

calc_distance <- function(s_1, s_2, net){
  
  
  
}

##### Function to find exits #####

# Making example with segment #16 (label = 001)

# Get membership of given sub-segment
get_exits <- function(sub.segment, G){
  
  # Get membership
  membership <- G %N>% filter(label == sub.segment) %>% pull(membership)
  
  # Get same segment nodes
  member.nodes <- G %N>% filter(membership == membership)
  
  nodes <- G %>% activate(nodes) %>% data.frame()
  
  # Get membership
  membership <- G %>%
    activate(nodes) %>%
    filter(membership == )
  
}