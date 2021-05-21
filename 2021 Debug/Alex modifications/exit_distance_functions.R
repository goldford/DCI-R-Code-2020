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

# DONE FOR each sub-segment in the network (n_1)

  # DONE Retrieve segment membership (m_1)

  # DONE Get segment exits (function 1/3)        

  # DONE FOR each exit node (b_1)

    # DONE Get distance to exit (d_1) (function 1/3)

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

##### Function to get distance between 2 nodes #####

path_to_root <- function(label){
  
  # Create empty result container
  path <- vector("character", nchar(label))
  
  # Get path to root
  for(i in 1:nchar(label)){
    path[i] <- substr(label, 1, nchar(label) - (i-1))
  }
  
  # Return result
  return(path)
  
}

path_between <- function(s1, s2){
  
  # Set start and end
  start <- s1
  end <- s2
  
  # Get segment to root paths
  start.path <- path_to_root(s1)
  end.path <- path_to_root(s2)
  
  # Find common ancestor of both paths
  path.ca <- match(start.path, end.path)
  ca.position.end <- (min(path.ca, na.rm = T))
  ca.position.start <- match(ca.position.end, path.ca)
  
  # Get path, reducing by 1 to exclude the common ancestor
  full.path <- start.path[1:ca.position.start - 1]
  full.path <- append(full.path, end.path[1:ca.position.end - 1])
  
  # Return full path
  return(full.path)
  
}

##### Function to find entrance and exits #####

# Get entrance node (downstream) of segment
get_entrance <- function(seg.membership, network){
  
  # Get segment's sub-segments
  member.segs <- network %N>% filter(membership == seg.membership) %>% data.frame()
  
  # Get downstream entrance node
  entrance <- member.segs[which.min(member.segs$label),]$label
  
  # Return entrance node label
  return(entrance)
  
}

# Get exit nodes (upstream) of segment
get_exits <- function(seg.membership, network){
  
  # Get segment's sub-segments
  member.segs <- network %N>% filter(membership == seg.membership) %>% data.frame()
  
  # Get upstream exit nodes
  exits <- member.segs %>%
    filter(type %in% "Barrier") %>%
    pull(label)
  
  # Return exit nodes labels
  if(length(exits) == 0){
    return(NA)
  } else {return(exits)}
    
}

##### Create segment-segment table #####

# Gather all segments into a vector
segments <- g.sub %N>%
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

##### Coding main loop #####

# -- Note g_sub should be replaced with actual full network in full workflow

# Get all segments
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
