##### Load required packages and data #####

require(dplyr)
require(tidyr)
require(magrittr)
require(tidygraph)
require(ggraph)
require(igraph)

# Read in FIPEX table
FIPEX.table <- read.csv("2021 Debug/FIPEX_Advanced_DD_2020.csv")%>%
  # Ensure sink is coded as "ink"
  mutate(DownstreamEID = ifelse(DownstreamEID == "Sink", "sink", as.character(DownstreamEID)))

# Read in FIPEX params file
FIPEX.params=read.csv("2021 Debug/FIPEX_2020_params.csv")

##### Source functions #####

# This function creates an adjacency matrix from the adjacency table
source("2021 Debug/Alex modifications/adj_weighted_function.R")

# These are the unique labelling functions
source("2021 Debug/Alex modifications/labelling_functions.R")

# These are the membership labelling functions
source("2021 Debug/Alex modifications/membership_functions.R")

##### Create graph object #####

# Create igraph from adjacency table
g.dd <- graph_from_adjacency_matrix(adj_weighted(FIPEX.table = FIPEX.table, direction = "anti-flow"), 
                                    weighted = TRUE, 
                                    mode = "directed")
# Convert with tidygraph
g.tidy <- as_tbl_graph(g.dd)

# Join node attributes: type and permeability
g.tidy <- g.tidy %>%
  activate(nodes) %>%
  left_join(FIPEX.table %>% mutate(ID = as.character(NodeEID+1)), by = c("name" = "ID")) %>%
  select(name, type = NodeType, perm = BarrierPerm) %>%
  # Create category for sink node
  mutate(type = replace_na(type, "Sink"))

# Add edge weight to node table as length attribute
g.tidy %>%
  activate(nodes) %>%
  mutate(row = 1:n()) %>%
  mutate(length = .E()$weight[row]) %>%
  select(-row)

# Plot graph to see if all makes sense
ggraph(g.tidy, "tree") +
  geom_edge_link() +
  geom_node_point(aes(colour = as.factor(type)), size = 1.5)

##### Apply labelling workflow #####

# Extract node table
ex.nodes <- g.tidy %>%
  activate(nodes) %>%
  data.frame() %>%
  mutate(label = "1") %>%
  mutate(parent = "0")

# Apply node labelling function over graph
g.label <- g.tidy %N>%
  mutate(label = map_dfs_chr(root = which(.N()$type == "Sink"),
                             .f = node_labeller))

# Apply edge labelling function over graph
g.label <- g.label %>%
  activate(edges) %>%
  mutate(label = edge_labeller(g.label))

# IDs should be equal to maximum possibilitty - # nodes
# Since the number of segments is equal to barriers + 1, we can generate IDs based on this rule
num.labels <- nrow(ex.nodes[ex.nodes$type == "Barrier",])
member.labels <- 1:num.labels

# Apply node membership function over graph
g.label <- g.label %N>%
  mutate(membership = map_dfs_chr(root = which(.N()$type == "Sink"),
                                  .f = node_membership))

# Apply edge membership function over graph
g.label <- g.label %>%
  activate(edges) %>%
  mutate(membership = edge_membership(g.label))

##### Results of labelling functions #####

# Extract graph edges
edges <- g.label %>%
  activate(edges) %>%
  data.frame()

# Extract graph nodes
nodes <- g.label %>%
  activate(nodes) %>%
  data.frame()

# Check that each edge label is unique
length(unique(edges$label)) == nrow(edges)

# Chack that each edge has a membership
ggraph(g.label) +
  geom_edge_link(aes(colour = forcats::fct_shuffle(as.factor(membership))), show.legend = FALSE, edge_width = 2) +
  geom_node_point()

##### Clean up environment #####

rm(list=setdiff(ls(), "g.label"))