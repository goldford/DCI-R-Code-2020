##### Load required packages and data #####

require(dplyr)
require(tidyr)
require(magrittr)
require(tidygraph)
require(ggraph)
require(igraph)

# Read in FIPEX table
FIPEX_table <- read.csv("../FIPEX_Advanced_DD_2020.csv")%>%
  # Ensure sink is coded as "ink"
  mutate(DownstreamEID = ifelse(DownstreamEID == "Sink", "sink", as.character(DownstreamEID)))

# Read in FIPEX params file
FIPEX_params=read.csv("../FIPEX_2020_params.csv")

##### Source functions #####

# This function creates an adjacency matrix from the adjacency table
source("adj_weighted_function.R")

# These are the unique labelling functions
source("labelling_functions.R")

# These are the membership labelling functions
source("membership_functions.R")

##### Create graph object #####

# Create igraph from adjacency table
g_dd <- graph_from_adjacency_matrix(adj_weighted(FIPEX_table = FIPEX_table, direction = "anti-flow"), 
                                    weighted = TRUE, 
                                    mode = "directed")
# Convert with tidygraph
g_tidy <- as_tbl_graph(g_dd)

# Join node attributes: type and permeability
g_tidy <- g_tidy %>%
  activate(nodes) %>%
  left_join(FIPEX_table %>% mutate(ID = as.character(NodeEID+1)), by = c("name" = "ID")) %>%
  select(name, type = NodeType, perm = BarrierPerm) %>%
  # Create category for sink node
  mutate(type = replace_na(type, "Sink"))

# Plot graph to see if all makes sense
ggraph(g_tidy, "tree") +
  geom_edge_link() +
  geom_node_point(aes(colour = as.factor(type)), size = 1.5)

##### Apply labelling workflow #####

# Extract node table
ex_nodes <- g_tidy %>%
  activate(nodes) %>%
  data.frame() %>%
  mutate(label = "1") %>%
  mutate(parent = "0")

# Apply node labelling function over graph
g_label <- g_tidy %N>%
  mutate(label = map_dfs_chr(root = which(.N()$type == "Sink"),
                             .f = node_labeller))

# Apply edge labelling function over graph
g_label <- g_label %>%
  activate(edges) %>%
  mutate(label = edge_labeller(g_label))

# Since the number of segments is equal to the number of source and branch junctions we can generate unique membership IDs following this rule
num_labels <- nrow(ex_nodes[ex_nodes$type %in% c("Source Junction", "Branch Junction"),])
member_labels <- 1:num_labels

# Apply node membership function over graph
g_label <- g_label %N>%
  mutate(membership = map_dfs_chr(root = which(.N()$type == "Sink"),
                                  .f = node_membership))

# Apply edge membership function over graph
g_label <- g_label %>%
  activate(edges) %>%
  mutate(membership = edge_membership(g_label))

##### Results of labelling functions #####

# Extract graph edges
edges <- g_label %>%
  activate(edges) %>%
  data.frame()

# Extract graph nodes
nodes <- g_label %>%
  activate(nodes) %>%
  data.frame()

# Check that each edge label is unique
length(unique(edges$label)) == nrow(edges)

# Chack that each edge has a membership
ggraph(g_label) +
  geom_edge_link(aes(colour = forcats::fct_shuffle(as.factor(membership))), show.legend = FALSE, edge_width = 2) +
  geom_node_point()

