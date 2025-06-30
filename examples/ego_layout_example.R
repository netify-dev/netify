# Example: Ego Network Layouts in netify
# This script demonstrates the use of specialized ego-centric layouts

library(netify)
library(ggplot2)

# Set seed for reproducibility
set.seed(6886)

# Create a sample network with an ego and multiple alters
# representing different types of relationships
sample_data <- data.frame(
    from = c(rep("Alice", 12), 
             "Bob", "Carol", "Dave", "Eve",
             "Frank", "Grace", "Henry", "Ivy"),
    to = c("Bob", "Carol", "Dave", "Eve", "Frank", 
           "Grace", "Henry", "Ivy", "Jack", "Kate",
           "Laura", "Mike",
           "Carol", "Dave", "Eve", "Frank",
           "Grace", "Henry", "Ivy", "Jack"),
    weight = c(10, 8, 9, 7, 6, 5, 8, 9, 3, 4, 2, 3,
               5, 4, 3, 6, 7, 8, 2, 3),
    stringsAsFactors = FALSE
)

# Add nodal attributes for grouping and ordering
nodal_attrs <- data.frame(
    actor = c("Alice", "Bob", "Carol", "Dave", "Eve", "Frank",
              "Grace", "Henry", "Ivy", "Jack", "Kate", "Laura", "Mike"),
    group = c("ego", "family", "family", "work", "work", "work",
              "friends", "friends", "friends", "acquaintance", 
              "acquaintance", "acquaintance", "acquaintance"),
    closeness = c(10, 9, 9, 7, 6, 5, 8, 8, 7, 3, 4, 2, 3),
    degree = c(12, 3, 4, 4, 4, 4, 4, 4, 4, 2, 1, 1, 1),
    stringsAsFactors = FALSE
)

# Create netify object
net <- netify(
    sample_data,
    actor1 = "from",
    actor2 = "to", 
    weight = "weight",
    symmetric = TRUE,
    nodal_data = nodal_attrs
)

# Create ego network for Alice
ego_net <- ego_netify(net, ego = "Alice", threshold = 0)

# Example 1: Basic star layout (default)
plot(ego_net, 
     layout = "star",
     add_text = TRUE,
     node_size = 8,
     edge_alpha = 0.5) +
     ggtitle("Ego Network: Star Layout")

# Example 2: Radial layout with grouping by relationship type
plot(ego_net,
     layout = "radial",
     ego_group_by = "group",
     add_text = TRUE,
     node_size_by = "degree",
     node_fill_by = "group",
     edge_alpha_by = "weight") +
     ggtitle("Ego Network: Radial Layout Grouped by Relationship Type")

# Example 3: Radial layout with weight-based distance
plot(ego_net,
     layout = "radial", 
     ego_weight_to_distance = TRUE,
     add_text = TRUE,
     node_size = 8,
     node_fill_by = "group",
     edge_alpha_by = "weight") +
     ggtitle("Ego Network: Distance Represents Tie Strength")

# Example 4: Concentric layout with rings based on closeness
plot(ego_net,
     layout = "concentric",
     ego_group_by = "closeness",
     ego_ring_gap = 0.2,
     add_text = TRUE,
     node_size_by = "degree", 
     node_fill_by = "group") +
     ggtitle("Ego Network: Concentric Rings by Closeness")

# Example 5: Concentric layout with categorical groups
plot(ego_net,
     layout = "concentric",
     ego_group_by = "group",
     ego_order_by = "degree",
     add_label = TRUE,
     node_size = 10,
     edge_alpha = 0.3) +
     ggtitle("Ego Network: Concentric Rings by Group")

# Example 6: Longitudinal ego network
# Create time-varying data
longit_data <- rbind(
    transform(sample_data[1:15,], time = 2020),
    transform(sample_data[c(1:10, 16:20),], time = 2021,
              weight = weight * runif(15, 0.8, 1.2))
)

# Update nodal attributes for both time periods
longit_nodal <- rbind(
    transform(nodal_attrs, time = 2020),
    transform(nodal_attrs[c(1:10, 11:12),], time = 2021)
)

# Create longitudinal network
net_longit <- netify(
    longit_data,
    actor1 = "from",
    actor2 = "to",
    weight = "weight", 
    time = "time",
    symmetric = TRUE,
    nodal_data = longit_nodal
)

# Create longitudinal ego network
ego_net_longit <- ego_netify(net_longit, ego = "Alice")

# Plot with radial layout maintaining positions
plot(ego_net_longit,
     layout = "radial",
     ego_group_by = "group",
     add_text = TRUE,
     node_fill_by = "group") +
     facet_wrap(~ time) +
     ggtitle("Ego Network Evolution: 2020-2021")

# Example 7: Custom styling with ego layout
# Create a focused view highlighting strong ties
plot(ego_net,
     layout = "radial",
     ego_weight_to_distance = TRUE,
     ego_group_by = "group",
     edge_filter = weight > 5,  # Show only strong ties
     add_text = TRUE,
     node_size = 10,
     node_fill_by = "group",
     edge_linewidth_by = "weight",
     edge_alpha = 0.7,
     edge_color = "darkgray") +
     scale_size_continuous(range = c(5, 15)) +
     scale_edge_linewidth_continuous(range = c(0.5, 3)) +
     theme_minimal() +
     ggtitle("Strong Ties in Alice's Ego Network")

# Example 8: Comparing layouts side by side
library(patchwork)

p1 <- plot(ego_net, layout = "fr", add_text = TRUE, node_size = 6) + 
      ggtitle("Standard Force-Directed")
      
p2 <- plot(ego_net, layout = "radial", ego_group_by = "group", 
           add_text = TRUE, node_size = 6) +
      ggtitle("Ego-Centric Radial")

p1 + p2

# Example 9: Using pre-computed ego layout
# Get the layout data
ego_layout <- get_ego_layout(
    ego_net,
    layout = "radial",
    group_by = "group",
    order_by = "degree",
    weight_to_distance = TRUE
)

# Modify positions if needed
ego_layout[[1]]$x <- ego_layout[[1]]$x * 1.2  # Spread out more
ego_layout[[1]]$y <- ego_layout[[1]]$y * 1.2

# Use custom layout
plot(ego_net,
     point_layout = ego_layout,
     add_text = TRUE,
     node_fill_by = "group") +
     ggtitle("Ego Network with Custom Modified Layout")