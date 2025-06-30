# Demonstration of ggrepel integration in netify
# This example shows how to create network plots with non-overlapping labels

library(netify)
library(ggplot2)

# Create a dense network where label overlap is likely
set.seed(123)
data <- data.frame(
    from = rep(c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix", 
                  "Philadelphia", "San Antonio", "San Diego"), each = 5),
    to = sample(c("New York", "Los Angeles", "Chicago", "Houston", "Phoenix", 
                  "Philadelphia", "San Antonio", "San Diego"), 40, replace = TRUE),
    weight = runif(40, 0.1, 1)
)
# Remove self-loops
data <- data[data$from != data$to, ]

# Create netify object
city_net <- netify(data, actor1 = "from", actor2 = "to", weight = "weight")

# Add network statistics
city_net <- add_node_vars(city_net, summary_actor(city_net), "actor")

# 1. Standard plot with overlapping labels
cat("\n=== Creating plot with standard labels (may overlap) ===\n")
p1 <- plot(city_net, 
           add_text = TRUE,
           node_size_by = "degree",
           edge_alpha = 0.3,
           node_color = "steelblue") +
    ggtitle("Standard Text Labels (Overlapping)")

# 2. Plot with repelled text labels
cat("\n=== Creating plot with repelled text labels ===\n")
p2 <- plot(city_net, 
           add_text_repel = TRUE,
           node_size_by = "degree",
           edge_alpha = 0.3,
           node_color = "steelblue",
           text_repel_force = 2,
           text_repel_segment_color = "gray30") +
    ggtitle("Repelled Text Labels (No Overlap)")

# 3. Plot with repelled box labels
cat("\n=== Creating plot with repelled box labels ===\n")
p3 <- plot(city_net,
           add_label_repel = TRUE,
           node_size_by = "degree",
           edge_alpha = 0.3,
           node_color = "darkgreen",
           label_repel_box_padding = 0.35,
           label_repel_force = 1.5,
           label_repel_segment_color = "gray50") +
    ggtitle("Repelled Box Labels")

# 4. Selective labeling with repel (only label top nodes)
cat("\n=== Creating plot with selective repelled labels ===\n")
top_cities <- names(sort(summary_actor(city_net)$degree, decreasing = TRUE)[1:4])
p4 <- plot(city_net,
           add_text_repel = TRUE,
           select_text = top_cities,
           node_size_by = "degree",
           edge_alpha = 0.3,
           node_color_by = "degree",
           text_repel_segment_color = "red",
           text_repel_min_segment_length = 0,
           text_size = 4) +
    scale_color_viridis_c() +
    ggtitle("Selective Repelled Labels (Top 4 Cities)")

# 5. Advanced customization with component extraction
cat("\n=== Creating customized plot using components ===\n")
comp <- plot(city_net,
             add_text_repel = TRUE,
             node_size_by = "degree",
             return_components = TRUE)

p5 <- ggplot() +
    netify_edge(comp) +
    scale_alpha_continuous(range = c(0.1, 0.4)) +
    reset_scales() +
    netify_node(comp) +
    scale_size_continuous(range = c(3, 10)) +
    scale_color_gradient(low = "lightblue", high = "darkblue") +
    reset_scales() +
    netify_text_repel(comp) +
    theme_netify() +
    ggtitle("Custom Plot with Repelled Labels")

# Display comparison
library(patchwork)
comparison <- (p1 + p2) / (p3 + p4)
print(comparison + plot_annotation(
    title = "ggrepel Integration in netify",
    subtitle = "Comparing standard labels vs. repelled labels"
))

# Show repel parameters in action
cat("\n=== Demonstrating repel parameters ===\n")

# Low force vs high force
p_low_force <- plot(city_net,
                    add_text_repel = TRUE,
                    text_repel_force = 0.5,
                    edge_alpha = 0.2) +
    ggtitle("Low Repel Force (0.5)")

p_high_force <- plot(city_net,
                     add_text_repel = TRUE,
                     text_repel_force = 5,
                     edge_alpha = 0.2) +
    ggtitle("High Repel Force (5)")

force_comparison <- p_low_force + p_high_force
print(force_comparison + plot_annotation(
    title = "Effect of Repel Force Parameter"
))

cat("\n=== ggrepel features available in netify ===\n")
cat("
Text repel parameters:
- text_repel_force: Repulsion force between labels
- text_repel_max_overlaps: Maximum allowed overlaps
- text_repel_box_padding: Padding around text
- text_repel_point_padding: Padding around points
- text_repel_segment_color: Color of connecting lines
- text_repel_min_segment_length: Minimum segment length
- And many more...

Label repel parameters:
- Similar to text_repel with label_repel_ prefix
- Additional: label_repel_label_padding, label_repel_label_r

Usage tips:
1. Use add_text_repel=TRUE for simple text without backgrounds
2. Use add_label_repel=TRUE for text with background boxes
3. Combine with select_text/select_label for selective labeling
4. Adjust force parameter to control spacing
5. Use segment parameters to style connector lines
")

cat("\nExample complete! Plots have been created showing various ggrepel features.\n")