# Example demonstrating ggrepel integration in netify plots

library(netify)

# Create a simple network
icews_10 <- icews[icews$year==2010,]
net_10 <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Add nodal stats
net_10 <- add_node_vars(
    net_10,
    summary_actor(net_10),
    "actor"
)

# Basic plot with regular text (may overlap)
p1 <- plot(net_10, 
           add_text = TRUE,
           node_size_by = "degree_total",
           edge_alpha_by = "verbCoop")

# Plot with text_repel (avoids overlaps)
p2 <- plot(net_10, 
           add_text_repel = TRUE,
           node_size_by = "degree_total",
           edge_alpha_by = "verbCoop",
           text_repel_force = 2,
           text_repel_max_overlaps = 20)

# Plot with label_repel (boxed labels that avoid overlaps)
p3 <- plot(net_10,
           add_label_repel = TRUE,
           node_size_by = "degree_total",
           edge_alpha_by = "verbCoop",
           label_repel_force = 1.5,
           label_repel_box_padding = 0.3,
           label_repel_segment_color = "grey30")

# Selective labeling with repel
top_nodes <- names(sort(summary_actor(net_10)$degree_total, decreasing = TRUE)[1:5])
p4 <- plot(net_10,
           add_text_repel = TRUE,
           select_text = top_nodes,
           node_size_by = "degree_total",
           edge_alpha_by = "verbCoop",
           text_repel_segment_color = "red",
           text_repel_box_padding = 0.5)

# Using both regular nodes and repelled labels
p5 <- plot(net_10,
           add_text = FALSE,  # This is overridden by add_text_repel
           add_text_repel = TRUE,
           node_color_by = "degree_total",
           edge_alpha = 0.3,
           text_repel_force = 3,
           text_repel_min_segment_length = 0)

# Advanced: Get components and build custom plot
comp <- plot(net_10,
             add_text_repel = TRUE,
             add_label_repel = TRUE,
             return_components = TRUE)

# Manually assemble with custom modifications
library(ggplot2)
custom_plot <- ggplot() +
    netify_edge(comp) +
    scale_alpha_continuous(range = c(0.1, 0.5)) +
    reset_scales() +
    netify_node(comp) +
    scale_color_viridis_c() +
    reset_scales() +
    netify_text_repel(comp) +
    theme_netify()

# Compare plots side by side
library(patchwork)
comparison <- p1 + p2 + 
    plot_annotation(
        title = "Comparison: Regular Text vs Text Repel",
        subtitle = "Left: Overlapping labels | Right: Automatically repositioned labels"
    )

print(comparison)