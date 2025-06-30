# Example: Visualizing Attribute Mixing Patterns in Networks

library(netify)
library(ggplot2)

# Create example international relations network
set.seed(123)
n_countries <- 60

# Generate country attributes
country_data <- data.frame(
    actor = paste0("Country_", 1:n_countries),
    region = sample(c("Americas", "Europe", "Asia", "Africa"),
        n_countries,
        replace = TRUE,
        prob = c(0.25, 0.25, 0.35, 0.15)
    ),
    regime_type = sample(c("Democracy", "Autocracy", "Hybrid"),
        n_countries,
        replace = TRUE,
        prob = c(0.4, 0.3, 0.3)
    ),
    development = sample(c("High", "Medium", "Low"),
        n_countries,
        replace = TRUE,
        prob = c(0.3, 0.4, 0.3)
    ),
    stringsAsFactors = FALSE
)

# Create network with homophily patterns
adjacency <- matrix(0, n_countries, n_countries)
for (i in 1:(n_countries - 1)) {
    for (j in (i + 1):n_countries) {
        # Base probability
        prob <- 0.05

        # Same region increases probability
        if (country_data$region[i] == country_data$region[j]) {
            prob <- prob + 0.25
        }

        # Same regime type increases probability
        if (country_data$regime_type[i] == country_data$regime_type[j]) {
            prob <- prob + 0.15
        }

        # Similar development level increases probability
        if (country_data$development[i] == country_data$development[j]) {
            prob <- prob + 0.10
        }

        # Create alliance with calculated probability
        adjacency[i, j] <- adjacency[j, i] <- rbinom(1, 1, min(prob, 0.8))
    }
}
rownames(adjacency) <- colnames(adjacency) <- country_data$actor

# Create netify object
alliance_network <- netify(adjacency, actor1 = NULL, actor2 = NULL, symmetric = TRUE)
alliance_network <- add_node_vars(alliance_network, country_data, actor = "actor")

# Example 1: Basic Mixing Matrix Visualization
cat("=== Example 1: Regime Type Mixing Patterns ===\n")

# Calculate mixing matrix
regime_mixing <- mixing_matrix(
    alliance_network,
    attribute = "regime_type",
    normalized = TRUE
)

# Print summary statistics
cat("\nMixing matrix statistics:\n")
cat("- Assortativity:", round(regime_mixing$summary_stats$assortativity, 3), "\n")
cat("- Diagonal proportion:", round(regime_mixing$summary_stats$diagonal_proportion, 3), "\n")

# Create visualization
p1 <- plot_mixing_matrix(regime_mixing,
    show_values = TRUE,
    diagonal_emphasis = TRUE
) +
    labs(
        title = "Alliance Patterns by Regime Type",
        subtitle = "How different political systems interact",
        x = "Partner regime type",
        y = "Country regime type"
    )

print(p1)
ggsave("regime_mixing_example.png", p1, width = 8, height = 7)

# Example 2: Regional Mixing with Custom Colors
cat("\n=== Example 2: Regional Alliance Patterns ===\n")

region_mixing <- mixing_matrix(
    alliance_network,
    attribute = "region",
    normalized = TRUE,
    by_row = TRUE # Row normalization
)

p2 <- plot_mixing_matrix(region_mixing,
    show_values = TRUE,
    value_digits = 2,
    color_scale = c("white", "lightgreen", "darkgreen"),
    diagonal_emphasis = TRUE
) +
    labs(
        title = "Regional Alliance Patterns",
        subtitle = "Row-normalized: Each row sums to 1.0",
        x = "Allied with region",
        y = "From region"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)
ggsave("region_mixing_example.png", p2, width = 9, height = 8)

# Example 3: Cross-Dimensional Analysis
cat("\n=== Example 3: Cross-Dimensional Mixing ===\n")

# How do different regions ally with different regime types?
cross_mixing <- mixing_matrix(
    alliance_network,
    attribute = "regime_type",
    row_attribute = "region",
    normalized = TRUE
)

p3 <- plot_mixing_matrix(cross_mixing,
    show_values = TRUE,
    text_size = 3.5,
    color_scale = c("#FFF3E0", "#FFB74D", "#E65100")
) +
    labs(
        title = "Cross-Dimensional Alliance Patterns",
        subtitle = "How regions interact with different regime types",
        x = "Regime type of partner",
        y = "Region"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)
ggsave("cross_mixing_example.png", p3, width = 10, height = 8)

# Example 4: Development Level Mixing
cat("\n=== Example 4: Development Level Patterns ===\n")

dev_mixing <- mixing_matrix(
    alliance_network,
    attribute = "development",
    normalized = TRUE
)

# Reorder categories for better visualization
p4 <- plot_mixing_matrix(dev_mixing,
    show_values = TRUE,
    diagonal_emphasis = TRUE,
    reorder_categories = FALSE
) +
    labs(
        title = "Economic Development and Alliance Formation",
        subtitle = "Do countries ally with similar development levels?",
        x = "Partner development level",
        y = "Country development level"
    )

print(p4)
ggsave("development_mixing_example.png", p4, width = 8, height = 7)

cat("\n✓ All mixing matrix visualizations created!\n")
cat("Generated files:\n")
cat("- regime_mixing_example.png\n")
cat("- region_mixing_example.png\n")
cat("- cross_mixing_example.png\n")
cat("- development_mixing_example.png\n")
