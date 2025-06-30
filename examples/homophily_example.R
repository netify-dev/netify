# Example: Analyzing and Visualizing Homophily in Networks

library(netify)
library(ggplot2)

# Create example network data
set.seed(42)
n_actors <- 50

# Generate node attributes
node_data <- data.frame(
    actor = paste0("country_", 1:n_actors),
    gdp_per_capita = exp(rnorm(n_actors, 10, 1)), # Log-normal distribution
    democracy_score = runif(n_actors, 0, 10), # Democracy index 0-10
    region = sample(c("Americas", "Europe", "Asia", "Africa"), n_actors,
        replace = TRUE, prob = c(0.2, 0.3, 0.3, 0.2)
    ),
    stringsAsFactors = FALSE
)

# Create network with homophily
# Countries with similar GDP and democracy scores are more likely to connect
adjacency <- matrix(0, n_actors, n_actors)
for (i in 1:(n_actors - 1)) {
    for (j in (i + 1):n_actors) {
        # Calculate similarity
        gdp_sim <- -abs(log(node_data$gdp_per_capita[i]) - log(node_data$gdp_per_capita[j]))
        dem_sim <- -abs(node_data$democracy_score[i] - node_data$democracy_score[j]) / 10

        # Higher probability if same region
        region_bonus <- ifelse(node_data$region[i] == node_data$region[j], 0.3, 0)

        # Connection probability
        prob <- 0.1 + 0.2 * exp(gdp_sim) + 0.2 * exp(5 * dem_sim) + region_bonus
        prob <- min(prob, 0.8) # Cap at 80%

        adjacency[i, j] <- adjacency[j, i] <- rbinom(1, 1, prob)
    }
}
rownames(adjacency) <- colnames(adjacency) <- node_data$actor

# Create netify object
trade_network <- netify(adjacency, actor1 = NULL, actor2 = NULL, symmetric = TRUE)
trade_network <- add_node_vars(trade_network, node_data, actor = "actor")

# Analyze homophily for different attributes
cat("=== Analyzing Homophily in Trade Networks ===\n\n")

# 1. GDP homophily (continuous variable)
gdp_homophily <- homophily(
    trade_network,
    attribute = "gdp_per_capita",
    method = "correlation",
    significance_test = TRUE
)

cat("GDP Per Capita Homophily:\n")
print(gdp_homophily[, c("homophily_correlation", "p_value", "ci_lower", "ci_upper")])

# 2. Democracy homophily (continuous variable)
democracy_homophily <- homophily(
    trade_network,
    attribute = "democracy_score",
    method = "correlation",
    significance_test = TRUE
)

cat("\nDemocracy Score Homophily:\n")
print(democracy_homophily[, c("homophily_correlation", "p_value", "ci_lower", "ci_upper")])

# 3. Regional homophily (categorical variable)
region_homophily <- homophily(
    trade_network,
    attribute = "region",
    method = "categorical",
    significance_test = TRUE
)

cat("\nRegional Homophily:\n")
print(region_homophily[, c("homophily_correlation", "p_value", "ci_lower", "ci_upper")])

# Visualize results

# Distribution plot for democracy
cat("\n=== Creating Visualizations ===\n")

p1 <- plot_homophily(
    democracy_homophily,
    trade_network,
    type = "distribution",
    attribute = "democracy_score",
    method = "correlation"
) +
    labs(
        title = "Democracy Score Homophily in Trade Networks",
        subtitle = "Countries with similar democracy scores are more likely to trade"
    )

print(p1)

# Comparison plot
all_results <- rbind(gdp_homophily, democracy_homophily, region_homophily)

p2 <- plot_homophily(
    all_results,
    type = "comparison"
) +
    labs(
        title = "What Drives Trade Partnerships?",
        subtitle = "Comparing homophily across different country attributes"
    )

print(p2)

# Interpretation
cat("\n=== Interpretation ===\n")
if (region_homophily$p_value < 0.05) {
    cat("- Regional clustering is significant in trade networks\n")
}
if (democracy_homophily$p_value < 0.05) {
    cat("- Countries with similar political systems tend to trade more\n")
}
if (gdp_homophily$p_value < 0.05) {
    cat("- Economic similarity influences trade partnerships\n")
}
