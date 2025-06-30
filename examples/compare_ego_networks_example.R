# Example: Better way to compare ego networks with compare_networks

library(netify)

# Load example data
data(icews)

# Create network
netlet <- netify(
    icews,
    actor1 = "i", actor2 = "j", 
    time = "year",
    weight = "verbCoop",
    nodal_vars = c("i_polity2", "i_log_gdp", 'i_region')
)

# Extract ego networks for US and China
us_ego <- ego_netify(netlet, ego = "United States")
china_ego <- ego_netify(netlet, ego = "China")

# Option 1: Compare specific years
# This is clearer because it compares single networks
us_2012 <- subset(us_ego, time = "2012")
china_2012 <- subset(china_ego, time = "2012")

cat("=== Comparing US and China ego networks for 2012 ===\n")
comp_2012 <- compare_networks(
    list("US_2012" = us_2012, "China_2012" = china_2012), 
    what = 'structure'
)
print(comp_2012$summary)

# Option 2: Compare averages across time periods
# Get summaries for each country
us_summary <- summary(us_ego)
china_summary <- summary(china_ego)

cat("\n=== Average ego network properties (2002-2014) ===\n")
avg_comparison <- data.frame(
    Country = c("United States", "China"),
    Avg_Density = c(
        mean(us_summary$density, na.rm = TRUE),
        mean(china_summary$density, na.rm = TRUE)
    ),
    Avg_Transitivity = c(
        mean(us_summary$transitivity, na.rm = TRUE),
        mean(china_summary$transitivity, na.rm = TRUE)
    ),
    Avg_Mean_Degree = c(
        mean(us_summary$mean_degree, na.rm = TRUE),
        mean(china_summary$mean_degree, na.rm = TRUE)
    ),
    Avg_Num_Actors = c(
        mean(us_summary$num_actors, na.rm = TRUE),
        mean(china_summary$num_actors, na.rm = TRUE)
    )
)
print(avg_comparison)

# Option 3: Year-by-year comparison
# Extract specific years and compare them
years_to_compare <- 2010:2012
year_comparisons <- list()

for (year in years_to_compare) {
    us_year <- subset(us_ego, time = as.character(year))
    china_year <- subset(china_ego, time = as.character(year))
    
    comp <- compare_networks(
        list("US" = us_year, "China" = china_year),
        what = "structure"
    )
    
    # Store results with year label
    year_comparisons[[as.character(year)]] <- comp$summary
}

cat("\n=== Year-by-year structural comparison ===\n")
for (year in names(year_comparisons)) {
    cat("\nYear", year, ":\n")
    print(year_comparisons[[year]])
}

# Option 4: Track evolution within each ego network
cat("\n=== Temporal evolution of US ego network ===\n")
us_temporal <- compare_networks(us_ego, what = "structure")
# This shows how US ego network changes over time
print(head(us_temporal$summary))

cat("\n=== Temporal evolution of China ego network ===\n")
china_temporal <- compare_networks(china_ego, what = "structure")
print(head(china_temporal$summary))

# Option 5: Edge-level comparison for specific year
cat("\n=== Edge-level comparison for 2012 ===\n")
edge_comp_2012 <- compare_networks(
    list("US" = us_2012, "China" = china_2012),
    what = "edges",
    method = "all"
)
print(edge_comp_2012$summary)

cat("\nInterpretation tips:
- When comparing longitudinal ego networks, subset to specific years first
- Use summary() to get average properties across all years
- compare_networks() on a single longitudinal network shows temporal evolution
- For cross-country comparison, align time periods for meaningful results
")