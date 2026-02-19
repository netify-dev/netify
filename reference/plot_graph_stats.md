# Visualize network-level statistics

`plot_graph_stats` creates line or bar plots to visualize network-level
statistics across multiple networks, time points, or layers. This
function is designed to work with output from `summary_net()` or similar
functions that produce network-level summary statistics.

## Usage

``` r
plot_graph_stats(summary_df, type = "line", specific_stats = NULL)
```

## Arguments

- summary_df:

  A data frame containing network-level statistics, typically from
  `summary_net()`. Must include a "net" column identifying each network
  or time point. May include a "layer" column for multilayer networks.
  All other columns should contain numeric statistics to plot.

- type:

  Character string specifying the plot type. Options are:

  - `"line"`: Line plot with points (default). Best for temporal data

  - `"bar"`: Bar plot with grouped bars. Required for multilayer
    non-temporal data

- specific_stats:

  Character vector of statistic names to plot. If NULL (default), plots
  all numeric columns in summary_df. Must match column names exactly.

## Value

A ggplot object displaying the specified statistics. The plot structure
depends on the data:

- **Single time/network**: Returns error (single row not plottable)

- **Multiple times/networks**: Line or bar plot faceted by statistic

- **Multilayer temporal**: Line plots colored by layer

- **Multilayer non-temporal**: Grouped bar plots by layer

All plots are faceted by statistic with free y-axis scales for better
comparison across different value ranges.

## Details

**Data structure detection:**

The function automatically detects the structure of your data:

- **Longitudinal**: Multiple unique values in "net" column

- **Multilayer**: Multiple unique values in "layer" column

- **Single network**: Only one row (returns error with suggestion)

**Plot type selection:**

- Line plots are preferred for temporal data to show trends

- Bar plots are automatically selected for multilayer non-temporal data

- Bar plots can be useful for comparing discrete time points

**Faceting behavior:**

Each statistic gets its own facet panel with:

- Independent y-axis scales (scales = "free_y")

- Shared x-axis across all panels

- Automatic layout based on number of statistics

## Note

The function requires at least two networks/time points to create a
meaningful plot. For single network summaries, consider using a table
format instead.

## Author

Ha Eun Choi, Cassy Dorff, Shahryar Minhas
