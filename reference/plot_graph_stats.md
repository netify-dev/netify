# Visualize network-level statistics

`plot_graph_stats` creates line or bar plots to visualize network-level
statistics across multiple networks, time points, or layers. this
function is designed to work with output from
[`summary()`](https://rdrr.io/r/base/summary.html) for netify objects or
similar functions that produce network-level summary statistics.

## Usage

``` r
plot_graph_stats(summary_df, type = "line", specific_stats = NULL)
```

## Arguments

- summary_df:

  a data frame containing network-level statistics, typically from
  [`summary()`](https://rdrr.io/r/base/summary.html) for a netify
  object. must include a "net" column identifying each network or time
  point. may include a "layer" column for multilayer networks. all other
  columns should contain numeric statistics to plot.

- type:

  character string specifying the plot type. options are:

  - `"line"`: line plot with points (default). best for temporal data

  - `"bar"`: bar plot with grouped bars. required for multilayer
    non-temporal data

- specific_stats:

  character vector of statistic names to plot. if NULL (default), plots
  all numeric columns in summary_df. must match column names exactly.

## Value

a ggplot object displaying the specified statistics. the plot structure
depends on the data:

- **single time/network**: returns error (single row not plottable)

- **multiple times/networks**: line or bar plot faceted by statistic

- **multilayer temporal**: line plots colored by layer

- **multilayer non-temporal**: grouped bar plots by layer

all plots are faceted by statistic with free y-axis scales for better
comparison across different value ranges.

## Details

**data structure detection:**

the function automatically detects the structure of your data:

- **longitudinal**: multiple unique values in "net" column

- **multilayer**: multiple unique values in "layer" column

- **single network**: only one row (returns error with suggestion)

**plot type selection:**

- line plots are preferred for temporal data to show trends

- bar plots are automatically selected for multilayer non-temporal data

- bar plots can be useful for comparing discrete time points

**faceting behavior:**

each statistic gets its own facet panel with:

- independent y-axis scales (scales = "free_y")

- shared x-axis across all panels

- automatic layout based on number of statistics

## Note

the function requires at least two networks/time points to create a
meaningful plot. for single network summaries, consider using a table
format instead.

## Author

ha eun choi, cassy dorff, shahryar minhas
