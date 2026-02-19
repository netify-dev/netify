# Visualize actor-level network statistics

`plot_actor_stats` creates visualizations of actor-level statistics from
[`summary_actor()`](https://netify-dev.github.io/netify/reference/summary_actor.md)
output. The function automatically adapts to the data structure
(cross-sectional/longitudinal, single/multilayer) and offers two main
visualization approaches: distribution across actors or tracking
specific actors.

## Usage

``` r
plot_actor_stats(
  summary_df,
  longitudinal = ifelse("time" %in% colnames(summary_df), TRUE, FALSE),
  multilayer = ifelse("layer" %in% colnames(summary_df), TRUE, FALSE),
  across_actor = TRUE,
  specific_stats = NULL,
  specific_actors = NULL
)
```

## Arguments

- summary_df:

  A data frame from
  [`summary_actor()`](https://netify-dev.github.io/netify/reference/summary_actor.md)
  containing actor-level statistics. Must include an "actor" column. May
  include "time" column for longitudinal data and "layer" column for
  multilayer networks.

- longitudinal:

  Logical indicating whether to treat data as longitudinal. If NULL
  (default), automatically detected based on presence of "time" column.
  Set to FALSE if only one unique time point exists.

- multilayer:

  Logical indicating whether to treat data as multilayer. If NULL
  (default), automatically detected based on presence of "layer" column.
  Set to FALSE if only one unique layer exists.

- across_actor:

  Logical. If TRUE (default), visualizes distribution of statistics
  across all actors. If FALSE, focuses on tracking specific actors. When
  TRUE with `specific_actors` provided, shows distribution for only
  those actors.

- specific_stats:

  Character vector of statistic names to plot. If NULL (default), plots
  all available statistics. Must match column names in `summary_df`.

- specific_actors:

  Character vector of actor names to highlight or focus on. If NULL
  (default) with `across_actor = FALSE`, includes all actors (with
  warning if \> 25 actors). Must match values in the "actor" column.

## Value

A ggplot object that can be further customized or saved. The plot type
depends on the data structure and parameters:

- **Cross-sectional, across actors**: Density plots with rug plots

- **Cross-sectional, specific actors**: Beeswarm plots

- **Longitudinal, across actors**: Ridge density plots over time

- **Longitudinal, specific actors**: Line plots over time

All plots are faceted by statistic and, when applicable, by layer.

## Details

**Visualization logic:**

The function chooses appropriate visualizations based on data structure:

- **Distribution plots** (`across_actor = TRUE`): Show how statistics
  are distributed across the actor population

- **Actor-specific plots** (`across_actor = FALSE`): Track individual
  actors, with specified actors highlighted in color while others appear
  in gray

All plots use
[`theme_stat_netify()`](https://netify-dev.github.io/netify/reference/theme_stat_netify.md)
for consistent styling across netify visualizations.

For multilayer longitudinal data with `across_actor = FALSE`, consider
using `specific_stats` to avoid overcrowded facets.

## Author

Cassy Dorff, Shahryar Minhas
