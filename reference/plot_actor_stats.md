# Visualize actor-level network statistics

`plot_actor_stats` creates visualizations of actor-level statistics from
[`summary_actor()`](https://netify-dev.github.io/netify/reference/summary_actor.md)
output. the function automatically adapts to the data structure
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

  a data frame from
  [`summary_actor()`](https://netify-dev.github.io/netify/reference/summary_actor.md)
  containing actor-level statistics. must include an "actor" column. may
  include "time" column for longitudinal data and "layer" column for
  multilayer networks.

- longitudinal:

  logical indicating whether to treat data as longitudinal. if NULL
  (default), automatically detected based on presence of "time" column.
  set to FALSE if only one unique time point exists.

- multilayer:

  logical indicating whether to treat data as multilayer. if NULL
  (default), automatically detected based on presence of "layer" column.
  set to FALSE if only one unique layer exists.

- across_actor:

  logical. if TRUE (default), visualizes distribution of statistics
  across all actors. if FALSE, focuses on tracking specific actors. when
  TRUE with `specific_actors` provided, shows distribution for only
  those actors.

- specific_stats:

  character vector of statistic names to plot. if NULL (default), plots
  all available statistics. must match column names in `summary_df`.

- specific_actors:

  character vector of actor names to highlight or focus on. if NULL
  (default) with `across_actor = FALSE`, includes all actors (with
  warning if \> 25 actors). must match values in the "actor" column.

## Value

a ggplot object that can be further customized or saved. the plot type
depends on the data structure and parameters:

- **cross-sectional, across actors**: density plots with rug plots

- **cross-sectional, specific actors**: beeswarm plots

- **longitudinal, across actors**: ridge density plots over time

- **longitudinal, specific actors**: line plots over time

all plots are faceted by statistic and, when applicable, by layer.

## Details

**visualization logic:**

the function chooses appropriate visualizations based on data structure:

- **distribution plots** (`across_actor = TRUE`): show how statistics
  are distributed across the actor population

- **actor-specific plots** (`across_actor = FALSE`): track individual
  actors, with specified actors highlighted in color while others appear
  in gray

all plots use
[`theme_stat_netify()`](https://netify-dev.github.io/netify/reference/theme_stat_netify.md)
for consistent styling across netify visualizations.

for multilayer longitudinal data with `across_actor = FALSE`, consider
using `specific_stats` to avoid overcrowded facets.

## Author

cassy dorff, shahryar minhas
