# Calculate graph-level statistics for netify objects

Computes comprehensive graph-level statistics for netify objects,
including density, reciprocity, centralization measures, and custom
metrics. Handles cross-sectional and longitudinal networks, as well as
multilayer structures.

## Usage

``` r
# S3 method for class 'netify'
summary(object, ...)
```

## Arguments

- object:

  A netify object containing network data

- ...:

  Additional arguments, including:

  `other_stats`

  :   Named list of custom functions to calculate additional graph-level
      statistics. Each function should accept a matrix and return a
      named vector of scalar values.

## Value

A data frame with one row per network/time period containing:

**Basic network properties:**

- `net`:

  Network/time identifier

- `layer`:

  Layer name (for multilayer networks)

- `num_actors`:

  Number of actors (or `num_row_actors` and `num_col_actors` for
  bipartite networks)

- `density`:

  Proportion of possible ties that exist

- `num_edges`:

  Total number of edges (unweighted count)

- `prop_edges_missing`:

  Proportion of potential edges that are NA

**For weighted networks only:**

- `mean_edge_weight`:

  Average weight of existing edges

- `sd_edge_weight`:

  Standard deviation of edge weights

- `median_edge_weight`:

  Median edge weight

- `min_edge_weight`, `max_edge_weight`:

  Range of edge weights

**Structural measures:**

- `competition` (or `competition_row`/`competition_col`):

  Herfindahl-Hirschman Index measuring concentration of ties. Calculated
  as \\\sum\_{i=1}^{n} (s_i)^2\\ where \\s_i\\ is actor i's share of
  total ties. Ranges from 1/n (equal distribution) to 1 (one actor has
  all ties).

- `sd_of_actor_means` (or `sd_of_row_means`/`sd_of_col_means`):

  Standard deviation of actors' average tie strengths, measuring
  heterogeneity in actor activity levels

- `transitivity`:

  Global clustering coefficient (probability that two neighbors of a
  node are connected)

**For directed networks only:**

- `covar_of_row_col_means`:

  Covariance between actors' sending and receiving patterns

- `reciprocity`:

  Correlation between adjacency matrix and its transpose, measuring
  tendency for mutual ties

## Details

The function automatically adapts calculations based on network
properties:

- **Bipartite networks**: Reports row and column actors separately

- **Directed networks**: Calculates separate statistics for in/out ties

- **Weighted networks**: Includes weight-based statistics

- **Multilayer networks**: Processes each layer independently

- **Longitudinal networks**: Calculates statistics for each time period

**Competition Index Interpretation:**

The competition measure (HHI) captures how concentrated network ties are
among actors. Low values indicate distributed activity across many
actors (competitive), while high values indicate concentration among few
actors (monopolistic). This is particularly useful for analyzing power
dynamics or resource distribution in networks.

**Custom Statistics:**

Add custom graph-level metrics using the `other_stats` parameter:

    # Example: Community detection
    modularity_stat <- function(mat) {
      g <- netify_to_igraph(mat)
      comm <- igraph::cluster_walktrap(g)
      c(modularity = igraph::modularity(comm),
        n_communities = length(unique(comm$membership)))
    }

    summary(net, other_stats = list(community = modularity_stat))

## Note

For large longitudinal or multilayer networks, computation can be
intensive. Consider using
[`subset_netify`](https://netify-dev.github.io/netify/reference/subset_netify.md)
to analyze specific time periods or layers.

Missing edges (NA values) are excluded from density calculations but
tracked in the `prop_edges_missing` statistic.

## References

Dorff, C., Gallop, M., & Minhas, S. (2023). "Networks of violence:
Predicting conflict in Nigeria." *Journal of Politics*, 85(1).

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Basic usage
net <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# get summary
summary(net)
#>     net num_actors   density num_edges prop_edges_missing mean_edge_weight
#> 1  2002        152 0.3762119      8692                  0         19.59123
#> 2  2003        152 0.3846520      8887                  0         19.36358
#> 3  2004        152 0.4117902      9514                  0         19.84986
#> 4  2005        152 0.4045187      9346                  0         19.79488
#> 5  2006        152 0.4081111      9429                  0         21.09136
#> 6  2007        152 0.4215287      9739                  0         21.89753
#> 7  2008        152 0.4323061      9988                  0         21.03381
#> 8  2009        152 0.4265062      9854                  0         20.33139
#> 9  2010        152 0.4317867      9976                  0         18.13393
#> 10 2011        152 0.4201870      9708                  0         15.79217
#> 11 2012        152 0.4259436      9841                  0         15.99564
#> 12 2013        152 0.4289733      9911                  0         17.21301
#> 13 2014        152 0.4230436      9774                  0         18.39291
#>    sd_edge_weight median_edge_weight min_edge_weight max_edge_weight
#> 1        143.8213                  0               0            6003
#> 2        144.5982                  0               0            5937
#> 3        137.7292                  0               0            5141
#> 4        148.6667                  0               0            6561
#> 5        160.0782                  0               0            7579
#> 6        162.4166                  0               0            7125
#> 7        148.1206                  0               0            6091
#> 8        156.6702                  0               0            6889
#> 9        132.3670                  0               0            4937
#> 10       113.5584                  0               0            5581
#> 11       107.5946                  0               0            4225
#> 12       130.4925                  0               0            6320
#> 13       136.5412                  0               0            6327
#>    competition_row competition_col sd_of_row_means sd_of_col_means
#> 1       0.04093116      0.03813621        44.91530        43.04935
#> 2       0.04199822      0.03930188        45.07759        43.32783
#> 3       0.03481011      0.03262993        41.25503        39.63005
#> 4       0.04067046      0.03624472        45.20964        42.17312
#> 5       0.04151400      0.03697499        48.76301        45.48502
#> 6       0.03887627      0.03514325        48.67808        45.77854
#> 7       0.03785375      0.03421258        46.01192        43.25059
#> 8       0.04500583      0.03915054        49.29918        45.38806
#> 9       0.04071163      0.03493156        41.44120        37.76971
#> 10      0.03964994      0.03405077        35.52390        32.37731
#> 11      0.03587638      0.03146233        33.86660        31.21128
#> 12      0.04058869      0.03642385        39.26572        36.78299
#> 13      0.04058415      0.03521224        41.95447        38.49827
#>    covar_of_row_col_means reciprocity transitivity
#> 1               0.9946888   0.9778217    0.6058952
#> 2               0.9872959   0.9632488    0.6072045
#> 3               0.9923809   0.9769563    0.6215978
#> 4               0.9932157   0.9804325    0.6215075
#> 5               0.9909337   0.9771928    0.6277829
#> 6               0.9940837   0.9783703    0.6330626
#> 7               0.9916659   0.9694667    0.6401296
#> 8               0.9946612   0.9800665    0.6318319
#> 9               0.9934913   0.9823385    0.6386063
#> 10              0.9903188   0.9763723    0.6220630
#> 11              0.9877987   0.9702754    0.6337725
#> 12              0.9939394   0.9782226    0.6314429
#> 13              0.9904402   0.9730745    0.6297063

if (FALSE) { # \dontrun{
# Add custom statistics - community detection
comm_stats <- function(mat) {
    g <- netify_to_igraph(mat)
    comm <- igraph::cluster_spinglass(g)
    c(
        n_communities = length(comm$csize),
        modularity = comm$modularity
    )
}

# Apply to subset for efficiency
sub_net <- subset_netify(net, time = as.character(2013:2014))
summary(sub_net, other_stats = list(community = comm_stats))
} # }
```
