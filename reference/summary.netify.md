# Calculate graph-level statistics for netify objects

computes graph-level statistics for netify objects, including density,
reciprocity, centralization measures, and custom metrics. handles
cross-sectional and longitudinal networks, as well as multilayer
structures.

## Usage

``` r
# S3 method for class 'netify'
summary(object, ...)
```

## Arguments

- object:

  a netify object containing network data

- ...:

  additional arguments, including:

  `other_stats`

  :   named list of custom functions to calculate additional graph-level
      statistics. each function should accept the current netify slice
      and return a named vector of scalar values.

## Value

a data frame with one row per network/time period containing:

**basic network properties:**

- `net`:

  network/time identifier

- `layer`:

  layer name (for multilayer networks)

- `num_actors`:

  number of actors (or `num_row_actors` and `num_col_actors` for
  bipartite networks)

- `density`:

  proportion of possible ties that exist

- `num_edges`:

  total number of edges (count of every non-zero entry; for signed
  networks, negative-weight ties are included alongside positive ones)

- `prop_edges_missing`:

  proportion of potential edge dyads that are na. uses the same
  denominator as `density` (off-diagonal cells for unipartite with
  `diag_to_NA`, halved for symmetric networks, all cells for bipartite),
  so `density + prop_edges_missing + observed_zero_fraction = 1`.

- `prop_unknown_edges`:

  proportion of potential edges that are na because they were
  *unobserved* at the data-entry stage (as opposed to structurally
  absent or on-diagonal). only appears when the netlet was built with
  `missing_to_zero = FALSE`; otherwise unobserved dyads have been filled
  with 0 and the column would be identically zero.

**for weighted networks only:**

- `mean_edge_weight`:

  average weight of realized non-zero edges. signed networks include
  negative ties; observed zero non-ties and missing dyads are excluded.

- `sd_edge_weight`:

  standard deviation of realized non-zero edge weights

- `median_edge_weight`:

  median realized non-zero edge weight

- `min_edge_weight`, `max_edge_weight`:

  range of realized non-zero edge weights

**structural measures:**

- `competition` (or `competition_row`/`competition_col`):

  herfindahl-hirschman index measuring concentration of ties. calculated
  as \\\sum\_{i=1}^{n} (s_i)^2\\ where \\s_i\\ is actor i's share of
  total ties. ranges from 1/n (equal distribution) to 1 (one actor has
  all ties).

- `sd_of_actor_means` (or `sd_of_row_means`/`sd_of_col_means`):

  standard deviation of actors' average tie strengths, measuring
  heterogeneity in actor activity levels

- `transitivity`:

  global clustering coefficient (probability that two neighbors of a
  node are connected). returns `na` when the coefficient is undefined.

**for directed networks only:**

- `covar_of_row_col_means`:

  correlation between actors' sending and receiving means. the column
  name is retained for compatibility.

- `reciprocity`:

  pearson correlation between the adjacency matrix and its transpose.
  this measures the linear association between outgoing and incoming tie
  weights for each dyad (i.e., how similar \\a\_{ij}\\ is to \\a\_{ji}\\
  across all dyads). values near 1 indicate strong reciprocity, while
  values near 0 indicate no relationship. note: this differs from the
  traditional edge-based reciprocity (proportion of mutual dyads) used
  by igraph and other packages.

- `mutual`:

  proportion of mutual dyads: the fraction of connected dyad pairs where
  both directions are present. ranges from 0 (no mutual ties) to 1 (all
  ties are reciprocated). this is the traditional edge-based reciprocity
  measure used by igraph and most network analysis textbooks.

## Details

the function automatically adapts calculations based on network
properties:

- **bipartite networks**: reports row and column actors separately

- **directed networks**: calculates separate statistics for in/out ties

- **weighted networks**: includes weight-based statistics

- **multilayer networks**: processes each layer independently

- **longitudinal networks**: calculates statistics for each time period

**competition index interpretation:**

the competition measure (hhi) captures how concentrated network ties are
among actors. low values indicate distributed activity across many
actors (competitive), while high values indicate concentration among few
actors (monopolistic). this is particularly useful for analyzing power
dynamics or resource distribution in networks.

**custom statistics:**

add custom graph-level metrics using the `other_stats` parameter:


    # example: community detection
    modularity_stat <- function(net) {
      g <- netify_to_igraph(net)
      comm <- igraph::cluster_walktrap(g)
      c(modularity = igraph::modularity(comm),
        n_communities = length(unique(comm$membership)))
    }

    summary(net, other_stats = list(community = modularity_stat))

## Note

for large longitudinal or multilayer networks, computation can be
intensive. consider using
[`subset_netify`](https://netify-dev.github.io/netify/reference/subset_netify.md)
to analyze specific time periods or layers.

density uses the full potential-dyad denominator. missing edges (na
values) are not counted as present ties; they are reported separately in
`prop_edges_missing`.

## References

dorff, c., gallop, m., & minhas, s. (2023). "networks of violence:
predicting conflict in nigeria." *journal of politics*, 85(1).

## Author

cassy dorff, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# \donttest{
# basic usage
net <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# get summary
summary(net)
#>     net num_actors   density num_edges prop_edges_missing mean_edge_weight
#> 1  2002        152 0.3787034      8692                  0         51.73240
#> 2  2003        152 0.3871994      8887                  0         50.00934
#> 3  2004        152 0.4145173      9514                  0         47.88669
#> 4  2005        152 0.4071976      9346                  0         48.61245
#> 5  2006        152 0.4108139      9429                  0         51.34044
#> 6  2007        152 0.4243203      9739                  0         51.60612
#> 7  2008        152 0.4351690      9988                  0         48.33480
#> 8  2009        152 0.4293308      9854                  0         47.35600
#> 9  2010        152 0.4346462      9976                  0         41.72113
#> 10 2011        152 0.4229697      9708                  0         37.33642
#> 11 2012        152 0.4287644      9841                  0         37.30637
#> 12 2013        152 0.4318142      9911                  0         39.86207
#> 13 2014        152 0.4258452      9774                  0         43.19153
#>    sd_edge_weight median_edge_weight min_edge_weight max_edge_weight
#> 1        230.1314                  8               1            6003
#> 2        229.0646                  7               1            5937
#> 3        210.7666                  7               1            5141
#> 4        229.9569                  6               1            6561
#> 5        246.6313                  7               1            7579
#> 6        246.2486                  7               1            7125
#> 7        221.5845                  7               1            6091
#> 8        236.4214                  6               1            6889
#> 9        198.3157                  6               1            4937
#> 10       172.2944                  5               1            5581
#> 11       161.8837                  6               1            4225
#> 12       196.2998                  5               1            6320
#> 13       206.6673                  6               1            6327
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
#>    covar_of_row_col_means reciprocity    mutual transitivity
#> 1               0.9946888   0.9778217 0.8537001    0.6058952
#> 2               0.9872959   0.9632488 0.8479933    0.6072045
#> 3               0.9923809   0.9769563 0.8452289    0.6215978
#> 4               0.9932157   0.9804325 0.8386779    0.6215075
#> 5               0.9909337   0.9771928 0.8510012    0.6277829
#> 6               0.9940837   0.9783703 0.8511690    0.6330626
#> 7               0.9916659   0.9694667 0.8468935    0.6401296
#> 8               0.9946612   0.9800665 0.8467016    0.6318319
#> 9               0.9934913   0.9823385 0.8402509    0.6386063
#> 10              0.9903188   0.9763723 0.8327355    0.6220630
#> 11              0.9877987   0.9702754 0.8336128    0.6337725
#> 12              0.9939394   0.9782226 0.8248941    0.6314429
#> 13              0.9904402   0.9730745 0.8389464    0.6297063
# }

if (FALSE) { # \dontrun{
# add custom statistics - community detection
comm_stats <- function(mat) {
    g <- netify_to_igraph(mat)
    comm <- igraph::cluster_spinglass(g)
    c(
        n_communities = length(comm$csize),
        modularity = comm$modularity
    )
}

# apply to subset for efficiency
sub_net <- subset_netify(net, time = as.character(2013:2014))
summary(sub_net, other_stats = list(community = comm_stats))
} # }
```
