# Calculate actor-level network statistics

Computes comprehensive actor-level statistics for netify objects,
including degree, strength, centrality measures, and custom metrics.
Handles different network types (directed/undirected,
weighted/unweighted) appropriately.

## Usage

``` r
summary_actor(netlet, invert_weights_for_igraph = TRUE, other_stats = NULL)
```

## Arguments

- netlet:

  A netify object containing network data

- invert_weights_for_igraph:

  Logical. If TRUE (default), inverts edge weights when calculating
  closeness and betweenness centrality, as igraph interprets weights as
  distances. Set to FALSE if your weights already represent distances.

- other_stats:

  Optional named list of custom functions to calculate additional
  actor-level statistics. Each function should accept a matrix and
  return a vector with one value per actor.

## Value

A data frame with actor-level statistics. Columns always include:

- `actor`:

  Actor name/identifier

- `time`:

  Time period (for longitudinal networks)

- `layer`:

  Layer name (for multilayer networks)

Additional columns depend on network type:

**For undirected networks:**

- `degree`:

  Number of connections. Calculated as \\d_i = \sum\_{j=1}^{n}
  a\_{ij}\\, where \\a\_{ij}\\ is the adjacency matrix element.

- `prop_ties`:

  Proportion of possible ties realized. Calculated as \\p_i =
  \frac{d_i}{n-1}\\, where \\d_i\\ is the degree and \\n\\ is the total
  number of actors.

- `net_share`:

  Actor's share of total network connections. Calculated as \\s_i =
  \frac{d_i}{\sum\_{j=1}^{n} d_j}\\.

- `closeness`:

  Closeness centrality. Calculated as \\C_i = \frac{1}{\sum\_{j} d(i,
  j)}\\, where \\d(i, j)\\ is the shortest path distance to every other
  actor \\j\\.

- `betweenness`:

  Betweenness centrality. Calculated as \\B_i = \sum\_{s \neq i \neq t}
  \frac{\sigma\_{st}(i)}{\sigma\_{st}}\\, where \\\sigma\_{st}\\ is the
  total number of shortest paths from node \\s\\ to node \\t\\ and
  \\\sigma\_{st}(i)\\ is the number of those paths that pass through
  \\i\\.

- `eigen_centrality`:

  Eigenvector centrality, based on the principal eigenvector of the
  adjacency matrix.

**For directed networks:**

- `in_degree`, `out_degree`:

  Incoming and outgoing connections. \\d_i^{in} = \sum\_{j=1}^{n}
  a\_{ji}\\ and \\d_i^{out} = \sum\_{j=1}^{n} a\_{ij}\\.

- `in_prop_ties`, `out_prop_ties`:

  Proportion of possible in/out ties

- `total_degree`:

  Sum of in and out degree

- `in_closeness`, `out_closeness`:

  Directed closeness centrality

- `betweenness`:

  Betweenness centrality

- `authority`, `hub`:

  Authority and hub scores (asymmetric only)

**For weighted networks, additional columns:**

- `strength_sum`:

  Sum of edge weights. For undirected: \\s_i^{sum} = \sum\_{j=1}^{n}
  w\_{ij}\\. For directed: separate in/out sums.

- `strength_avg`:

  Average edge weight. Calculated as \\s_i^{avg} =
  \frac{s_i^{sum}}{d_i}\\.

- `strength_sd`:

  Standard deviation of edge weights. Calculated as \\s_i^{sd} =
  \sqrt{\frac{1}{d_i} \sum\_{j=1}^{n} (w\_{ij} - s_i^{avg})^2}\\.

- `strength_median`:

  Median edge weight

## Details

The function automatically adapts calculations based on network
properties:

**Centrality Measures:**

- **Degree**: Count of direct connections. For directed networks,
  calculated separately for incoming (in-degree) and outgoing
  (out-degree) ties.

- **Closeness**: Measures how quickly an actor can reach all others.
  Based on the inverse of the sum of shortest path distances.

- **Betweenness**: Measures how often an actor lies on shortest paths
  between other actors, indicating brokerage potential.

- **Eigenvector**: Measures importance based on connections to other
  important actors. Computed using the principal eigenvector of the
  adjacency matrix.

- **Authority/Hub**: For directed networks only. Authority scores
  measure importance as targets of ties from important sources. Hub
  scores measure importance as sources of ties to important targets.

**Weight Handling:**

By default, the function assumes larger weights indicate stronger
relationships. When calculating closeness and betweenness centrality,
weights are inverted (1/weight) because igraph treats edge weights as
distances. For distance-based networks where larger values already
represent distances or weaker relationships, set
`invert_weights_for_igraph = FALSE`.

**Custom Statistics:**

Add custom metrics using the `other_stats` parameter. Each function
receives the adjacency matrix and should return a vector with one value
per actor:

    # Example: Maximum tie weight for each actor
    max_out <- function(mat) apply(mat, 1, max, na.rm = TRUE)
    max_in <- function(mat) apply(mat, 2, max, na.rm = TRUE)

    stats <- summary_actor(net,
      other_stats = list(max_out = max_out, max_in = max_in))

**Mathematical Formulations:**

For symmetric unweighted networks:

- Degree: \\d_i = \sum\_{j=1}^{n} a\_{ij}\\

- Proportion of ties: \\p_i = \frac{d_i}{n-1}\\

- Network share: \\s_i = \frac{d_i}{\sum\_{j=1}^{n} d_j}\\

- Closeness: \\C_i = \frac{1}{\sum\_{j} d(i, j)}\\

- Betweenness: \\B_i = \sum\_{s \neq i \neq t}
  \frac{\sigma\_{st}(i)}{\sigma\_{st}}\\

For symmetric weighted networks, additional measures:

- Strength sum: \\s_i^{sum} = \sum\_{j=1}^{n} w\_{ij}\\

- Strength average: \\s_i^{avg} = \frac{s_i^{sum}}{d_i}\\

- Strength standard deviation: \\s_i^{sd} = \sqrt{\frac{1}{d_i}
  \sum\_{j=1}^{n} (w\_{ij} - s_i^{avg})^2}\\

For asymmetric networks, statistics are calculated separately for rows
(out) and columns (in), with totals where applicable.

## Note

For longitudinal networks, statistics are calculated separately for each
time period. For multilayer networks, statistics are calculated
separately for each layer unless layers have been aggregated beforehand.

Missing values (NA) in the network are excluded from calculations.
Isolates (actors with no connections) receive appropriate values (0 for
degree, NA for some centrality measures).

The function handles both cross-sectional and longitudinal data
structures, as well as single-layer and multilayer networks. For ego
networks created with netify, the function appropriately handles the
ego-alter structure.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Basic usage with directed network
net <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Get actor statistics
actor_stats <- summary_actor(net)
head(actor_stats)
#>         actor time degree_in degree_out degree_total prop_ties_in prop_ties_out
#> 1 Afghanistan 2002        92         81          173    0.6052632     0.5328947
#> 2     Albania 2002        46         49           95    0.3026316     0.3223684
#> 3     Algeria 2002        68         65          133    0.4473684     0.4276316
#> 4      Angola 2002        64         67          131    0.4210526     0.4407895
#> 5   Argentina 2002        48         48           96    0.3157895     0.3157895
#> 6     Armenia 2002        71         66          137    0.4671053     0.4342105
#>   prop_ties_total network_share_in network_share_out network_share_total
#> 1               1      0.022594950       0.019334694         0.020964822
#> 2               1      0.002272838       0.002097149         0.002184994
#> 3               1      0.002679814       0.002497454         0.002588634
#> 4               1      0.002915549       0.002470767         0.002693158
#> 5               1      0.003173523       0.002804354         0.002988938
#> 6               1      0.005926727       0.005875577         0.005901152
#>   closeness_in closeness_out closeness_all  betweenness authority_score
#> 1     71.13409      68.44904      78.06787 1.324503e-02      0.26866096
#> 2     42.48817      43.64733      47.16201 0.000000e+00      0.01252200
#> 3     44.78256      39.95772      47.41426 0.000000e+00      0.01406673
#> 4     51.35489      46.60123      54.85391 8.830022e-05      0.01905067
#> 5     63.91481      61.26657      69.47750 0.000000e+00      0.03580770
#> 6     65.51012      63.04273      71.46157 0.000000e+00      0.05049222
#>     hub_score strength_sum_in strength_sum_out strength_sum_total
#> 1 0.183233794           10160             8694              18854
#> 2 0.009730031            1022              943               1965
#> 3 0.010671854            1205             1123               2328
#> 4 0.011275109            1311             1111               2422
#> 5 0.024732267            1427             1261               2688
#> 6 0.039622725            2665             2642               5307
#>   strength_avg_in strength_avg_out strength_avg_total strength_std_in
#> 1       67.284768        57.576159          62.430464       227.24232
#> 2        6.768212         6.245033           6.506623        22.27089
#> 3        7.980132         7.437086           7.708609        18.93197
#> 4        8.682119         7.357616           8.019868        24.29633
#> 5        9.450331         8.350993           8.900662        45.34294
#> 6       17.649007        17.496689          17.572848        66.88569
#>   strength_std_out strength_std_total strength_median_in strength_median_out
#> 1        198.98323          213.11277                  2                   1
#> 2         21.17104           21.72096                  0                   0
#> 3         17.00179           17.96688                  0                   0
#> 4         20.18988           22.24310                  0                   0
#> 5         39.54933           42.44614                  0                   0
#> 6         62.13790           64.51180                  0                   0
#>   strength_median_total
#> 1                   1.5
#> 2                   0.0
#> 3                   0.0
#> 4                   0.0
#> 5                   0.0
#> 6                   0.0

# Add custom statistics
# Maximum incoming and outgoing tie weights
max_out <- function(mat) apply(mat, 1, max, na.rm = TRUE)
max_in <- function(mat) apply(mat, 2, max, na.rm = TRUE)

actor_stats_custom <- summary_actor(
    net,
    other_stats = list(
        max_out = max_out,
        max_in = max_in
    )
)
head(actor_stats_custom)
#>         actor time degree_in degree_out degree_total prop_ties_in prop_ties_out
#> 1 Afghanistan 2002        92         81          173    0.6052632     0.5328947
#> 2     Albania 2002        46         49           95    0.3026316     0.3223684
#> 3     Algeria 2002        68         65          133    0.4473684     0.4276316
#> 4      Angola 2002        64         67          131    0.4210526     0.4407895
#> 5   Argentina 2002        48         48           96    0.3157895     0.3157895
#> 6     Armenia 2002        71         66          137    0.4671053     0.4342105
#>   prop_ties_total network_share_in network_share_out network_share_total
#> 1               1      0.022594950       0.019334694         0.020964822
#> 2               1      0.002272838       0.002097149         0.002184994
#> 3               1      0.002679814       0.002497454         0.002588634
#> 4               1      0.002915549       0.002470767         0.002693158
#> 5               1      0.003173523       0.002804354         0.002988938
#> 6               1      0.005926727       0.005875577         0.005901152
#>   closeness_in closeness_out closeness_all  betweenness authority_score
#> 1     71.13409      68.44904      78.06787 1.324503e-02      0.26866096
#> 2     42.48817      43.64733      47.16201 0.000000e+00      0.01252200
#> 3     44.78256      39.95772      47.41426 0.000000e+00      0.01406673
#> 4     51.35489      46.60123      54.85391 8.830022e-05      0.01905067
#> 5     63.91481      61.26657      69.47750 0.000000e+00      0.03580770
#> 6     65.51012      63.04273      71.46157 0.000000e+00      0.05049222
#>     hub_score strength_sum_in strength_sum_out strength_sum_total
#> 1 0.183233794           10160             8694              18854
#> 2 0.009730031            1022              943               1965
#> 3 0.010671854            1205             1123               2328
#> 4 0.011275109            1311             1111               2422
#> 5 0.024732267            1427             1261               2688
#> 6 0.039622725            2665             2642               5307
#>   strength_avg_in strength_avg_out strength_avg_total strength_std_in
#> 1       67.284768        57.576159          62.430464       227.24232
#> 2        6.768212         6.245033           6.506623        22.27089
#> 3        7.980132         7.437086           7.708609        18.93197
#> 4        8.682119         7.357616           8.019868        24.29633
#> 5        9.450331         8.350993           8.900662        45.34294
#> 6       17.649007        17.496689          17.572848        66.88569
#>   strength_std_out strength_std_total strength_median_in strength_median_out
#> 1        198.98323          213.11277                  2                   1
#> 2         21.17104           21.72096                  0                   0
#> 3         17.00179           17.96688                  0                   0
#> 4         20.18988           22.24310                  0                   0
#> 5         39.54933           42.44614                  0                   0
#> 6         62.13790           64.51180                  0                   0
#>   strength_median_total max_out max_in
#> 1                   1.5    1516   1847
#> 2                   0.0     160    157
#> 3                   0.0      91    114
#> 4                   0.0     147    176
#> 5                   0.0     424    469
#> 6                   0.0     547    609

# For networks where weights represent distances
# (larger values = weaker relationships)
distance_net <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    weight = "matlConf" # conflict measure
)
#> ✖ Warning: there are repeating dyads within time periods in the dataset. When `sum_dyads = FALSE` and `weight` variable is supplied but there are repeating dyads in the dataset, we cannot uniquely identify edges. Try sum_dyads=TRUE or remove repeating dyads.

# Don't invert weights for centrality calculations
actor_stats_dist <- summary_actor(
    distance_net,
    invert_weights_for_igraph = FALSE
)
```
