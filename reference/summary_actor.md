# Calculate actor-level network statistics

computes actor-level statistics for netify objects, including degree,
strength, centrality measures, and custom metrics. handles different
network types (directed/undirected, weighted/unweighted) appropriately.

## Usage

``` r
summary_actor(
  netlet,
  invert_weights_for_igraph = TRUE,
  other_stats = NULL,
  stats = c("all", "fast")
)
```

## Arguments

- netlet:

  a netify object containing network data

- invert_weights_for_igraph:

  logical. if TRUE (default), inverts edge weights when calculating
  closeness and betweenness centrality, as igraph interprets weights as
  distances. set to FALSE if your weights already represent distances.

- other_stats:

  optional named list of custom functions to calculate additional
  actor-level statistics. each function should accept a matrix and
  return a vector with one value per actor.

- stats:

  one of `"all"` (default) or `"fast"`. the `"fast"` path returns only
  degree- and strength-style columns and skips closeness, betweenness,
  eigenvector, and hits. when the user does not pass `stats` explicitly,
  the default auto-promotes to `"fast"` once the number of actors
  reaches `getoption("netify.fast_threshold", 1500l)`; passing
  `stats = "all"` explicitly always honors that request.

## Value

a data frame with actor-level statistics. columns always include:

- `actor`:

  actor name/identifier

- `time`:

  time period (for longitudinal networks)

- `layer`:

  layer name (for multilayer networks)

additional columns depend on network type:

**for undirected networks:**

- `degree`:

  number of realized non-zero connections. for weighted networks, degree
  remains a count; use strength columns for sums and moments of edge
  weights.

- `prop_ties`:

  proportion of possible ties realized. calculated as \\p_i =
  \frac{d_i}{n-1}\\, where \\d_i\\ is the degree and \\n\\ is the total
  number of actors.

- `network_share`:

  actor's share of total network connections. for unweighted networks
  this is based on degree; for weighted networks this is based on
  absolute realized tie mass, so signed networks do not produce negative
  shares.

- `closeness`:

  closeness centrality. calculated as \\c_i = \frac{1}{\sum\_{j} d(i,
  j)}\\, where \\d(i, j)\\ is the shortest path distance to every other
  actor \\j\\.

- `betweenness`:

  betweenness centrality. calculated as \\b_i = \sum\_{s \neq i \neq t}
  \frac{\sigma\_{st}(i)}{\sigma\_{st}}\\, where \\\sigma\_{st}\\ is the
  total number of shortest paths from node \\s\\ to node \\t\\ and
  \\\sigma\_{st}(i)\\ is the number of those paths that pass through
  \\i\\.

- `eigen_vector`:

  eigenvector centrality, based on the principal eigenvector of the
  adjacency matrix.

**for directed networks:**

- `degree_in`, `degree_out`:

  incoming and outgoing realized non-zero connection counts. for
  weighted networks, degree columns remain counts and strength columns
  summarize edge weights.

- `prop_ties_in`, `prop_ties_out`, `prop_ties_total`:

  proportion of possible in, out, and total ties.

- `network_share_in`, `network_share_out`, `network_share_total`:

  actor's share of incoming, outgoing, and total network connections.
  for weighted networks these shares use absolute realized tie mass.

- `degree_total`:

  sum of in and out degree

- `closeness_in`, `closeness_out`, `closeness_all`:

  directed closeness centrality

- `betweenness`:

  betweenness centrality

- `authority_score`, `hub_score`:

  authority and hub scores (asymmetric only)

**for weighted networks, additional columns:**

- `strength_sum`:

  sum of edge weights. directed networks return `strength_sum_in`,
  `strength_sum_out`, and `strength_sum_total`.

- `strength_avg`:

  average weight among realized non-zero ties. directed networks return
  `strength_avg_in`, `strength_avg_out`, and `strength_avg_total`.

- `strength_std`:

  standard deviation of weights among realized non-zero ties. directed
  networks return `strength_std_in`, `strength_std_out`, and
  `strength_std_total`.

- `strength_median`:

  median weight among realized non-zero ties. directed networks return
  `strength_median_in`, `strength_median_out`, and
  `strength_median_total`.

## Details

the function automatically adapts calculations based on network
properties:

**centrality measures:**

- **degree**: count of direct connections. for directed networks,
  calculated separately for incoming (in-degree) and outgoing
  (out-degree) ties.

- **closeness**: measures how quickly an actor can reach all others.
  based on the inverse of the sum of shortest path distances.

- **betweenness**: measures how often an actor lies on shortest paths
  between other actors, indicating brokerage potential.

- **eigenvector**: measures importance based on connections to other
  important actors. computed using the principal eigenvector of the
  adjacency matrix.

- **authority/hub**: for directed networks only. authority scores
  measure importance as targets of ties from important sources. hub
  scores measure importance as sources of ties to important targets.

**weight handling:**

by default, the function assumes larger weights indicate stronger
relationships. when calculating closeness and betweenness centrality,
weights are shifted to a positive scale when needed and inverted
(1/weight) because igraph treats edge weights as distances. for
distance-based networks where larger values already represent distances
or weaker relationships, set `invert_weights_for_igraph = FALSE`.
eigenvector, authority, and hub scores use the positive shifted strength
scale without distance inversion. strength summaries are calculated over
realized non-zero ties. observed zero non-ties and missing dyads are
excluded from strength averages, standard deviations, and medians;
negative signed ties remain part of the realized-tie distribution.

**custom statistics:**

add custom metrics using the `other_stats` parameter. each function
receives the adjacency matrix and should return a vector with one value
per actor:


    # example: maximum tie weight for each actor
    max_out <- function(mat) apply(mat, 1, max, na.rm = TRUE)
    max_in <- function(mat) apply(mat, 2, max, na.rm = TRUE)

    stats <- summary_actor(net,
      other_stats = list(max_out = max_out, max_in = max_in))

**mathematical formulations:**

for symmetric unweighted networks:

- degree: \\d_i = \sum\_{j=1}^{n} a\_{ij}\\

- proportion of ties: \\p_i = \frac{d_i}{n-1}\\

- network share: \\s_i = \frac{d_i}{\sum\_{j=1}^{n} d_j}\\

- closeness: \\c_i = \frac{1}{\sum\_{j} d(i, j)}\\

- betweenness: \\b_i = \sum\_{s \neq i \neq t}
  \frac{\sigma\_{st}(i)}{\sigma\_{st}}\\

for symmetric weighted networks, additional measures:

- strength sum: \\s_i^{sum} = \sum\_{j=1}^{n} w\_{ij}\\

- strength average: mean of the actor's realized non-zero tie weights

- strength standard deviation: standard deviation of the actor's
  realized non-zero tie weights

for asymmetric networks, statistics are calculated separately for rows
(out) and columns (in), with totals where applicable.

## Note

for longitudinal networks, statistics are calculated separately for each
time period. for multilayer networks, statistics are calculated
separately for each layer unless layers have been aggregated beforehand.

missing values (na) in the network are excluded from calculations.
isolates (actors with no connections) receive appropriate values (0 for
degree, na for some centrality measures).

the function handles both cross-sectional and longitudinal data
structures, as well as single-layer and multilayer networks. for ego
networks created with netify, the function appropriately handles the
ego-alter structure.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
# \donttest{
# load example data
data(icews)

# basic usage with directed network
net <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop"
)

# get actor statistics
actor_stats <- summary_actor(net)
head(actor_stats)
#>         actor time degree_in degree_out degree_total prop_ties_in prop_ties_out
#> 1 Afghanistan 2002        92         81          173    0.6092715     0.5364238
#> 2     Albania 2002        46         49           95    0.3046358     0.3245033
#> 3     Algeria 2002        68         65          133    0.4503311     0.4304636
#> 4      Angola 2002        64         67          131    0.4238411     0.4437086
#> 5   Argentina 2002        48         48           96    0.3178808     0.3178808
#> 6     Armenia 2002        71         66          137    0.4701987     0.4370861
#>   prop_ties_total network_share_in network_share_out network_share_total
#> 1       0.5728477      0.022594950       0.019334694         0.020964822
#> 2       0.3145695      0.002272838       0.002097149         0.002184994
#> 3       0.4403974      0.002679814       0.002497454         0.002588634
#> 4       0.4337748      0.002915549       0.002470767         0.002693158
#> 5       0.3178808      0.003173523       0.002804354         0.002988938
#> 6       0.4536424      0.005926727       0.005875577         0.005901152
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
#> 1       110.43478        107.33333          108.98266       283.37558
#> 2        22.21739         19.24490           20.68421        36.08934
#> 3        17.72059         17.27692           17.50376        25.04494
#> 4        20.48438         16.58209           18.48855        34.05737
#> 5        29.72917         26.27083           28.00000        77.10996
#> 6        37.53521         40.03030           38.73723        93.96486
#>   strength_std_out strength_std_total strength_median_in strength_median_out
#> 1        262.35853          272.95147                 13                14.0
#> 2         33.84187           34.79309                  6                 5.0
#> 3         22.46839           23.73165                  6                 7.0
#> 4         27.76970           30.94272                  6                 4.0
#> 5         67.16532           71.94823                  3                 4.5
#> 6         89.41123           91.47101                 10                13.0
#>   strength_median_total
#> 1                    14
#> 2                     6
#> 3                     6
#> 4                     4
#> 5                     4
#> 6                    11

# add custom statistics
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
#> 1 Afghanistan 2002        92         81          173    0.6092715     0.5364238
#> 2     Albania 2002        46         49           95    0.3046358     0.3245033
#> 3     Algeria 2002        68         65          133    0.4503311     0.4304636
#> 4      Angola 2002        64         67          131    0.4238411     0.4437086
#> 5   Argentina 2002        48         48           96    0.3178808     0.3178808
#> 6     Armenia 2002        71         66          137    0.4701987     0.4370861
#>   prop_ties_total network_share_in network_share_out network_share_total
#> 1       0.5728477      0.022594950       0.019334694         0.020964822
#> 2       0.3145695      0.002272838       0.002097149         0.002184994
#> 3       0.4403974      0.002679814       0.002497454         0.002588634
#> 4       0.4337748      0.002915549       0.002470767         0.002693158
#> 5       0.3178808      0.003173523       0.002804354         0.002988938
#> 6       0.4536424      0.005926727       0.005875577         0.005901152
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
#> 1       110.43478        107.33333          108.98266       283.37558
#> 2        22.21739         19.24490           20.68421        36.08934
#> 3        17.72059         17.27692           17.50376        25.04494
#> 4        20.48438         16.58209           18.48855        34.05737
#> 5        29.72917         26.27083           28.00000        77.10996
#> 6        37.53521         40.03030           38.73723        93.96486
#>   strength_std_out strength_std_total strength_median_in strength_median_out
#> 1        262.35853          272.95147                 13                14.0
#> 2         33.84187           34.79309                  6                 5.0
#> 3         22.46839           23.73165                  6                 7.0
#> 4         27.76970           30.94272                  6                 4.0
#> 5         67.16532           71.94823                  3                 4.5
#> 6         89.41123           91.47101                 10                13.0
#>   strength_median_total max_out max_in
#> 1                    14    1516   1847
#> 2                     6     160    157
#> 3                     6      91    114
#> 4                     4     147    176
#> 5                     4     424    469
#> 6                    11     547    609
# }
```
