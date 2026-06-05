# Compare networks across time, layers, or attributes

compares networks to identify similarities and differences. supports
comparison of edge patterns, structural properties, and node
compositions across multiple networks or within subgroups.

## Usage

``` r
compare_networks(
  nets,
  method = "correlation",
  by = NULL,
  what = "edges",
  test = TRUE,
  n_permutations = 5000,
  include_diagonal = FALSE,
  return_details = FALSE,
  edge_threshold = 0,
  permutation_type = c("classic", "degree_preserving"),
  correlation_type = c("pearson", "spearman"),
  binary_metric = c("phi", "simple_matching", "mean_centered"),
  seed = NULL,
  p_adjust = c("none", "holm", "BH", "BY"),
  adaptive_stop = FALSE,
  alpha = 0.05,
  max_permutations = 20000,
  spectral_rank = 0,
  attr_metric = c("ecdf_cor", "wasserstein"),
  other_stats = NULL
)
```

## Arguments

- nets:

  either a list of netify objects to compare, or a single netify object
  (for longitudinal, multilayer, or by-group comparisons).

- method:

  character string specifying comparison method:

  "correlation"

  :   pearson correlation of edge weights (default)

  "jaccard"

  :   jaccard similarity for binary networks

  "hamming"

  :   hamming distance (proportion of differing edges)

  "qap"

  :   quadratic assignment procedure with permutation test

  "spectral"

  :   spectral distance based on eigenvalue spectra. measures global
      structural differences by comparing the sorted eigenvalues of
      network laplacian matrices. useful for detecting fundamental
      structural changes.

  "all"

  :   applies all applicable methods

- by:

  character vector of nodal attributes. if specified, compares networks
  within and between attribute groups rather than across input networks.
  (note: currently not fully implemented)

- what:

  character string specifying what to compare:

  "edges"

  :   edge-level comparison (default)

  "structure"

  :   structural properties (density, clustering, etc.)

  "nodes"

  :   node composition and attributes

  "attributes"

  :   compare networks based on node attribute distributions

- test:

  logical. whether to perform qap significance testing for edge
  comparisons when `method = "qap"` or `method = "all"`. other
  comparison metrics are returned as descriptive summaries. default
  TRUE.

- n_permutations:

  integer. number of permutations for qap test. default 5000.

- include_diagonal:

  logical. whether to include diagonal in comparison. default FALSE.

- return_details:

  logical. whether to return detailed comparison matrices. default
  FALSE.

- edge_threshold:

  numeric or function. for weighted networks, threshold to determine
  edge presence in jaccard, hamming, and edge-change summaries. default
  is 0 (positive weights are treated as present ties).

- permutation_type:

  character string specifying permutation scheme:

  "classic"

  :   standard label permutation qap (default)

  "degree_preserving"

  :   preserves the first network's degree sequence for binary directed
      or symmetric networks

- correlation_type:

  character string. "pearson" (default) or "spearman" for rank-based
  edge correlation. qap significance testing currently uses pearson.

- binary_metric:

  character string for binary network correlation:

  "phi"

  :   standard phi coefficient (default)

  "simple_matching"

  :   proportion of matching edges

  "mean_centered"

  :   mean-centered phi coefficient

- seed:

  integer. random seed for reproducible permutations. default NULL.

- p_adjust:

  character string for multiple testing correction: "none" (default),
  "holm", "bh" (benjamini-hochberg), or "by".

- adaptive_stop:

  logical. whether to use adaptive stopping for permutations. default
  FALSE. (currently not implemented)

- alpha:

  numeric. significance level for adaptive stopping. default 0.05.

- max_permutations:

  integer. maximum permutations for adaptive stopping. default 20000.

- spectral_rank:

  integer. number of eigenvalues to use for spectral distance. default 0
  (use all). set to a smaller value (e.g., 50-100) for large networks to
  improve performance while maintaining accuracy.

- attr_metric:

  character string for continuous attribute comparison:

  "ecdf_cor"

  :   correlation of empirical cdfs (default)

  "wasserstein"

  :   wasserstein-1 (earth mover's) distance

- other_stats:

  named list of custom functions to calculate additional comparison
  statistics. each function should accept a netify object (or matrix for
  edge comparisons) and return a named vector of scalar values. the
  specific input depends on the `what` parameter:

  for `what = "edges"`

  :   functions receive adjacency matrices

  for `what = "structure"`

  :   functions receive netify objects

  for `what = "nodes"`

  :   functions receive netify objects

  for `what = "attributes"`

  :   functions receive netify objects

  example:
  `list(connectivity = function(net) { g <- to_igraph(net); c(vertex_conn = igraph::vertex_connectivity(g)) })`

## Value

a list of class "netify_comparison" containing:

- comparison_type:

  character string: "cross_network", "temporal", "multilayer", or
  "by_group"

- method:

  comparison method(s) used

- n_networks:

  number of networks compared

- summary:

  data frame with comparison statistics

- edge_changes:

  list detailing added, removed, and maintained edges (for edge
  comparisons)

- node_changes:

  list detailing added, removed, and maintained nodes (for node
  comparisons)

- significance_tests:

  qap test results when edge qap testing was run

- details:

  detailed comparison matrices if return_details = TRUE

- comparisons:

  long-format data frame for `what = "edges"` with one row per (network
  pair, metric) triple. columns: `net_i`, `net_j` (the two network
  names), `metric` (e.g. `"correlation"`, `"jaccard"`, `"hamming"`,
  `"qap_correlation"`, `"spectral"`, `"weight_correlation"`), `value`
  (scalar metric), `p_value` (only populated for `qap_correlation`; `na`
  otherwise). coerce to a tibble with `tibble::as_tibble(comp)` (the
  `as_tibble` s3 method returns this frame directly).

## Details

the function supports four types of comparisons:

**edge comparison (what = "edges"):** compares edge patterns between
networks using correlation, jaccard similarity, hamming distance,
spectral distance, or qap permutation tests. returns detailed edge
changes showing which edges are added, removed, or maintained between
networks.

**structure comparison (what = "structure"):** compares network-level
properties like density, reciprocity, transitivity, and mean degree. for
two networks, also provides percent change calculations.

**node comparison (what = "nodes"):** analyzes changes in actor
composition between networks, tracking which actors enter or exit the
network.

**attribute comparison (what = "attributes"):** compares distributions
of node attributes across networks using appropriate statistical tests
(ks test for continuous, total variation distance for categorical).

**automatic handling of longitudinal data:** when passed a single
longitudinal netify object, the function automatically extracts time
periods and performs pairwise comparisons between them.

**automatic handling of multilayer networks:** when passed a single
multilayer netify object (created with
[`layer_netify()`](https://netify-dev.github.io/netify/reference/layer_netify.md)),
the function automatically extracts layers and performs pairwise
comparisons between them. this works for cross-sectional multilayer (3d
arrays), longitudinal multilayer (4d arrays), and longitudinal list
multilayer formats.

**summary statistics interpretation:**

- correlation: ranges from -1 to 1, measuring linear relationship
  between edge weights

- jaccard: ranges from 0 to 1, proportion of edges present in both
  networks

- hamming: ranges from 0 to 1, proportion of differing edges

- qap p-value: observed pearson edge correlation compared to the
  selected permutation reference distribution

**permutation methods:** when `permutation_type = "degree_preserving"`,
the first network must be binary (0/1). directed inputs use directed
edge swaps that preserve in- and out-degree. symmetric inputs use
undirected edge swaps that preserve the undirected degree sequence.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# create networks for different years
net_2002 <- netify(icews[icews$year == 2002, ],
    actor1 = "i", actor2 = "j",
    weight = "matlConf"
)
net_2003 <- netify(icews[icews$year == 2003, ],
    actor1 = "i", actor2 = "j",
    weight = "matlConf"
)

# basic edge comparison
comp <- compare_networks(list("2002" = net_2002, "2003" = net_2003))
print(comp)
#> 
#> ── Network Comparison Results ──────────────────────────────────────────────────
#> Comparison type: cross_network
#> Number of networks: 2
#> Comparison focus: edges
#> Method: correlation
#> Algorithm: correlation
#> Permutation type: classic
#> Correlation type: pearson
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Edge Comparison Summary ──
#> 
#>     comparison correlation
#> 1 2002 vs 2003       0.943
#> ── Edge Changes 
#> 2002_vs_2003:
#> Added: 1366 | Removed: 1198 | Maintained: 1866

# structural comparison
struct_comp <- compare_networks(
    list(net_2002, net_2003),
    what = "structure"
)

# \donttest{
# create longitudinal network for automatic temporal comparison
longit_net <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    weight = "verbCoop",
    output_format = "longit_list"
)

# automatic temporal comparison
temporal_comp <- compare_networks(longit_net, method = "all")

# create multilayer network example
verbal_coop <- netify(
    icews[icews$year == 2010, ],
    actor1 = "i", actor2 = "j",
    weight = "verbCoop"
)

material_coop <- netify(
    icews[icews$year == 2010, ],
    actor1 = "i", actor2 = "j",
    weight = "matlCoop"
)

# combine into multilayer network
multilayer <- layer_netify(
    list(verbal = verbal_coop, material = material_coop)
)

# automatic multilayer comparison
layer_comp <- compare_networks(multilayer, method = "all")
print(layer_comp)
#> 
#> ── Network Comparison Results ──────────────────────────────────────────────────
#> Comparison type: multilayer
#> Number of networks: 2
#> Comparison focus: edges
#> Method: all
#> Algorithm: all
#> Permutation type: classic
#> Correlation type: pearson
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Edge Comparison Summary ──
#> 
#>           comparison correlation jaccard hamming qap_correlation qap_pvalue
#> 1 verbal vs material       0.609   0.264   0.354           0.609      2e-04
#>   spectral
#> 1  8.4e+04
#> ── Edge Changes 
#> verbal_vs_material:
#> Added: 188 | Removed: 7930 | Maintained: 2912
#> 
#> ── Statistical Tests ──
#> 
#> Permutations: 5000
#> 
#> ── qap_correlations 
#>             verbal  material
#> verbal   1.0000000 0.6090188
#> material 0.6090188 1.0000000
#> 
#> ── qap_pvalues 
#>              verbal   material
#> verbal           NA 0.00019996
#> material 0.00019996         NA
#> attr(,"n_perm")
#> [1] 5000
#> 
#> ── qap_valid_permutations 
#>          verbal material
#> verbal       NA     5000
#> material   5000       NA
#> attr(,"n_perm")
#> [1] 5000

# get detailed matrices
detailed_comp <- compare_networks(
    list(net_2002, net_2003),
    return_details = TRUE
)
names(detailed_comp$details) # shows available matrices
#> [1] "correlation_matrix" "jaccard_matrix"     "hamming_matrix"    
#> [4] "spectral_matrix"   
# }

# compare with custom statistics
if (FALSE) { # \dontrun{
library(igraph)

# define custom connectivity function
connectivity_stats <- function(net) {
    g <- to_igraph(net)
    c(vertex_connectivity = vertex_connectivity(g),
      edge_connectivity = edge_connectivity(g),
      diameter = diameter(g, directed = FALSE))
}

# apply to structural comparison
struct_comp_custom <- compare_networks(
    list("2002" = net_2002, "2003" = net_2003),
    what = "structure",
    other_stats = list(connectivity = connectivity_stats)
)

# custom stats will appear in the summary
print(struct_comp_custom$summary)
} # }
```
