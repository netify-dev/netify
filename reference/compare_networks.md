# Compare networks across time, layers, or attributes

Performs comprehensive comparison of networks to identify similarities
and differences. Supports comparison of edge patterns, structural
properties, and node compositions across multiple networks or within
subgroups.

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
  permutation_type = c("classic", "degree_preserving", "freedman_lane", "dsp_mrqap"),
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

  Either a list of netify objects to compare, or a single netify object
  (for longitudinal, multilayer, or by-group comparisons).

- method:

  Character string specifying comparison method:

  "correlation"

  :   Pearson correlation of edge weights (default)

  "jaccard"

  :   Jaccard similarity for binary networks

  "hamming"

  :   Hamming distance (proportion of differing edges)

  "qap"

  :   Quadratic Assignment Procedure with permutation test

  "spectral"

  :   Spectral distance based on eigenvalue spectra. Measures global
      structural differences by comparing the sorted eigenvalues of
      network Laplacian matrices. Useful for detecting fundamental
      structural changes.

  "all"

  :   Applies all applicable methods

- by:

  Character vector of nodal attributes. If specified, compares networks
  within and between attribute groups rather than across input networks.
  (Note: Currently not fully implemented)

- what:

  Character string specifying what to compare:

  "edges"

  :   Edge-level comparison (default)

  "structure"

  :   Structural properties (density, clustering, etc.)

  "nodes"

  :   Node composition and attributes

  "attributes"

  :   Compare networks based on node attribute distributions

- test:

  Logical. Whether to perform significance testing. Default TRUE.

- n_permutations:

  Integer. Number of permutations for QAP test. Default 5000.

- include_diagonal:

  Logical. Whether to include diagonal in comparison. Default FALSE.

- return_details:

  Logical. Whether to return detailed comparison matrices. Default
  FALSE.

- edge_threshold:

  Numeric or function. For weighted networks, threshold to determine
  edge presence. Default is 0 (any positive weight).

- permutation_type:

  Character string specifying permutation scheme:

  "classic"

  :   Standard label permutation QAP (default)

  "degree_preserving"

  :   Preserves degree sequence (binary networks only)

  "freedman_lane"

  :   Freeman-Lane MRQAP for controlling autocorrelation

  "dsp_mrqap"

  :   Double-Semi-Partialling MRQAP

- correlation_type:

  Character string. "pearson" (default) or "spearman" for rank-based
  correlation.

- binary_metric:

  Character string for binary network correlation:

  "phi"

  :   Standard phi coefficient (default)

  "simple_matching"

  :   Proportion of matching edges

  "mean_centered"

  :   Mean-centered phi coefficient

- seed:

  Integer. Random seed for reproducible permutations. Default NULL.

- p_adjust:

  Character string for multiple testing correction: "none" (default),
  "holm", "BH" (Benjamini-Hochberg), or "BY".

- adaptive_stop:

  Logical. Whether to use adaptive stopping for permutations. Default
  FALSE. (Currently not implemented)

- alpha:

  Numeric. Significance level for adaptive stopping. Default 0.05.

- max_permutations:

  Integer. Maximum permutations for adaptive stopping. Default 20000.

- spectral_rank:

  Integer. Number of eigenvalues to use for spectral distance. Default 0
  (use all). Set to a smaller value (e.g., 50-100) for large networks to
  improve performance while maintaining accuracy.

- attr_metric:

  Character string for continuous attribute comparison:

  "ecdf_cor"

  :   Correlation of empirical CDFs (default)

  "wasserstein"

  :   Wasserstein-1 (Earth Mover's) distance

- other_stats:

  Named list of custom functions to calculate additional comparison
  statistics. Each function should accept a netify object (or matrix for
  edge comparisons) and return a named vector of scalar values. The
  specific input depends on the `what` parameter:

  For `what = "edges"`

  :   Functions receive adjacency matrices

  For `what = "structure"`

  :   Functions receive netify objects

  For `what = "nodes"`

  :   Functions receive netify objects

  For `what = "attributes"`

  :   Functions receive netify objects

  Example:
  `list(connectivity = function(net) { g <- to_igraph(net); c(vertex_conn = igraph::vertex_connectivity(g)) })`

## Value

A list of class "netify_comparison" containing:

- comparison_type:

  Character string: "cross_network", "temporal", "multilayer", or
  "by_group"

- method:

  Comparison method(s) used

- n_networks:

  Number of networks compared

- summary:

  Data frame with comparison statistics

- edge_changes:

  List detailing added, removed, and maintained edges (for edge
  comparisons)

- node_changes:

  List detailing added, removed, and maintained nodes (for node
  comparisons)

- significance_tests:

  QAP test results if test = TRUE

- details:

  Detailed comparison matrices if return_details = TRUE

## Details

The function supports four types of comparisons:

**Edge comparison (what = "edges"):** Compares edge patterns between
networks using correlation, Jaccard similarity, Hamming distance,
spectral distance, or QAP permutation tests. Returns detailed edge
changes showing which edges are added, removed, or maintained between
networks.

**Structure comparison (what = "structure"):** Compares network-level
properties like density, reciprocity, transitivity, and mean degree. For
two networks, also provides percent change calculations.

**Node comparison (what = "nodes"):** Analyzes changes in actor
composition between networks, tracking which actors enter or exit the
network.

**Attribute comparison (what = "attributes"):** Compares distributions
of node attributes across networks using appropriate statistical tests
(KS test for continuous, total variation distance for categorical).

**Automatic handling of longitudinal data:** When passed a single
longitudinal netify object, the function automatically extracts time
periods and performs pairwise comparisons between them.

**Automatic handling of multilayer networks:** When passed a single
multilayer netify object (created with
[`layer_netify()`](https://netify-dev.github.io/netify/reference/layer_netify.md)),
the function automatically extracts layers and performs pairwise
comparisons between them. This works for cross-sectional multilayer (3D
arrays), longitudinal multilayer (4D arrays), and longitudinal list
multilayer formats.

**Summary statistics interpretation:**

- Correlation: Ranges from -1 to 1, measuring linear relationship
  between edge weights

- Jaccard: Ranges from 0 to 1, proportion of edges present in both
  networks

- Hamming: Ranges from 0 to 1, proportion of differing edges

- QAP p-value: Significance of observed correlation under random
  permutation

**Permutation methods:** When `permutation_type = "degree_preserving"`,
the first network must be binary (0/1). This method preserves the degree
sequence during permutations, which is important when degree
heterogeneity could inflate Type I error rates.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Create networks for different years
net_2002 <- netify(icews[icews$year == 2002, ],
    actor1 = "i", actor2 = "j",
    weight = "matlConf"
)
net_2003 <- netify(icews[icews$year == 2003, ],
    actor1 = "i", actor2 = "j",
    weight = "matlConf"
)

# Basic edge comparison
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
#> Random seed: 1608854729
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Edge Comparison Summary ──
#> 
#>     comparison correlation
#> 1 2002 vs 2003       0.943
#> ── Edge Changes 
#> 2002_vs_2003:
#> Added: 1366 | Removed: 1198 | Maintained: 1866

# Structural comparison
struct_comp <- compare_networks(
    list(net_2002, net_2003),
    what = "structure"
)

# \donttest{
# Create longitudinal network for automatic temporal comparison
longit_net <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    weight = "verbCoop",
    output_format = "longit_list"
)

# Automatic temporal comparison
temporal_comp <- compare_networks(longit_net, method = "all")

# Create multilayer network example
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

# Combine into multilayer network
multilayer <- layer_netify(
    list(verbal = verbal_coop, material = material_coop)
)

# Automatic multilayer comparison
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
#> Random seed: 1733169229
#> ────────────────────────────────────────────────────────────────────────────────
#> 
#> ── Edge Comparison Summary ──
#> 
#>           comparison correlation jaccard hamming qap_correlation qap_pvalue
#> 1 verbal vs material       0.609   0.264   0.351           0.609          0
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
#> verbal   1.0000000 0.6090541
#> material 0.6090541 1.0000000
#> 
#> ── qap_pvalues 
#>          verbal material
#> verbal        0        0
#> material      0        0
#> attr(,"n_perm")
#> [1] 5000

# Get detailed matrices
detailed_comp <- compare_networks(
    list(net_2002, net_2003),
    return_details = TRUE
)
names(detailed_comp$details) # Shows available matrices
#> [1] "correlation_matrix" "jaccard_matrix"     "hamming_matrix"    
#> [4] "spectral_matrix"   
# }

# Compare with custom statistics
if (FALSE) { # \dontrun{
library(igraph)

# Define custom connectivity function
connectivity_stats <- function(net) {
    g <- to_igraph(net)
    c(vertex_connectivity = vertex_connectivity(g),
      edge_connectivity = edge_connectivity(g),
      diameter = diameter(g, directed = FALSE))
}

# Apply to structural comparison
struct_comp_custom <- compare_networks(
    list("2002" = net_2002, "2003" = net_2003),
    what = "structure",
    other_stats = list(connectivity = connectivity_stats)
)

# Custom stats will appear in the summary
print(struct_comp_custom$summary)
} # }
```
