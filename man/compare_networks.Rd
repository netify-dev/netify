% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_networks.R
\name{compare_networks}
\alias{compare_networks}
\title{Compare networks across time, layers, or attributes}
\usage{
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
}
\arguments{
\item{nets}{Either a list of netify objects to compare, or a single netify
object (for longitudinal, multilayer, or by-group comparisons).}

\item{method}{Character string specifying comparison method:
\describe{
\item{"correlation"}{Pearson correlation of edge weights (default)}
\item{"jaccard"}{Jaccard similarity for binary networks}
\item{"hamming"}{Hamming distance (proportion of differing edges)}
\item{"qap"}{Quadratic Assignment Procedure with permutation test}
\item{"spectral"}{Spectral distance based on eigenvalue spectra. Measures
global structural differences by comparing the sorted eigenvalues of network
Laplacian matrices. Useful for detecting fundamental structural changes.}
\item{"all"}{Applies all applicable methods}
}}

\item{by}{Character vector of nodal attributes. If specified, compares networks
within and between attribute groups rather than across input networks.
(Note: Currently not fully implemented)}

\item{what}{Character string specifying what to compare:
\describe{
\item{"edges"}{Edge-level comparison (default)}
\item{"structure"}{Structural properties (density, clustering, etc.)}
\item{"nodes"}{Node composition and attributes}
\item{"attributes"}{Compare networks based on node attribute distributions}
}}

\item{test}{Logical. Whether to perform significance testing. Default TRUE.}

\item{n_permutations}{Integer. Number of permutations for QAP test. Default 5000.}

\item{include_diagonal}{Logical. Whether to include diagonal in comparison. Default FALSE.}

\item{return_details}{Logical. Whether to return detailed comparison matrices. Default FALSE.}

\item{edge_threshold}{Numeric or function. For weighted networks, threshold to
determine edge presence. Default is 0 (any positive weight).}

\item{permutation_type}{Character string specifying permutation scheme:
\describe{
\item{"classic"}{Standard label permutation QAP (default)}
\item{"degree_preserving"}{Preserves degree sequence (binary networks only)}
\item{"freedman_lane"}{Freeman-Lane MRQAP for controlling autocorrelation}
\item{"dsp_mrqap"}{Double-Semi-Partialling MRQAP}
}}

\item{correlation_type}{Character string. "pearson" (default) or "spearman" for
rank-based correlation.}

\item{binary_metric}{Character string for binary network correlation:
\describe{
\item{"phi"}{Standard phi coefficient (default)}
\item{"simple_matching"}{Proportion of matching edges}
\item{"mean_centered"}{Mean-centered phi coefficient}
}}

\item{seed}{Integer. Random seed for reproducible permutations. Default NULL.}

\item{p_adjust}{Character string for multiple testing correction:
"none" (default), "holm", "BH" (Benjamini-Hochberg), or "BY".}

\item{adaptive_stop}{Logical. Whether to use adaptive stopping for permutations.
Default FALSE. (Currently not implemented)}

\item{alpha}{Numeric. Significance level for adaptive stopping. Default 0.05.}

\item{max_permutations}{Integer. Maximum permutations for adaptive stopping. Default 20000.}

\item{spectral_rank}{Integer. Number of eigenvalues to use for spectral distance.
Default 0 (use all). Set to a smaller value (e.g., 50-100) for large networks
to improve performance while maintaining accuracy.}

\item{attr_metric}{Character string for continuous attribute comparison:
\describe{
\item{"ecdf_cor"}{Correlation of empirical CDFs (default)}
\item{"wasserstein"}{Wasserstein-1 (Earth Mover's) distance}
}}

\item{other_stats}{Named list of custom functions to calculate additional
comparison statistics. Each function should accept a netify object (or matrix
for edge comparisons) and return a named vector of scalar values. The specific
input depends on the \code{what} parameter:
\describe{
\item{For \code{what = "edges"}}{Functions receive adjacency matrices}
\item{For \code{what = "structure"}}{Functions receive netify objects}
\item{For \code{what = "nodes"}}{Functions receive netify objects}
\item{For \code{what = "attributes"}}{Functions receive netify objects}
}
Example: \code{list(connectivity = function(net) \{ g <- to_igraph(net); 
  c(vertex_conn = igraph::vertex_connectivity(g)) \})}}
}
\value{
A list of class "netify_comparison" containing:
\describe{
\item{comparison_type}{Character string: "cross_network", "temporal", "multilayer", or "by_group"}
\item{method}{Comparison method(s) used}
\item{n_networks}{Number of networks compared}
\item{summary}{Data frame with comparison statistics}
\item{edge_changes}{List detailing added, removed, and maintained edges (for edge comparisons)}
\item{node_changes}{List detailing added, removed, and maintained nodes (for node comparisons)}
\item{significance_tests}{QAP test results if test = TRUE}
\item{details}{Detailed comparison matrices if return_details = TRUE}
}
}
\description{
Performs comprehensive comparison of networks to identify similarities and
differences. Supports comparison of edge patterns, structural properties,
and node compositions across multiple networks or within subgroups.
}
\details{
The function supports four types of comparisons:

\strong{Edge comparison (what = "edges"):}
Compares edge patterns between networks using correlation, Jaccard similarity,
Hamming distance, spectral distance, or QAP permutation tests. Returns detailed
edge changes showing which edges are added, removed, or maintained between networks.

\strong{Structure comparison (what = "structure"):}
Compares network-level properties like density, reciprocity, transitivity, and
mean degree. For two networks, also provides percent change calculations.

\strong{Node comparison (what = "nodes"):}
Analyzes changes in actor composition between networks, tracking which actors
enter or exit the network.

\strong{Attribute comparison (what = "attributes"):}
Compares distributions of node attributes across networks using appropriate
statistical tests (KS test for continuous, total variation distance for categorical).

\strong{Automatic handling of longitudinal data:}
When passed a single longitudinal netify object, the function automatically
extracts time periods and performs pairwise comparisons between them.

\strong{Automatic handling of multilayer networks:}
When passed a single multilayer netify object (created with \code{layer_netify()}),
the function automatically extracts layers and performs pairwise comparisons between
them. This works for cross-sectional multilayer (3D arrays), longitudinal multilayer
(4D arrays), and longitudinal list multilayer formats.

\strong{Summary statistics interpretation:}
\itemize{
\item Correlation: Ranges from -1 to 1, measuring linear relationship between edge weights
\item Jaccard: Ranges from 0 to 1, proportion of edges present in both networks
\item Hamming: Ranges from 0 to 1, proportion of differing edges
\item QAP p-value: Significance of observed correlation under random permutation
}

\strong{Permutation methods:}
When \code{permutation_type = "degree_preserving"}, the first network must be
binary (0/1). This method preserves the degree sequence during permutations,
which is important when degree heterogeneity could inflate Type I error rates.
}
\examples{
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

# Structural comparison
struct_comp <- compare_networks(
    list(net_2002, net_2003),
    what = "structure"
)

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
\dontrun{
# Create separate networks for different interaction types
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
# Will show comparison between verbal and material cooperation layers
}

# Get detailed matrices
detailed_comp <- compare_networks(
    list(net_2002, net_2003),
    return_details = TRUE
)
names(detailed_comp$details) # Shows available matrices

# Compare with custom statistics
\dontrun{
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
}

}
\author{
Cassy Dorff, Shahryar Minhas
}
