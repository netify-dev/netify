# Comprehensive summary of network-attribute relationships

Provides comprehensive analysis of how nodal and dyadic attributes
relate to network structure. Combines multiple analytical approaches
including homophily analysis, mixing patterns, dyadic correlations, and
network position-based attribute summaries.

## Usage

``` r
attribute_report(
  netlet,
  node_vars = NULL,
  dyad_vars = NULL,
  include_centrality = TRUE,
  include_homophily = TRUE,
  include_mixing = TRUE,
  include_dyadic_correlations = TRUE,
  centrality_measures = c("degree", "betweenness"),
  categorical_threshold = 10,
  significance_test = TRUE,
  other_stats = NULL,
  ...
)
```

## Arguments

- netlet:

  A netify object containing network data.

- node_vars:

  Character vector of nodal attributes to analyze. If NULL, analyzes all
  available nodal variables except actor and time.

- dyad_vars:

  Character vector of dyadic attributes to analyze. If NULL, analyzes
  all available dyadic variables.

- include_centrality:

  Logical. Whether to calculate attribute-centrality relationships.
  Default TRUE.

- include_homophily:

  Logical. Whether to perform homophily analysis. Default TRUE.

- include_mixing:

  Logical. Whether to create mixing matrices for categorical attributes.
  Default TRUE.

- include_dyadic_correlations:

  Logical. Whether to calculate dyadic correlations. Default TRUE.

- centrality_measures:

  Character vector of centrality measures to calculate. Options:
  "degree", "betweenness", "closeness", "eigenvector". Default
  c("degree", "betweenness").

- categorical_threshold:

  Maximum number of unique values for categorical treatment. Default 10.

- significance_test:

  Logical. Whether to perform significance tests. Default TRUE.

- other_stats:

  Named list of custom functions for additional statistics.

- ...:

  Additional arguments passed to component functions.

## Value

List containing:

- `homophily_analysis`:

  Results from homophily analysis for nodal attributes

- `mixing_analysis`:

  Results from mixing matrix analysis for categorical attributes

- `dyadic_correlations`:

  Results from dyadic correlation analysis

- `centrality_correlations`:

  Correlations between nodal attributes and centrality

- `attribute_summaries`:

  Descriptive statistics for attributes

- `overall_summary`:

  High-level summary of key findings

## Details

Serves as comprehensive wrapper around exploratory analysis functions.
Automatically determines appropriate analysis methods based on attribute
types. For large networks or many attributes, consider setting some
components to FALSE for faster computation. Centrality measures use
igraph functions.

## Author

Cassy Dorff, Shahryar Minhas
