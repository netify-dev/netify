# Summary of network-attribute relationships

summarizes how nodal and dyadic attributes relate to network structure.
combines homophily analysis, mixing patterns, dyadic correlations, and
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

  a netify object containing network data.

- node_vars:

  character vector of nodal attributes to analyze. if NULL, analyzes all
  available nodal variables except actor and time.

- dyad_vars:

  character vector of dyadic attributes to analyze. if NULL, analyzes
  all available dyadic variables.

- include_centrality:

  logical. whether to calculate attribute-centrality relationships.
  default TRUE.

- include_homophily:

  logical. whether to perform homophily analysis. default TRUE.

- include_mixing:

  logical. whether to create mixing matrices for categorical attributes.
  default TRUE.

- include_dyadic_correlations:

  logical. whether to calculate dyadic correlations. default TRUE.

- centrality_measures:

  character vector of centrality measures to calculate. options:
  "degree", "betweenness", "closeness", "eigenvector". default
  c("degree", "betweenness").

- categorical_threshold:

  maximum number of unique values for categorical treatment. default 10.

- significance_test:

  logical. whether to perform significance tests. default TRUE.

- other_stats:

  named list of custom functions for additional statistics.

- ...:

  additional arguments passed to component functions.

## Value

list containing:

- `homophily_analysis`:

  results from homophily analysis for nodal attributes

- `mixing_analysis`:

  results from mixing matrix analysis for categorical attributes

- `dyadic_correlations`:

  results from dyadic correlation analysis

- `centrality_correlations`:

  correlations between nodal attributes and centrality

- `attribute_summaries`:

  descriptive statistics for attributes

- `overall_summary`:

  brief summary of key findings

## Details

wraps the exploratory analysis functions and chooses methods based on
attribute types. for large networks or many attributes, consider setting
some components to FALSE for faster computation. centrality measures use
igraph functions.

## Author

cassy dorff, shahryar minhas
