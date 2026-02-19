# Analyze correlations between dyadic attributes and network ties

Examines relationships between dyadic (pairwise) attributes and network
connections. Calculates correlations between dyadic variables and edge
weights/presence, with support for multiple correlation methods and
significance testing.

## Usage

``` r
dyad_correlation(
  netlet,
  dyad_vars = NULL,
  edge_vars = NULL,
  method = "pearson",
  binary_network = FALSE,
  remove_diagonal = TRUE,
  significance_test = TRUE,
  alpha = 0.05,
  partial_correlations = FALSE,
  other_stats = NULL,
  ...
)
```

## Arguments

- netlet:

  A netify object containing network data.

- dyad_vars:

  Character vector of dyadic attribute names to analyze. If NULL,
  analyzes all available dyadic variables.

- edge_vars:

  Character vector of edge variables to correlate with. If NULL, uses
  the main network matrix.

- method:

  Character string specifying correlation method:

  "pearson"

  :   Pearson product-moment correlation (default)

  "spearman"

  :   Spearman rank correlation

  "kendall"

  :   Kendall's tau correlation

- binary_network:

  Logical. Whether to convert ties to binary before correlation. Default
  FALSE.

- remove_diagonal:

  Logical. Whether to exclude diagonal elements. Default TRUE.

- significance_test:

  Logical. Whether to calculate P-values and confidence intervals.
  Default TRUE.

- alpha:

  Significance level for confidence intervals. Default 0.05.

- partial_correlations:

  Logical. Whether to calculate partial correlations controlling for
  other dyadic variables. Default FALSE.

- other_stats:

  Named list of custom functions for additional statistics.

- ...:

  Additional arguments passed to custom functions.

## Value

Data frame with one row per dyadic variable per network/time period:

- `net`:

  Network/time identifier

- `layer`:

  Layer name

- `dyad_var`:

  Name of dyadic variable

- `edge_var`:

  Name of edge variable

- `correlation`:

  Correlation coefficient

- `p_value`:

  P-value for correlation significance

- `ci_lower`, `ci_upper`:

  Confidence interval bounds

- `n_pairs`:

  Number of dyad pairs included

- `method`:

  Correlation method used

- `mean_dyad_var`:

  Mean value of dyadic variable

- `sd_dyad_var`:

  Standard deviation of dyadic variable

- `mean_edge_var`:

  Mean value of edge variable

- `sd_edge_var`:

  Standard deviation of edge variable

## Details

Extracts dyadic variables from dyad_data attribute and correlates them
with network ties. For longitudinal networks, correlations are
calculated separately for each time period. Dyadic variables should be
stored as matrices with rows and columns corresponding to network
actors. Missing values are handled using pairwise complete observations.

## Author

Cassy Dorff, Shahryar Minhas
