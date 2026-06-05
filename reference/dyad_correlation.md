# Analyze correlations between dyadic attributes and network ties

examines relationships between dyadic (pairwise) attributes and network
connections. calculates correlations between dyadic variables and edge
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

  a netify object containing network data.

- dyad_vars:

  character vector of dyadic attribute names to analyze. if NULL,
  analyzes all available dyadic variables.

- edge_vars:

  character vector of edge variables to correlate with. if NULL, uses
  the main network matrix.

- method:

  character string specifying correlation method:

  "pearson"

  :   pearson product-moment correlation (default)

  "spearman"

  :   spearman rank correlation

  "kendall"

  :   kendall's tau correlation

- binary_network:

  logical. whether to convert ties to binary before correlation. default
  FALSE.

- remove_diagonal:

  logical. whether to exclude diagonal elements. default TRUE.

- significance_test:

  logical. whether to calculate ordinary correlation p-values and
  confidence intervals on the dyad vectors. default TRUE.

- alpha:

  significance level for confidence intervals. default 0.05.

- partial_correlations:

  logical. whether to calculate partial correlations controlling for
  other dyadic variables. default FALSE.

- other_stats:

  named list of custom functions for additional statistics.

- ...:

  additional arguments passed to custom functions.

## Value

data frame with one row per dyadic variable per network/time period:

- `net`:

  network/time identifier

- `layer`:

  layer name

- `dyad_var`:

  name of dyadic variable

- `edge_var`:

  name of edge variable

- `correlation`:

  correlation coefficient

- `p_value`:

  p-value for correlation significance

- `ci_lower`, `ci_upper`:

  confidence interval bounds

- `n_pairs`:

  number of dyad pairs included

- `method`:

  correlation method used

- `mean_dyad_var`:

  mean value of dyadic variable

- `sd_dyad_var`:

  standard deviation of dyadic variable

- `mean_edge_var`:

  mean value of edge variable

- `sd_edge_var`:

  standard deviation of edge variable

## Details

extracts dyadic variables from dyad_data attribute and correlates them
with network ties. for longitudinal networks, correlations are
calculated separately for each time period. dyadic variables should be
stored as matrices with rows and columns corresponding to network
actors. missing values are handled using pairwise complete observations.

the reported p-values and confidence intervals are the standard tests
from [`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html)
applied to the dyad vectors. they are useful as descriptive screens, but
they do not model network dependence among dyads.

## Author

cassy dorff, shahryar minhas
