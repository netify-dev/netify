# Analyze homophily in network data

Tests whether connected actors have similar attributes (homophily).
Calculates the correlation between attribute similarity and tie
presence, with support for multiple similarity metrics and significance
testing.

## Usage

``` r
homophily(
  netlet,
  attribute,
  method = "correlation",
  threshold = 0,
  signed_handling = c("abs", "drop_negative", "preserve_sign"),
  significance_test = TRUE,
  n_permutations = 1000,
  alpha = 0.05,
  other_stats = NULL,
  ...
)
```

## Arguments

- netlet:

  A netify object containing network data.

- attribute:

  Character string specifying the nodal attribute to analyze.

- method:

  Character string specifying the similarity metric:

  "correlation"

  :   Negative absolute difference for continuous data (default)

  "euclidean"

  :   Negative euclidean distance for continuous data

  "manhattan"

  :   Negative Manhattan/city-block distance for continuous data

  "cosine"

  :   Cosine similarity for continuous data

  "categorical"

  :   Binary similarity (0/1) for categorical data

  "jaccard"

  :   Jaccard similarity for binary/presence-absence data

  "hamming"

  :   Negative Hamming distance for categorical data

- threshold:

  Numeric value or function to determine tie presence in weighted
  networks. If numeric, edges with weights \> threshold are considered
  ties. If a function, it should take the network matrix and return a
  logical matrix. Default is 0 (any positive weight is a tie). Common
  values: 0 (default), mean(weights), median(weights), or quantile-based
  thresholds. For pre-binarized networks, consider using
  [`mutate_weights()`](https://netify-dev.github.io/netify/reference/mutate_weights.md)
  first.

- signed_handling:

  Character. Strategy for signed (negative-weight) edges:

  `"abs"`

  :   (default) Take absolute values before thresholding so any tie
      magnitude — positive or negative — can become a connection.

  `"drop_negative"`

  :   Set negative weights to zero before thresholding (positive ties
      only).

  `"preserve_sign"`

  :   Treat any non-zero entry (positive or negative) as a connection;
      `threshold` is ignored for sign decisions.

  Ignored when the network has no negative weights.

- significance_test:

  Logical. Whether to perform permutation test. Default TRUE.

- n_permutations:

  Number of permutations for significance testing. Default 1000.

- alpha:

  Significance level for confidence intervals. Default 0.05.

- other_stats:

  Named list of custom functions for additional statistics.

- ...:

  Additional arguments passed to custom functions.

## Value

Data frame with homophily statistics per network/time period:

- `net`:

  Network/time identifier

- `layer`:

  Layer name

- `attribute`:

  Analyzed attribute name

- `method`:

  Similarity method used

- `threshold_value`:

  Threshold used for determining ties (NA for binary networks)

- `homophily_correlation`:

  Correlation between similarity and tie presence (binary: tie/no tie)

- `mean_similarity_connected`:

  Mean similarity among connected pairs (weight \> threshold)

- `mean_similarity_unconnected`:

  Mean similarity among unconnected pairs (weight \<= threshold or
  missing)

- `similarity_difference`:

  Difference between connected and unconnected mean similarities

- `p_value`:

  Permutation test p-value

- `ci_lower`, `ci_upper`:

  Confidence interval bounds

- `n_connected_pairs`:

  Number of connected pairs

- `n_unconnected_pairs`:

  Number of unconnected pairs

## Details

**Auto-promotion to `categorical`:**

If you leave `method` at its default and pass a `character`, `factor`,
or `logical` attribute, `homophily()` will switch to
`method = "categorical"` automatically and inform you once per
attribute. This avoids the C++-level error that would otherwise come
from feeding non-numeric data to the correlation-based similarity
routine. Pass `method = "categorical"` (or any other explicit choice) to
silence the message.

**Similarity Metrics:**

For continuous attributes:

- `correlation`: Based on absolute difference, good general purpose
  metric

- `euclidean`: Similar to correlation for single attributes

- `manhattan`: Less sensitive to outliers than euclidean

- `cosine`: Useful for normalized data or when sign matters

For categorical/binary attributes:

- `categorical`: Simple matching (1 if same, 0 if different)

- `jaccard`: For binary data, emphasizes shared presence over shared
  absence

- `hamming`: Counts positions where values differ (negated for
  similarity)

**Threshold Parameter:**

For weighted networks, the `threshold` parameter determines what edge
weights constitute a "connection". You can specify:

- A numeric value: edges with weight \> threshold are ties

- A function: should take a matrix and return a single numeric threshold

- Common threshold functions:

  - `function(x) mean(x, na.rm = TRUE)` - mean weight

  - `function(x) median(x, na.rm = TRUE)` - median weight

  - `function(x) quantile(x, 0.75, na.rm = TRUE)` - 75th percentile

For more complex binarization needs (e.g., different thresholds by time
period), consider using
[`mutate_weights()`](https://netify-dev.github.io/netify/reference/mutate_weights.md)
to pre-process your network.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
# quick homophily check on the bundled classroom friendship data
data(classroom_edges)
data(classroom_nodes)
net <- netify(
    classroom_edges,
    actor1 = "from", actor2 = "to",
    symmetric = TRUE,
    nodal_data = classroom_nodes
)
# do students cluster by gender?
homophily(net, attribute = "gender", method = "categorical")
#>   net   layer attribute      method threshold_value homophily_correlation
#> 1   1 weight1    gender categorical              NA            0.07436588
#>   mean_similarity_connected mean_similarity_unconnected similarity_difference
#> 1                 0.6666667                   0.5520833             0.1145833
#>   p_value    ci_lower  ci_upper n_connected_pairs n_unconnected_pairs n_missing
#> 1   0.127 -0.02056023 0.1595074                51                 384         0
#>   n_pairs
#> 1     435

if (FALSE) { # \dontrun{
# Basic homophily analysis with default threshold (> 0)
homophily_default <- homophily(net, attribute = "group")

# Using different similarity metrics for continuous data
homophily_manhattan <- homophily(
    net,
    attribute = "age",
    method = "manhattan" # Less sensitive to outliers
)

# For binary attributes (e.g., gender, membership)
homophily_jaccard <- homophily(
    net,
    attribute = "member",
    method = "jaccard" # Better for binary data than correlation
)

# For categorical attributes
homophily_categorical <- homophily(
    net,
    attribute = "department",
    method = "categorical"
)

# Combining method and threshold
homophily_combined <- homophily(
    net,
    attribute = "score",
    method = "manhattan",
    threshold = function(x) quantile(x, 0.75, na.rm = TRUE)
)
} # }
```
