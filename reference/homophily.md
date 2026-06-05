# Analyze homophily in network data

tests whether connected actors have similar attributes (homophily).
calculates the correlation between attribute similarity and tie
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

  a netify object containing network data.

- attribute:

  character string specifying the nodal attribute to analyze.

- method:

  character string specifying the similarity metric:

  "correlation"

  :   negative absolute difference for continuous data (default)

  "euclidean"

  :   negative euclidean distance for continuous data

  "manhattan"

  :   negative manhattan/city-block distance for continuous data

  "cosine"

  :   cosine similarity for continuous data

  "categorical"

  :   binary similarity (0/1) for categorical data

  "jaccard"

  :   jaccard similarity for binary/presence-absence data

  "hamming"

  :   negative hamming distance for categorical data

- threshold:

  numeric value or function to determine tie presence in weighted
  networks. if numeric, edges with weights \> threshold are considered
  ties. if a function, it should take the network matrix and return a
  logical matrix. default is 0 (any positive weight is a tie). common
  values: 0 (default), mean(weights), median(weights), or quantile-based
  thresholds. for pre-binarized networks, consider using
  [`mutate_weights()`](https://netify-dev.github.io/netify/reference/mutate_weights.md)
  first.

- signed_handling:

  character. strategy for signed (negative-weight) edges:

  `"abs"`

  :   (default) take absolute values before thresholding so any tie
      magnitude – positive or negative – can become a connection.

  `"drop_negative"`

  :   set negative weights to zero before thresholding (positive ties
      only).

  `"preserve_sign"`

  :   treat any non-zero entry (positive or negative) as a connection;
      `threshold` is ignored for sign decisions.

  ignored when the network has no negative weights.

- significance_test:

  logical. whether to perform a dyad-level permutation test. default
  TRUE.

- n_permutations:

  number of permutations for significance testing. default 1000.

- alpha:

  significance level for confidence intervals. default 0.05.

- other_stats:

  named list of custom functions for additional statistics.

- ...:

  additional arguments passed to custom functions.

## Value

data frame with homophily statistics per network/time period:

- `net`:

  network/time identifier

- `layer`:

  layer name

- `attribute`:

  analyzed attribute name

- `method`:

  similarity method used

- `threshold_value`:

  threshold used for determining ties (na for binary networks)

- `homophily_correlation`:

  correlation between similarity and tie presence (binary: tie/no tie)

- `mean_similarity_connected`:

  mean similarity among connected pairs (weight \> threshold)

- `mean_similarity_unconnected`:

  mean similarity among unconnected pairs (weight \<= threshold or
  missing)

- `similarity_difference`:

  difference between connected and unconnected mean similarities

- `p_value`:

  permutation test p-value

- `ci_lower`, `ci_upper`:

  confidence interval bounds

- `n_connected_pairs`:

  number of connected pairs

- `n_unconnected_pairs`:

  number of unconnected pairs

## Details

**auto-promotion to `categorical`:**

if you leave `method` at its default and pass a `character`, `factor`,
or `logical` attribute, `homophily()` will switch to
`method = "categorical"` automatically and inform you once per
attribute. this avoids the c++-level error that would otherwise come
from feeding non-numeric data to the correlation-based similarity
routine. pass `method = "categorical"` (or any other explicit choice) to
silence the message.

**similarity metrics:**

for continuous attributes:

- `correlation`: based on absolute difference, good general purpose
  metric

- `euclidean`: similar to correlation for single attributes

- `manhattan`: less sensitive to outliers than euclidean

- `cosine`: useful for normalized data or when sign matters

for categorical/binary attributes:

- `categorical`: simple matching (1 if same, 0 if different)

- `jaccard`: for binary data, emphasizes shared presence over shared
  absence

- `hamming`: counts positions where values differ (negated for
  similarity)

**threshold parameter:**

for weighted networks, the `threshold` parameter determines what edge
weights constitute a "connection". you can specify:

- a numeric value: edges with weight \> threshold are ties

- a function: should take a matrix and return a single numeric threshold

- common threshold functions:

  - `function(x) mean(x, na.rm = TRUE)` - mean weight

  - `function(x) median(x, na.rm = TRUE)` - median weight

  - `function(x) quantile(x, 0.75, na.rm = TRUE)` - 75th percentile

for more complex binarization needs (e.g., different thresholds by time
period), consider using
[`mutate_weights()`](https://netify-dev.github.io/netify/reference/mutate_weights.md)
to pre-process your network.

**permutation test:**

when `significance_test = TRUE`, `homophily()` holds the tie indicators
fixed and permutes the dyad-level similarity values to form an
exploratory reference distribution. the resulting p-value and confidence
interval summarize how unusual the observed dyad-level association is
under that exchangeability assumption. they are not a node-label
permutation test and should not be read as a causal estimate of tie
formation.

## Author

cassy dorff, shahryar minhas

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
#>     p_value    ci_lower  ci_upper n_connected_pairs n_unconnected_pairs
#> 1 0.1278721 -0.02260239 0.1701738                51                 384
#>   n_missing n_pairs
#> 1         0     435

if (FALSE) { # \dontrun{
# basic homophily analysis with default threshold (> 0)
homophily_default <- homophily(net, attribute = "group")

# using different similarity metrics for continuous data
homophily_manhattan <- homophily(
    net,
    attribute = "age",
    method = "manhattan" # less sensitive to outliers
)

# for binary attributes (e.g., gender, membership)
homophily_jaccard <- homophily(
    net,
    attribute = "member",
    method = "jaccard" # better for binary data than correlation
)

# for categorical attributes
homophily_categorical <- homophily(
    net,
    attribute = "department",
    method = "categorical"
)

# combining method and threshold
homophily_combined <- homophily(
    net,
    attribute = "score",
    method = "manhattan",
    threshold = function(x) quantile(x, 0.75, na.rm = TRUE)
)
} # }
```
