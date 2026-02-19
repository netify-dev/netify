# Mutate edge weights in a netify object

`mutate_weights` applies mathematical transformations to edge weights in
a netify object. This is useful for normalizing data, handling skewed
distributions, creating binary networks, or applying any custom
mathematical transformation to network weights.

## Usage

``` r
mutate_weights(
  netlet,
  transform_fn = NULL,
  add_constant = 0,
  new_name = NULL,
  keep_original = TRUE
)
```

## Arguments

- netlet:

  A netify object with edge weights to transform

- transform_fn:

  A function to apply to the weights. Can be any function that takes a
  matrix and returns a matrix (e.g., `log`, `sqrt`, `function(x) x^2`).
  If NULL, only the `add_constant` operation is performed.

- add_constant:

  Numeric value to add to weights before applying `transform_fn`. Useful
  for log transformations (e.g., `add_constant = 1` for `log(x + 1)`) or
  shifting distributions.

- new_name:

  Optional new name for the weight variable. If provided, updates the
  weight attribute and descriptive labels. If NULL, keeps the original
  name.

- keep_original:

  Logical. If TRUE (default), preserves the original weights as a dyadic
  variable. If FALSE, discards original weights to save memory.

## Value

A netify object with transformed weights. The original weights are
optionally preserved as a dyadic variable named "original_weight".

## Details

The function handles all netify object types:

- **Cross-sectional**: Transforms the single network matrix

- **Longitudinal arrays**: Transforms each time slice

- **Longitudinal lists**: Transforms each time period matrix

The function automatically updates network attributes:

- Updates `weight_binary` if transformation results in 0/1 values

- Updates `detail_weight` with transformation description

- Preserves all other network and nodal attributes

For longitudinal arrays, original weight preservation is not yet
implemented and will show an informational message.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)
icews_2010 <- icews[icews$year == 2010, ]

# Create a weighted network
net <- netify(
    icews_2010,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Example 1: Log transformation (common for skewed data)
net_log <- mutate_weights(
    net,
    transform_fn = log,
    add_constant = 1, # log(x + 1) to handle zeros
    new_name = "log_verbCoop"
)
print(net_log)
#> ✔ Hello, you have created network data, yay!
#> • Unipartite
#> • Asymmetric
#> • log_verbCoop (transformed)
#> • Cross-Sectional
#> • # Unique Actors: 152
#> Network Summary Statistics:
#>           dens miss  mean recip trans
#> verbCoop 0.432    0 0.975 0.966 0.639
#> • Nodal Features: None
#> • Dyad Features: original_weight

# Example 2: Square root transformation (moderate skewness)
net_sqrt <- mutate_weights(
    net,
    transform_fn = sqrt,
    new_name = "sqrt_verbCoop"
)

# Example 3: Binarization (convert to presence/absence)
net_binary <- mutate_weights(
    net,
    transform_fn = function(x) ifelse(x > 0, 1, 0),
    new_name = "verbCoop_binary"
)
#> ℹ network has been binarized through transformation

# Example 4: Standardization (z-scores)
net_std <- mutate_weights(
    net,
    transform_fn = function(x) {
        mean_x <- mean(x, na.rm = TRUE)
        sd_x <- sd(x, na.rm = TRUE)
        return((x - mean_x) / sd_x)
    },
    new_name = "verbCoop_standardized"
)

# Example 5: Rank transformation
net_rank <- mutate_weights(
    net,
    transform_fn = function(x) rank(x, na.last = "keep"),
    new_name = "verbCoop_ranked"
)

# Example 6: Power transformation
net_power <- mutate_weights(
    net,
    transform_fn = function(x) x^0.5, # Square root as power
    new_name = "verbCoop_power"
)

# Example 7: Min-max normalization (scale to 0-1)
net_norm <- mutate_weights(
    net,
    transform_fn = function(x) {
        min_x <- min(x, na.rm = TRUE)
        max_x <- max(x, na.rm = TRUE)
        return((x - min_x) / (max_x - min_x))
    },
    new_name = "verbCoop_normalized"
)

# Example 8: Winsorization (cap extreme values)
net_winsor <- mutate_weights(
    net,
    transform_fn = function(x) {
        q95 <- quantile(x, 0.95, na.rm = TRUE)
        return(pmin(x, q95)) # Cap at 95th percentile
    },
    new_name = "verbCoop_winsorized"
)

# Example 9: Only add constant (no transformation function)
net_shifted <- mutate_weights(
    net,
    add_constant = 10,
    new_name = "verbCoop_shifted"
)

# Example 10: Don't keep original weights to save memory
net_log_compact <- mutate_weights(
    net,
    transform_fn = log1p, # log(1 + x), handles zeros automatically
    new_name = "log1p_verbCoop",
    keep_original = FALSE
)

# Example 11: Longitudinal network transformation
if (FALSE) { # \dontrun{
# Create longitudinal network
net_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    actor_time_uniform = FALSE
)

# Transform across all time periods
net_longit_log <- mutate_weights(
    net_longit,
    transform_fn = log1p,
    new_name = "log_verbCoop"
)
} # }

# Example 12: Custom transformation with multiple operations
net_custom <- mutate_weights(
    net,
    transform_fn = function(x) {
        # Complex transformation: log, then standardize
        x_log <- log(x + 1)
        x_std <- (x_log - mean(x_log, na.rm = TRUE)) / sd(x_log, na.rm = TRUE)
        return(x_std)
    },
    new_name = "verbCoop_log_std"
)
```
