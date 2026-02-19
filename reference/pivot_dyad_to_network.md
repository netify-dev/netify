# Pivot a dyadic variable to become the network

`pivot_dyad_to_network` swaps a dyadic attribute with the main network
in a netify object. This is useful when you want to analyze a different
relationship type that was stored as a dyadic attribute. For example, if
your network represents trade relationships but you have FDI stored as a
dyadic attribute, you can pivot to make FDI the main network.

## Usage

``` r
pivot_dyad_to_network(
  netlet,
  dyad_var,
  make_network_dyad_var = TRUE,
  network_var_name = NULL,
  symmetric = NULL,
  weight_type = NULL,
  diag_to_NA = NULL,
  missing_to_zero = NULL
)
```

## Arguments

- netlet:

  A netify object containing the network and dyadic attributes

- dyad_var:

  Character string naming the dyadic variable to become the new network.
  Must exist in the netlet's dyad_data attribute.

- make_network_dyad_var:

  Logical. If TRUE (default), the current network will be preserved as a
  dyadic attribute.

- network_var_name:

  Character string specifying the name for the old network when
  converted to a dyadic attribute. If NULL (default), uses the current
  weight attribute name if available, otherwise "old_network".

- symmetric:

  Logical or NULL. Specifies whether the new network should be treated
  as symmetric. If NULL (default), attempts to detect from the dyadic
  variable's symmetry setting or data structure.

- weight_type:

  Character string describing the type of weight for the new network
  (e.g., "trade_volume", "fdi_amount"). If NULL (default), uses the
  dyad_var name.

- diag_to_NA:

  Logical. Whether to set diagonal values to NA in the new network. If
  NULL (default), inherits from the original netlet.

- missing_to_zero:

  Logical. Whether to treat missing values as zeros in the new network.
  If NULL (default), inherits from the original netlet.

## Value

A netify object with the dyadic variable as the main network and
(optionally) the old network preserved as a dyadic attribute. All other
attributes and dyadic variables are preserved.

## Details

The function handles different netify types appropriately:

- For cross-sectional networks: performs a simple matrix swap

- For longitudinal arrays: swaps matrices across all time periods

- For longitudinal lists: swaps matrices for each time period

- For multilayer networks: swaps within each layer

When the new network has different properties than the original (e.g.,
different symmetry or weight type), the function updates the netify
attributes accordingly. For bipartite networks, the new network is
always treated as asymmetric.

If the dyadic variable was originally specified with symmetry
information via
[`add_dyad_vars()`](https://netify-dev.github.io/netify/reference/add_dyad_vars.md),
that information is used unless overridden by the symmetric parameter.

## Author

Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Create a netify object with verbal cooperation as the main network
# and material cooperation as a dyadic attribute
icews_10 <- icews[icews$year == 2010, ]

net <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

net <- add_dyad_vars(
    net,
    icews_10,
    actor1 = "i", actor2 = "j",
    dyad_vars = "matlCoop",
    dyad_vars_symmetric = FALSE
)

# Check the current network
print(net)
#> ✔ Hello, you have created network data, yay!
#> • Unipartite
#> • Asymmetric
#> • Weights from `verbCoop`
#> • Cross-Sectional
#> • # Unique Actors: 152
#> Network Summary Statistics:
#>           dens miss   mean recip trans
#> verbCoop 0.432    0 18.134 0.982 0.639
#> • Nodal Features: None
#> • Dyad Features: matlCoop

# Pivot to make material cooperation the main network
net_pivoted <- pivot_dyad_to_network(
    net,
    dyad_var = "matlCoop",
    network_var_name = "verbCoop"
)
#> ℹ Auto-detected symmetry for 'matlCoop': asymmetric
#> ✔ Successfully pivoted 'matlCoop' to be the main network
#> ℹ Previous network preserved as dyadic variable 'verbCoop'

# The main network is now material cooperation
print(net_pivoted)
#> ✔ Hello, you have created network data, yay!
#> • Unipartite
#> • Asymmetric
#> • Weighted: matlCoop
#> • Cross-Sectional
#> • # Unique Actors: 152
#> Network Summary Statistics:
#>           dens   miss  mean recip trans
#> verbCoop 0.092 -0.007 0.467 0.442 0.333
#> • Nodal Features: None
#> • Dyad Features: verbCoop

# The old network (verbal cooperation) is preserved as a dyadic attribute
attr(net_pivoted, "dyad_data")[["1"]][["verbCoop"]][1:5, 1:5]
#>             Afghanistan Albania Algeria Angola Argentina
#> Afghanistan          NA       0       0      0         1
#> Albania               5      NA       0      0         0
#> Algeria               5       1      NA      8         0
#> Angola                0       0       6     NA         7
#> Argentina             2       0       0      8        NA
```
