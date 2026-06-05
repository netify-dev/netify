# Pivot a dyadic variable to become the network

`pivot_dyad_to_network` swaps a dyadic attribute with the main network
in a netify object. this is useful when you want to analyze a different
relationship type that was stored as a dyadic attribute. for example, if
your network represents trade relationships but you have fdi stored as a
dyadic attribute, you can pivot to make fdi the main network.

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

  a netify object containing the network and dyadic attributes

- dyad_var:

  character string naming the dyadic variable to become the new network.
  must exist in the netlet's dyad_data attribute.

- make_network_dyad_var:

  logical. if TRUE (default), the current network will be preserved as a
  dyadic attribute.

- network_var_name:

  character string specifying the name for the old network when
  converted to a dyadic attribute. if NULL (default), uses the current
  weight attribute name if available, otherwise "old_network".

- symmetric:

  logical or NULL. specifies whether the new network should be treated
  as symmetric. if NULL (default), attempts to detect from the dyadic
  variable's symmetry setting or data structure.

- weight_type:

  character string describing the type of weight for the new network
  (e.g., "trade_volume", "fdi_amount"). if NULL (default), uses the
  dyad_var name.

- diag_to_NA:

  logical. whether to set diagonal values to na in the new network. if
  NULL (default), inherits from the original netlet.

- missing_to_zero:

  logical. whether to treat missing values as zeros in the new network.
  if NULL (default), inherits from the original netlet.

## Value

a netify object with the dyadic variable as the main network and
(optionally) the old network preserved as a dyadic attribute. all other
attributes and dyadic variables are preserved.

## Details

the function handles different netify types appropriately:

- for cross-sectional networks: performs a simple matrix swap

- for longitudinal arrays: swaps matrices across all time periods

- for longitudinal lists: swaps matrices for each time period

- for multilayer networks: swaps within each layer

when the new network has different properties than the original (e.g.,
different symmetry or weight type), the function updates the netify
attributes accordingly. for bipartite networks, the new network is
always treated as asymmetric.

if the dyadic variable was originally specified with symmetry
information via
[`add_dyad_vars()`](https://netify-dev.github.io/netify/reference/add_dyad_vars.md),
that information is used unless overridden by the symmetric parameter.

## Author

shahryar minhas

## Examples

``` r
# load example data
data(icews)

# create a netify object with verbal cooperation as the main network
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

# check the current network
print(net)
#> ✔ Network data created.
#> • Unipartite
#> • Asymmetric
#> • Weights from `verbCoop`
#> • Cross-Sectional
#> • # Unique Actors: 152
#> Network Summary Statistics:
#>           dens miss   mean recip trans
#> verbCoop 0.435    0 41.721 0.982 0.639
#> • Nodal Features: None
#> • Dyad Features: matlCoop

# pivot to make material cooperation the main network
net_pivoted <- pivot_dyad_to_network(
    net,
    dyad_var = "matlCoop",
    network_var_name = "verbCoop"
)
#> ℹ Auto-detected symmetry for 'matlCoop': asymmetric
#> ✔ Successfully pivoted 'matlCoop' to be the main network
#> ℹ Previous network preserved as dyadic variable 'verbCoop'

# the main network is now material cooperation
print(net_pivoted)
#> ✔ Network data created.
#> • Unipartite
#> • Asymmetric
#> • Weighted: matlCoop
#> • Cross-Sectional
#> • # Unique Actors: 152
#> Network Summary Statistics:
#>           dens miss  mean recip trans
#> verbCoop 0.093    0 5.073 0.442 0.333
#> • Nodal Features: None
#> • Dyad Features: verbCoop

# the old network (verbal cooperation) is preserved as a dyadic attribute
attr(net_pivoted, "dyad_data")[["1"]][["verbCoop"]][1:5, 1:5]
#>             Afghanistan Albania Algeria Angola Argentina
#> Afghanistan          NA       0       0      0         1
#> Albania               5      NA       0      0         0
#> Algeria               5       1      NA      8         0
#> Angola                0       0       6     NA         7
#> Argentina             2       0       0      8        NA
```
