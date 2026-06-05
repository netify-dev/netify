# Convert netify objects to dbn format

`netify_to_dbn` (also available as `to_dbn`) transforms netify network
objects into the array format required by the dbn package for dynamic
bilinear network models. this enables the use of longitudinal latent
space models for multilayer and single-layer networks.

## Usage

``` r
netify_to_dbn(netlet)

to_dbn(netlet)
```

## Arguments

- netlet:

  a netify object (class "netify") containing longitudinal network data.
  must be of type `longit_array` or `longit_list`. supports both
  single-layer and multilayer networks.

## Value

a list containing:

- **y**:

  network adjacency data as a 4d array of dimensions
  `[n_actors, n_actors, n_layers, n_time]`. for single-layer networks,
  `n_layers = 1` and the third dimension is labeled with the weight
  variable name (or `"edge_value"` for binary networks). missing edges
  are preserved as na.

- **xdyad**:

  dyadic covariates as a 4d array of dimensions
  `[n_actors, n_actors, n_covariates, n_time]`, or NULL if none exist.

- **xrow**:

  sender/row actor attributes as a 3d array of dimensions
  `[n_actors, n_attributes, n_time]`, or NULL if none exist.

- **xcol**:

  receiver/column actor attributes as a 3d array of dimensions
  `[n_actors, n_attributes, n_time]`, or NULL if none exist. for
  symmetric networks, xcol is identical to xrow.

## Details

the dbn package expects a 4d array with dimensions `[n, n, p, t]` where
`n` is the number of actors, `p` is the number of relation types
(layers), and `t` is the number of time periods.

**supported netify types:**

- `longit_array`: directly extracts or reshapes the underlying array

- `longit_list`: converts to array format first (actors are unioned
  across time, missing entries become na)

cross-sectional networks are not supported since dbn is designed for
longitudinal data. bipartite networks are also not supported because dbn
expects square actor-by-actor arrays.

**variable requirements:**

- all nodal attributes must be numeric (integer or double)

- all dyadic attributes must be numeric or logical matrices

- character or factor variables must be converted before using this
  function

## Author

shahryar minhas

cassy dorff, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# create two longitudinal networks
verbal_net <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    output_format = "longit_array"
)

material_net <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "matlCoop",
    output_format = "longit_array"
)

# create multilayer network
multi_net <- layer_netify(
    list(verbal_net, material_net),
    layer_labels = c("verbal", "material")
)

# convert to dbn format
dbn_data <- netify_to_dbn(multi_net)
dim(dbn_data$y) # [n_actors, n_actors, 2, n_years]
#> NULL

if (FALSE) { # \dontrun{
# single-layer also works
dbn_single <- netify_to_dbn(verbal_net)
dim(dbn_single$y) # [n_actors, n_actors, 1, n_years]
} # }
```
