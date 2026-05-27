# Convert netify objects to dbn format

`netify_to_dbn` (also available as `to_dbn`) transforms netify network
objects into the array format required by the dbn package for Dynamic
Bilinear Network models. This enables the use of longitudinal latent
space models for multilayer and single-layer networks.

## Usage

``` r
netify_to_dbn(netlet)

to_dbn(netlet)
```

## Arguments

- netlet:

  A netify object (class "netify") containing longitudinal network data.
  Must be of type `longit_array` or `longit_list`. Supports both
  single-layer and multilayer networks.

## Value

A list containing:

- **Y**:

  Network adjacency data as a 4D array of dimensions
  `[n_actors, n_actors, n_layers, n_time]`. For single-layer networks,
  `n_layers = 1` and the third dimension is labeled with the weight
  variable name (or `"edge_value"` for binary networks). Missing edges
  are preserved as NA.

- **Xdyad**:

  Dyadic covariates as a 4D array of dimensions
  `[n_actors, n_actors, n_covariates, n_time]`, or NULL if none exist.

- **Xrow**:

  Sender/row actor attributes as a 3D array of dimensions
  `[n_actors, n_attributes, n_time]`, or NULL if none exist.

- **Xcol**:

  Receiver/column actor attributes as a 3D array of dimensions
  `[n_actors, n_attributes, n_time]`, or NULL if none exist. For
  symmetric networks, Xcol is identical to Xrow.

## Details

The dbn package expects a 4D array with dimensions `[n, n, p, T]` where
`n` is the number of actors, `p` is the number of relation types
(layers), and `T` is the number of time periods.

**Supported netify types:**

- `longit_array`: Directly extracts or reshapes the underlying array

- `longit_list`: Converts to array format first (actors are unioned
  across time, missing entries become NA)

Cross-sectional networks are not supported since dbn is designed for
longitudinal data.

**Variable requirements:**

- All nodal attributes must be numeric (integer or double)

- Character or factor variables must be converted before using this
  function

## Author

Shahryar Minhas

Cassy Dorff, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Create two longitudinal networks
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

# Create multilayer network
multi_net <- layer_netify(
    list(verbal_net, material_net),
    layer_labels = c("Verbal", "Material")
)

# Convert to dbn format
dbn_data <- netify_to_dbn(multi_net)
dim(dbn_data$Y) # [n_actors, n_actors, 2, n_years]
#> [1] 152 152   2  13

if (FALSE) { # \dontrun{
# Single-layer also works
dbn_single <- netify_to_dbn(verbal_net)
dim(dbn_single$Y) # [n_actors, n_actors, 1, n_years]
} # }
```
