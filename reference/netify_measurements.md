# Extract measurements and dimensions from a netify object

`netify_measurements` (also available as `measurements`) extracts
information about the structure, dimensions, and attributes of a netify
object. this function provides a standardized way to inspect network
properties across different netify types.

## Usage

``` r
netify_measurements(netlet)

measurements(netlet)
```

## Arguments

- netlet:

  a netify object (class "netify") to analyze. can be cross-sectional,
  longitudinal array, or longitudinal list format.

## Value

a list containing measurements of the netify object with the following
components (availability depends on netify type):

**actor information:**

- `row_actors`: character vector (or list) of row actor names

- `col_actors`: character vector (or list) of column actor names

- `n_row_actors`: integer (or list) count of row actors

- `n_col_actors`: integer (or list) count of column actors

**temporal information:**

- `time`: character vector of time period labels (NULL for
  cross-sectional)

- `n_time`: integer count of time periods (NULL for cross-sectional)

**layer information:**

- `layers`: character vector of layer names (NULL if single layer)

- `n_layers`: integer count of layers (NULL if single layer)

**attribute information:**

- `nvars`: character vector of nodal variable names

- `n_nvars`: integer count of nodal variables

- `dvars`: character vector of dyadic variable names

- `n_dvars`: integer count of dyadic variables

## Details

the function will adapt its output based on the netify object type.

## Author

cassy dorff, shahryar minhas
