# Extract measurements and dimensions from a netify object

`netify_measurements` (also available as `measurements`) extracts
comprehensive information about the structure, dimensions, and
attributes of a netify object. This function provides a standardized way
to inspect network properties across different netify types.

## Usage

``` r
netify_measurements(netlet)

measurements(netlet)
```

## Arguments

- netlet:

  A netify object (class "netify") to analyze. Can be cross-sectional,
  longitudinal array, or longitudinal list format.

## Value

A list containing measurements of the netify object with the following
components (availability depends on netify type):

**Actor information:**

- `row_actors`: Character vector (or list) of row actor names

- `col_actors`: Character vector (or list) of column actor names

- `n_row_actors`: Integer (or list) count of row actors

- `n_col_actors`: Integer (or list) count of column actors

**Temporal information:**

- `time`: Character vector of time period labels (NULL for
  cross-sectional)

- `n_time`: Integer count of time periods (NULL for cross-sectional)

**Layer information:**

- `layers`: Character vector of layer names (NULL if single layer)

- `n_layers`: Integer count of layers (NULL if single layer)

**Attribute information:**

- `nvars`: Character vector of nodal variable names

- `n_nvars`: Integer count of nodal variables

- `dvars`: Character vector of dyadic variable names

- `n_dvars`: Integer count of dyadic variables

## Details

The function will adapt its output based on the netify object type.

## Author

Cassy Dorff, Shahryar Minhas
