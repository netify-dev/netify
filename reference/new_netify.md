# Low-level constructor for netify objects

`new_netify` (also available as `new_netlet`) is a low-level constructor
that creates netify objects from raw matrix, array, or list data
structures. This function automatically detects network properties and
sets appropriate attributes, making it useful for converting existing
network data into the netify format.

## Usage

``` r
new_netify(data, ...)
```

## Arguments

- data:

  A network data structure to convert:

  - **Matrix**: Creates a cross-sectional netify object

  - **3D array**: Creates a longitudinal array netify object
    (dimensions: actors × actors × time)

  - **List of matrices**: Creates a longitudinal list netify object
    (useful for time-varying actor composition)

- ...:

  Additional parameters to set as attributes on the netify object.
  Common parameters include:

  - `symmetric`: Logical indicating if network is undirected

  - `mode`: "unipartite" or "bipartite"

  - `weight`: Name of the edge weight variable

  - `diag_to_NA`: Whether to set diagonal to NA

  - `missing_to_zero`: Whether to treat missing edges as zeros

  - `nodal_data`: Data frame of node attributes

  - `dyad_data`: Dyadic attributes (see netify documentation)

  If not provided, these properties are automatically detected from the
  data.

## Value

A netify object with class "netify" and appropriate structure:

- For matrices: A single netify matrix with netify_type = "cross_sec"

- For arrays: A netify array with netify_type = "longit_array"

- For lists: A netify list with netify_type = "longit_list", where each
  element is itself a netify object

All netify objects include automatically detected or user-specified
attributes for network properties, making them ready for use with netify
functions.

## Details

**Automatic property detection:**

When properties are not explicitly provided, `new_netify` intelligently
detects:

- **Symmetry**: Checks if matrix equals its transpose

- **Mode**: Infers unipartite/bipartite from dimensions and actor names

- **Edge weights**: Detects binary (0/1) vs. weighted networks

- **Diagonal treatment**: Checks if diagonal contains all NAs

- **Missing values**: Determines if NAs exist off-diagonal

- **Actor composition**: For longitudinal data, detects if actors remain
  constant or vary over time

**Naming conventions:**

If row/column names are not provided:

- Unipartite networks: Actors named "a1", "a2", ...

- Bipartite networks: Row actors "r1", "r2", ...; column actors "c1",
  "c2", ...

- Time periods: Named as "1", "2", ... if not specified

**Longitudinal data handling:**

For longitudinal networks:

- Arrays assume constant actor composition across time

- Lists allow for time-varying actor composition

- Each time slice in a list becomes a separate cross-sectional netify
  object

- Properties are detected across all time periods (e.g., symmetric if
  ALL time slices are symmetric)

## Note

This is a low-level constructor primarily intended for package
developers or advanced users. Most users should use the higher-level
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
function, which provides more comprehensive data validation and
preprocessing.

The function does not support multilayer networks directly. To create
multilayer networks, create separate netify objects and combine them
with
[`layer_netify()`](https://netify-dev.github.io/netify/reference/layer_netify.md).

While the function attempts to detect network properties automatically,
explicitly providing these parameters is recommended for clarity.

## Author

Cassy Dorff, Shahryar Minhas
