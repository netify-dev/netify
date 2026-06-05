# low-level constructor for netify objects

`new_netify` (also available as `new_netlet`) is a low-level constructor
that creates netify objects from raw matrix, array, or list data
structures. this function automatically detects network properties and
sets appropriate attributes, making it useful for converting existing
network data into the netify format.

## Usage

``` r
new_netify(data, ...)
```

## Arguments

- data:

  a network data structure to convert:

  - **matrix**: creates a cross-sectional netify object

  - **3d array**: creates a longitudinal array netify object
    (dimensions: actors x actors x time)

  - **list of matrices**: creates a longitudinal list netify object
    (useful for time-varying actor composition)

- ...:

  additional parameters to set as attributes on the netify object.
  common parameters include:

  - `symmetric`: logical indicating if network is undirected

  - `mode`: "unipartite" or "bipartite"

  - `weight`: name of the edge weight variable

  - `diag_to_NA`: whether to set diagonal to na

  - `missing_to_zero`: whether to treat missing edges as zeros

  - `nodal_data`: data frame of node attributes

  - `dyad_data`: dyadic attributes (see netify documentation)

  if not provided, these properties are automatically detected from the
  data.

## Value

a netify object with class "netify" and appropriate structure:

- for matrices: a single netify matrix with netify_type = "cross_sec"

- for arrays: a netify array with netify_type = "longit_array"

- for lists: a netify list with netify_type = "longit_list", where each
  element is itself a netify object

all netify objects include automatically detected or user-specified
attributes for network properties, making them ready for use with netify
functions.

## Details

**automatic property detection:**

when properties are not explicitly provided, `new_netify` intelligently
detects:

- **symmetry**: checks if matrix equals its transpose

- **mode**: infers unipartite/bipartite from dimensions and actor names

- **edge weights**: detects binary (0/1) vs. weighted networks

- **diagonal treatment**: checks if diagonal contains all nas

- **missing values**: determines if nas exist off-diagonal

- **actor composition**: for longitudinal data, detects if actors remain
  constant or vary over time

**naming conventions:**

if row/column names are not provided:

- unipartite networks: actors named "a1", "a2", ...

- bipartite networks: row actors "r1", "r2", ...; column actors "c1",
  "c2", ...

- time periods: named as "1", "2", ... if not specified

**longitudinal data handling:**

for longitudinal networks:

- arrays assume constant actor composition across time

- lists allow for time-varying actor composition

- each time slice in a list becomes a separate cross-sectional netify
  object

- properties are detected across all time periods (e.g., symmetric if
  all time slices are symmetric)

## Note

this is a low-level constructor primarily intended for package
developers or advanced users. most users should use the higher-level
[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
function, which provides more validation and preprocessing.

the function does not support multilayer networks directly. to create
multilayer networks, create separate netify objects and combine them
with
[`layer_netify()`](https://netify-dev.github.io/netify/reference/layer_netify.md).

while the function attempts to detect network properties automatically,
explicitly providing these parameters is recommended for clarity.

## Author

cassy dorff, shahryar minhas
