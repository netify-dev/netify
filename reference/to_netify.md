# Convert igraph, network, or matrix objects to netify format

Converts various network object types (igraph, network, matrices/arrays,
or lists of these) into netify objects (also available as `to_netify`).
Automatically extracts adjacency matrices and any nodal/dyadic
attributes from the input objects.

## Usage

``` r
to_netify(net_obj, weight = NULL, ...)
```

## Arguments

- net_obj:

  An R object to convert: `igraph`, `network`, matrix, array, or a list
  of these objects.

- weight:

  Optional. Name of the weight attribute in `net_obj` to be used as the
  main edge weight in the netify object. Default is `NULL`. Important to
  specify for `igraph` and `network` objects as they do not have a
  default weight.

- ...:

  Additional arguments passed to `new_netify`. Can include `nodal_data`
  or `dyad_data` to override extracted attributes.

## Value

A netify object with:

- Adjacency matrix or list of matrices

- Nodal attributes (if present in the input)

- Dyadic attributes (if present in the input)

- Weight specification (if provided)

## Details

The function handles different input types:

- **igraph**: Extracts adjacency matrix, vertex attributes as nodal
  data, and edge attributes as dyadic data

- **network**: Extracts adjacency matrix, vertex attributes as nodal
  data, and edge attributes as dyadic data

- **matrix**: Direct conversion, no attribute extraction

- **array**: Assumes 3D arrays represent longitudinal networks

- **list**: Must contain all objects of the same type (all igraph, all
  network, or all matrices)

For longitudinal data (lists or 3D arrays), the function creates a
netify object with time-indexed components. Actor ordering is preserved
from the input objects and made consistent across all components
(adjacency, nodal, and dyadic data).

## Note

When converting from igraph or network objects, specify the `weight`
parameter to designate which edge attribute should be used as the
primary edge weight in the netify object.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
if (FALSE) { # \dontrun{
# From igraph
library(igraph)
g <- sample_gnp(10, 0.3)
E(g)$weight <- runif(ecount(g))
V(g)$type <- sample(c("A", "B"), vcount(g), replace = TRUE)

net <- to_netify(g, weight = "weight")

# From network
library(network)
n <- network(rgraph(10, tprob = 0.3))
set.vertex.attribute(n, "group", sample(1:2, 10, replace = TRUE))

net <- to_netify(n)

# From matrix
adj_mat <- matrix(rnorm(100), 10, 10)
net <- to_netify(adj_mat)

# From list of matrices (longitudinal)
mat_list <- list(
    "2001" = matrix(rnorm(100), 10, 10),
    "2002" = matrix(rnorm(100), 10, 10)
)
net <- to_netify(mat_list)
} # }
```
