# Convert igraph, network, or matrix objects to netify format

converts various network object types (igraph, network, matrices/arrays,
or lists of these) into netify objects (also available as `to_netify`).
automatically extracts adjacency matrices and any nodal/dyadic
attributes from the input objects.

## Usage

``` r
to_netify(net_obj, weight = NULL, ...)
```

## Arguments

- net_obj:

  an r object to convert: `igraph`, `network`, matrix, array, or a list
  of these objects.

- weight:

  optional. name of the weight attribute in `net_obj` to be used as the
  main edge weight in the netify object. default is `NULL`. important to
  specify for `igraph` and `network` objects as they do not have a
  default weight.

- ...:

  additional arguments passed to `new_netify`. can include `nodal_data`
  or `dyad_data` to override extracted attributes.

## Value

a netify object with:

- adjacency matrix or list of matrices

- nodal attributes (if present in the input)

- dyadic attributes (if present in the input)

- weight specification (if provided)

## Details

the function handles different input types:

- **igraph**: extracts adjacency matrix, vertex attributes as nodal
  data, and edge attributes as dyadic data

- **network**: extracts adjacency matrix, vertex attributes as nodal
  data, and edge attributes as dyadic data

- **matrix**: direct conversion, no attribute extraction

- **array**: assumes 3d arrays represent longitudinal networks

- **list**: must contain all objects of the same type (all igraph, all
  network, or all matrices)

for longitudinal data (lists or 3d arrays), the function creates a
netify object with time-indexed components. actor ordering is preserved
from the input objects and made consistent across all components
(adjacency, nodal, and dyadic data).

## Note

when converting from igraph or network objects, specify the `weight`
parameter to designate which edge attribute should be used as the
primary edge weight in the netify object.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
if (FALSE) { # \dontrun{
# from igraph
library(igraph)
g <- sample_gnp(10, 0.3)
e(g)$weight <- runif(ecount(g))
v(g)$type <- sample(c("a", "b"), vcount(g), replace = TRUE)

net <- to_netify(g, weight = "weight")

# from network
library(network)
n <- network(rgraph(10, tprob = 0.3))
set.vertex.attribute(n, "group", sample(1:2, 10, replace = TRUE))

net <- to_netify(n)

# from matrix
adj_mat <- matrix(rnorm(100), 10, 10)
net <- to_netify(adj_mat)

# from list of matrices (longitudinal)
mat_list <- list(
    "2001" = matrix(rnorm(100), 10, 10),
    "2002" = matrix(rnorm(100), 10, 10)
)
net <- to_netify(mat_list)
} # }
```
