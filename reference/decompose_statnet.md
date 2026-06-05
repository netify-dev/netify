# decompose a network object into base r components

`decompose_statnet` (also available as `decompose_network`) extracts the
adjacency matrix and any vertex/edge attributes from a network object
(from the statnet suite), returning them in a standardized list format.

## Usage

``` r
decompose_statnet(ntwk, weight = NULL)

decompose_network(ntwk, weight = NULL)
```

## Arguments

- ntwk:

  a network object (class "network") to be decomposed.

- weight:

  character string specifying the edge attribute to use as weights in
  the adjacency matrix. if NULL (default), the unweighted adjacency
  matrix is returned with 1s for edges and 0s for non-edges.

## Value

a list containing four elements:

- **adj_mat**: the adjacency matrix extracted from the network object

  - for unipartite networks: square matrix of dimension \\n \times n\\

  - for bipartite networks: full square matrix of dimension \\(n_1 +
    n_2) \times (n_1 + n_2)\\

  - values are edge weights if specified, otherwise 0/1

- **ndata**: a data frame of vertex attributes, or NULL if none exist

  - always includes an 'actor' column with vertex names

  - additional columns for each vertex attribute (excluding system
    attributes)

- **ddata**: a data frame of edge attributes, or NULL if none exist

  - columns 'from' and 'to' specify edge endpoints using vertex names

  - additional columns for each edge attribute

- **weight**: the edge attribute name used for weights, if provided

## Details

the function handles both unipartite and bipartite networks
appropriately:

**network type detection:**

- bipartite networks are identified using
  [`is.bipartite()`](https://r.igraph.org/reference/is.bipartite.html)

- the bipartite partition size is retrieved from the 'bipartite' network
  attribute

**vertex naming:**

the function checks for existing vertex names in the 'vertex.names'
attribute. if names are just the default numeric sequence (1, 2, 3,
...), they are treated as missing. default names are assigned when
needed:

- unipartite networks: "a1", "a2", ..., "an"

- bipartite networks: "r1", "r2", ... for first partition; "c1", "c2",
  ... for second partition

**matrix extraction:**

unlike igraph's bipartite handling, the network package returns the full
adjacency matrix even for bipartite networks. the function:

- extracts the full matrix using `as.matrix.network.adjacency()`

- for bipartite networks, the matrix has dimension \\(n_1 + n_2) \times
  (n_1 + n_2)\\ with the first \\n_1\\ rows/columns for the first
  partition

**attribute handling:**

system attributes ('vertex.names' and 'na') are excluded from the vertex
attribute data frame. all user-defined vertex and edge attributes are
preserved.

## Note

for longitudinal networks with changing actor composition, explicitly
set vertex names before decomposition to ensure consistent actor
identification across time periods.

the adjacency matrix format differs between this function and
`decompose_igraph` for bipartite networks: this function returns the
full square matrix while `decompose_igraph` returns only the rectangular
bipartite portion.

edge directions are preserved in the adjacency matrix according to the
network's directed/undirected property.

## Author

cassy dorff, shahryar minhas
