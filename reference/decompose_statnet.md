# Decompose a network object into base R components

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

  A network object (class "network") to be decomposed.

- weight:

  Character string specifying the edge attribute to use as weights in
  the adjacency matrix. If NULL (default), the unweighted adjacency
  matrix is returned with 1s for edges and 0s for non-edges.

## Value

A list containing four elements:

- **adj_mat**: The adjacency matrix extracted from the network object

  - For unipartite networks: Square matrix of dimension n×n

  - For bipartite networks: Full square matrix of dimension
    (n₁+n₂)×(n₁+n₂)

  - Values are edge weights if specified, otherwise 0/1

- **ndata**: A data frame of vertex attributes, or NULL if none exist

  - Always includes an 'actor' column with vertex names

  - Additional columns for each vertex attribute (excluding system
    attributes)

- **ddata**: A data frame of edge attributes, or NULL if none exist

  - Columns 'from' and 'to' specify edge endpoints using vertex names

  - Additional columns for each edge attribute

- **weight**: The edge attribute name used for weights, if provided

## Details

The function handles both unipartite and bipartite networks
appropriately:

**Network type detection:**

- Bipartite networks are identified using
  [`is.bipartite()`](https://r.igraph.org/reference/is.bipartite.html)

- The bipartite partition size is retrieved from the 'bipartite' network
  attribute

**Vertex naming:**

The function checks for existing vertex names in the 'vertex.names'
attribute. If names are just the default numeric sequence (1, 2, 3,
...), they are treated as missing. Default names are assigned when
needed:

- Unipartite networks: "a1", "a2", ..., "an"

- Bipartite networks: "r1", "r2", ... for first partition; "c1", "c2",
  ... for second partition

**Matrix extraction:**

Unlike igraph's bipartite handling, the network package returns the full
adjacency matrix even for bipartite networks. The function:

- Extracts the full matrix using `as.matrix.network.adjacency()`

- For bipartite networks, the matrix has dimension (n₁+n₂)×(n₁+n₂) with
  the first n₁ rows/columns for the first partition

**Attribute handling:**

System attributes ('vertex.names' and 'na') are excluded from the vertex
attribute data frame. All user-defined vertex and edge attributes are
preserved.

## Note

For longitudinal networks with changing actor composition, explicitly
set vertex names before decomposition to ensure consistent actor
identification across time periods.

The adjacency matrix format differs between this function and
`decompose_igraph` for bipartite networks: this function returns the
full square matrix while `decompose_igraph` returns only the rectangular
bipartite portion.

Edge directions are preserved in the adjacency matrix according to the
network's directed/undirected property.

## Author

Cassy Dorff, Shahryar Minhas
