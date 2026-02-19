# Decompose an igraph object into base R components

`decompose_igraph` extracts the adjacency matrix and any vertex/edge
attributes from an igraph object, returning them in a standardized list
format.

## Usage

``` r
decompose_igraph(grph, weight = NULL)
```

## Arguments

- grph:

  An igraph object to be decomposed.

- weight:

  Character string specifying the edge attribute to use as weights in
  the adjacency matrix. If NULL (default), the unweighted adjacency
  matrix is returned with 1s for edges and 0s for non-edges.

## Value

A list containing four elements:

- **adj_mat**: The adjacency matrix extracted from the igraph object

  - For unipartite graphs: Square matrix of dimension n×n

  - For bipartite graphs: Rectangular matrix of dimension n₁×n₂

  - Values are edge weights if specified, otherwise 0/1

- **ndata**: A data frame of vertex attributes, or NULL if none exist

  - Always includes an 'actor' column with vertex names

  - Additional columns for each vertex attribute

- **ddata**: A data frame of edge attributes, or NULL if none exist

  - Columns 'from' and 'to' specify edge endpoints

  - Additional columns for each edge attribute

- **weight**: The edge attribute name used for weights, if provided

## Details

The function handles both unipartite and bipartite graphs appropriately:

**Graph type detection:**

- Bipartite graphs must have a logical 'type' vertex attribute

- If the 'type' attribute exists but is not logical, the graph is
  treated as unipartite with a warning

**Vertex naming:**

If the graph lacks vertex names, default names are assigned:

- Unipartite graphs: "a1", "a2", ..., "an"

- Bipartite graphs: "r1", "r2", ... for type 1; "c1", "c2", ... for type
  2

Existing vertex names are always preserved and used in the output.

**Matrix extraction:**

- Unipartite: Uses
  [`as_adjacency_matrix()`](https://r.igraph.org/reference/as_adjacency_matrix.html)
  to get n×n matrix

- Bipartite: Uses
  [`as_biadjacency_matrix()`](https://r.igraph.org/reference/as_biadjacency_matrix.html)
  to get n₁×n₂ matrix where rows correspond to type=FALSE vertices and
  columns to type=TRUE vertices

**Attribute handling:**

All vertex and edge attributes are preserved in the output data frames.
System attributes (like 'name' and 'type') are included alongside
user-defined attributes.

## Note

For longitudinal networks with changing actor composition, explicitly
set vertex names before decomposition to ensure consistent actor
identification across time periods.

The adjacency matrix is always returned as a standard R matrix (not
sparse), which may have memory implications for very large graphs.

When edge attributes are used as weights, ensure they contain numeric
values. Non-numeric edge attributes will cause an error.

## Author

Cassy Dorff, Shahryar Minhas
