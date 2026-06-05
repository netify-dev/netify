# decompose an igraph object into base r components

`decompose_igraph` extracts the adjacency matrix and any vertex/edge
attributes from an igraph object, returning them in a standardized list
format.

## Usage

``` r
decompose_igraph(grph, weight = NULL)
```

## Arguments

- grph:

  an igraph object to be decomposed.

- weight:

  character string specifying the edge attribute to use as weights in
  the adjacency matrix. if NULL (default), the unweighted adjacency
  matrix is returned with 1s for edges and 0s for non-edges.

## Value

a list containing four elements:

- **adj_mat**: the adjacency matrix extracted from the igraph object

  - for unipartite graphs: square matrix of dimension \\n \times n\\

  - for bipartite graphs: rectangular matrix of dimension \\n_1 \times
    n_2\\

  - values are edge weights if specified, otherwise 0/1

- **ndata**: a data frame of vertex attributes, or NULL if none exist

  - always includes an 'actor' column with vertex names

  - additional columns for each vertex attribute

- **ddata**: a data frame of edge attributes, or NULL if none exist

  - columns 'from' and 'to' specify edge endpoints

  - additional columns for each edge attribute

- **weight**: the edge attribute name used for weights, if provided

## Details

the function handles both unipartite and bipartite graphs appropriately:

**graph type detection:**

- bipartite graphs must have a logical 'type' vertex attribute

- if the 'type' attribute exists but is not logical, the graph is
  treated as unipartite with a warning

**vertex naming:**

if the graph lacks vertex names, default names are assigned:

- unipartite graphs: "a1", "a2", ..., "an"

- bipartite graphs: "r1", "r2", ... for type 1; "c1", "c2", ... for type
  2

existing vertex names are always preserved and used in the output.

**matrix extraction:**

- unipartite: uses
  [`as_adjacency_matrix()`](https://r.igraph.org/reference/as_adjacency_matrix.html)
  to get \\n \times n\\ matrix

- bipartite: uses
  [`as_biadjacency_matrix()`](https://r.igraph.org/reference/as_biadjacency_matrix.html)
  to get \\n_1 \times n_2\\ matrix where rows correspond to type=FALSE
  vertices and columns to type=TRUE vertices

**attribute handling:**

all vertex and edge attributes are preserved in the output data frames.
system attributes (like 'name' and 'type') are included alongside
user-defined attributes.

## Note

for longitudinal networks with changing actor composition, explicitly
set vertex names before decomposition to ensure consistent actor
identification across time periods.

the adjacency matrix is always returned as a standard r matrix (not
sparse), which may have memory implications for very large graphs.

when edge attributes are used as weights, ensure they contain numeric
values. non-numeric edge attributes will cause an error.

## Author

cassy dorff, shahryar minhas
