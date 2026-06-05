# Generate edge layout coordinates for netify visualization

`get_edge_layout` prepares edge data for network visualization by
calculating start and end coordinates for line segments representing
edges. this function maps edges from a netify object to their
corresponding node positions as determined by a layout algorithm.

## Usage

``` r
get_edge_layout(netlet, nodes_layout, ig_netlet = NULL)
```

## Arguments

- netlet:

  a netify object (class "netify") containing the network structure from
  which edges will be extracted.

- nodes_layout:

  a data.frame or matrix containing node positions, or a list of such
  objects for longitudinal networks. each element must include columns:

  - **actor**: character string identifying each node

  - **x**: numeric x-coordinate of the node position

  - **y**: numeric y-coordinate of the node position

  for longitudinal networks, provide a named list where:

  - names correspond to time periods in the netify object

  - each element follows the structure described above

  - time period names must match those in the netify object

- ig_netlet:

  an optional pre-converted igraph object. if provided, this function
  will use it directly instead of converting the netify object again.

## Value

depending on the input netify object:

- **cross-sectional**: a list containing one data.frame with columns:

  - `from`: source node name

  - `to`: target node name

  - `x1`, `y1`: coordinates of the source node

  - `x2`, `y2`: coordinates of the target node

- **longitudinal**: a named list of data.frames (one per time period)
  with the same structure as above

the output maintains the same temporal structure as the input netify
object.

## Details

this function performs the following operations:

**edge extraction:**

- converts the netify object to igraph format internally

- extracts the edge list preserving edge directions

- handles both cross-sectional and longitudinal networks

**coordinate mapping:**

- matches each edge endpoint to its corresponding node position

- creates a complete set of coordinates for drawing edges

- preserves the temporal structure for longitudinal networks

**use in visualization:**

this function is typically used as part of a visualization pipeline:

1.  create node layout using
    [`get_node_layout()`](https://netify-dev.github.io/netify/reference/get_node_layout.md)
    or a custom layout algorithm

2.  generate edge coordinates using this function

3.  pass both to visualization functions for plotting

## Note

the nodes_layout structure must exactly match the actors and time
periods in the netify object. missing actors in the layout will result
in na coordinates for their associated edges.

for longitudinal networks, ensure that the names of the nodes_layout
list match the time period labels in the netify object (e.g., "2008",
"2009").

this function always returns a list structure for consistency, even for
cross-sectional networks where the list contains only one element.

## Author

cassy dorff, shahryar minhas
