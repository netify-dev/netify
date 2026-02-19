# Generate edge layout coordinates for netify visualization

`get_edge_layout` prepares edge data for network visualization by
calculating start and end coordinates for line segments representing
edges. This function maps edges from a netify object to their
corresponding node positions as determined by a layout algorithm.

## Usage

``` r
get_edge_layout(netlet, nodes_layout, ig_netlet = NULL)
```

## Arguments

- netlet:

  A netify object (class "netify") containing the network structure from
  which edges will be extracted.

- nodes_layout:

  A data.frame or matrix containing node positions, or a list of such
  objects for longitudinal networks. Each element must include columns:

  - **actor**: Character string identifying each node

  - **x**: Numeric x-coordinate of the node position

  - **y**: Numeric y-coordinate of the node position

  For longitudinal networks, provide a named list where:

  - Names correspond to time periods in the netify object

  - Each element follows the structure described above

  - Time period names must match those in the netify object

- ig_netlet:

  An optional pre-converted igraph object. If provided, this function
  will use it directly instead of converting the netify object again.

## Value

Depending on the input netify object:

- **Cross-sectional**: A list containing one data.frame with columns:

  - `from`: Source node name

  - `to`: Target node name

  - `x1`, `y1`: Coordinates of the source node

  - `x2`, `y2`: Coordinates of the target node

- **Longitudinal**: A named list of data.frames (one per time period)
  with the same structure as above

The output maintains the same temporal structure as the input netify
object.

## Details

This function performs the following operations:

**Edge extraction:**

- Converts the netify object to igraph format internally

- Extracts the edge list preserving edge directions

- Handles both cross-sectional and longitudinal networks

**Coordinate mapping:**

- Matches each edge endpoint to its corresponding node position

- Creates a complete set of coordinates for drawing edges

- Preserves the temporal structure for longitudinal networks

**Use in visualization:**

This function is typically used as part of a visualization pipeline:

1.  Create node layout using
    [`get_node_layout()`](https://netify-dev.github.io/netify/reference/get_node_layout.md)
    or a custom layout algorithm

2.  Generate edge coordinates using this function

3.  Pass both to visualization functions for plotting

## Note

The nodes_layout structure must exactly match the actors and time
periods in the netify object. Missing actors in the layout will result
in NA coordinates for their associated edges.

For longitudinal networks, ensure that the names of the nodes_layout
list match the time period labels in the netify object (e.g., "2008",
"2009").

This function always returns a list structure for consistency, even for
cross-sectional networks where the list contains only one element.

## Author

Cassy Dorff, Shahryar Minhas
