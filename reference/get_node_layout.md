# Calculate node layout positions for netify visualization

`get_node_layout` computes node positions for network visualization
using various layout algorithms from igraph. This function converts a
netify object to igraph format, applies the specified layout algorithm,
and returns node coordinates suitable for plotting.

## Usage

``` r
get_node_layout(
  netlet,
  layout = NULL,
  static_actor_positions = FALSE,
  which_static = NULL,
  seed = 6886,
  ig_netlet = NULL,
  ...
)
```

## Arguments

- netlet:

  A netify object (class "netify") for which to compute layout
  positions.

- layout:

  Character string specifying the layout algorithm to use. Options
  include:

  - `"nicely"`: Automatic selection of appropriate layout (default for
    unipartite)

  - `"bipartite"`: Two-column layout for bipartite networks (default for
    bipartite)

  - `"fruchtermanreingold"` or `"fr"`: Force-directed layout

  - `"kamadakawai"` or `"kk"`: Another force-directed layout

  - `"circle"`: Nodes arranged in a circle

  - `"star"`: Star-shaped layout

  - `"grid"`: Nodes on a grid

  - `"tree"`: Hierarchical tree layout

  - `"random"` or `"randomly"`: Random positions

  - Additional options: `"graphopt"`, `"sugiyama"`, `"drl"`, `"lgl"`,
    `"dh"`, `"gem"`, `"mds"`

  If NULL, defaults to "nicely" for unipartite or "bipartite" for
  bipartite networks.

- static_actor_positions:

  Logical. If TRUE, maintains consistent node positions across all time
  periods in longitudinal networks. If FALSE (default), each time period
  gets its own optimized layout.

- which_static:

  Integer specifying which time period's layout to use as the static
  template when static_actor_positions is TRUE. If NULL (default),
  creates a static layout based on the union of all edges across time
  periods, giving more weight to persistent edges.

- seed:

  Integer for random number generation to ensure reproducible layouts.
  Default is 6886.

- ig_netlet:

  An optional pre-converted igraph object. If provided, this function
  will use it directly instead of converting the netify object again.

- ...:

  Additional arguments passed to ego-specific layout functions when
  layout is "radial" or "concentric". See
  [`get_ego_layout`](https://netify-dev.github.io/netify/reference/get_ego_layout.md)
  for available options (ego_group_by, ego_order_by,
  ego_weight_to_distance, etc.).

## Value

A list of data frames (one per time period) where each data frame
contains:

- **index**: Integer node index

- **actor**: Character string with actor name

- **x**: Numeric x-coordinate for node position

- **y**: Numeric y-coordinate for node position

For cross-sectional networks, returns a list with one element. For
longitudinal networks, returns a named list with time periods as names.

## Details

This function handles layout generation for both cross-sectional and
longitudinal networks with several key features:

**Layout algorithms:**

The function provides access to all major igraph layout algorithms. The
default "nicely" option automatically selects an appropriate algorithm
based on the network structure. For ego networks, specialized layouts
("radial" and "concentric") are available that emphasize the ego-alter
structure.

**Longitudinal layouts:**

For longitudinal networks, two approaches are available:

- **Dynamic layouts**: Each time period gets its own optimized layout,
  which may better reveal structural changes but makes visual comparison
  harder

- **Static layouts**: All time periods use the same node positions,
  facilitating visual comparison of network evolution

When using static layouts with which_static = NULL, the function creates
a composite layout based on the union of all edges across time periods.
Edges that appear more frequently are given higher weight, producing
layouts that emphasize the stable core structure of the network.

**Bipartite networks:**

For bipartite networks, the default layout arranges the two node sets in
separate columns.

## Author

Cassy Dorff, Shahryar Minhas
