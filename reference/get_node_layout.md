# Calculate node layout positions for netify visualization

`get_node_layout` computes node positions for network visualization
using various layout algorithms from igraph. this function converts a
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

  a netify object (class "netify") for which to compute layout
  positions.

- layout:

  character string specifying the layout algorithm to use. options
  include:

  - `"nicely"`: automatic selection of appropriate layout (default for
    unipartite)

  - `"bipartite"`: two-column layout for bipartite networks (default for
    bipartite)

  - `"fruchtermanreingold"` or `"fr"`: force-directed layout

  - `"kamadakawai"` or `"kk"`: another force-directed layout

  - `"circle"`: nodes arranged in a circle

  - `"star"`: star-shaped layout

  - `"grid"`: nodes on a grid

  - `"tree"`: hierarchical tree layout

  - `"random"` or `"randomly"`: random positions

  - additional options: `"graphopt"`, `"sugiyama"`, `"drl"`, `"lgl"`,
    `"dh"`, `"gem"`, `"mds"`

  if NULL, defaults to "nicely" for unipartite or "bipartite" for
  bipartite networks.

- static_actor_positions:

  logical. if TRUE, maintains consistent node positions across all time
  periods in longitudinal networks. if FALSE (default), each time period
  gets its own optimized layout.

- which_static:

  integer specifying which time period's layout to use as the static
  template when static_actor_positions is TRUE. if NULL (default),
  creates a static layout based on the union of all edges across time
  periods, giving more weight to persistent edges.

- seed:

  integer for random number generation to ensure reproducible layouts.
  default is 6886.

- ig_netlet:

  an optional pre-converted igraph object. if provided, this function
  will use it directly instead of converting the netify object again.

- ...:

  additional arguments passed to ego-specific layout functions when
  layout is "radial" or "concentric". see
  [`get_ego_layout`](https://netify-dev.github.io/netify/reference/get_ego_layout.md)
  for available options (ego_group_by, ego_order_by,
  ego_weight_to_distance, etc.).

## Value

a list of data frames (one per time period) where each data frame
contains:

- **index**: integer node index

- **actor**: character string with actor name

- **x**: numeric x-coordinate for node position

- **y**: numeric y-coordinate for node position

for cross-sectional networks, returns a list with one element. for
longitudinal networks, returns a named list with time periods as names.

## Details

this function handles layout generation for both cross-sectional and
longitudinal networks with several key features:

**layout algorithms:**

the function provides access to all major igraph layout algorithms. the
default "nicely" option automatically selects an appropriate algorithm
based on the network structure. for ego networks, specialized layouts
("radial" and "concentric") are available that emphasize the ego-alter
structure.

**longitudinal layouts:**

for longitudinal networks, two approaches are available:

- **dynamic layouts**: each time period gets its own optimized layout,
  which may better reveal structural changes but makes visual comparison
  harder

- **static layouts**: all time periods use the same node positions,
  facilitating visual comparison of network evolution

when using static layouts with which_static = NULL, the function creates
a composite layout based on the union of all edges across time periods.
edges that appear more frequently are given higher weight, producing
layouts that emphasize the stable core structure of the network.

**bipartite networks:**

for bipartite networks, the default layout arranges the two node sets in
separate columns.

## Author

cassy dorff, shahryar minhas
