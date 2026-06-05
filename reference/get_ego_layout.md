# Calculate ego-centric layout positions for network visualization

`get_ego_layout` computes node positions for ego network visualization
using ego-centric layout algorithms. these layouts place the ego at the
center and arrange alters around it (radially, by concentric ring, etc.)
so that the ego's relationships are organized around the focal node.

## Usage

``` r
get_ego_layout(
  netlet,
  layout = "star",
  group_by = NULL,
  order_by = NULL,
  weight_to_distance = FALSE,
  ring_gap = 0.3,
  ego_size = 0.1,
  seed = 6886
)
```

## Arguments

- netlet:

  a netify object (class "netify") that is an ego network, created using
  [`ego_netify`](https://netify-dev.github.io/netify/reference/ego_netify.md).
  the object must have ego_netify = TRUE attribute.

- layout:

  character string specifying the ego-centric layout algorithm. options:

  - `"radial"`: places ego at center with alters arranged in a circle
    around it. distance from center can encode relationship strength.

  - `"concentric"`: places ego at center with alters in concentric
    circles based on a grouping variable or relationship strength.

  - `"star"`: simple star layout with ego at center and alters equally
    spaced around it (default).

- group_by:

  character string specifying a nodal attribute to use for grouping
  alters in the layout. for "radial" layout, groups are arranged in
  sectors. for "concentric" layout, groups determine which ring alters
  appear in. if NULL (default), no grouping is applied.

- order_by:

  character string specifying a nodal attribute to use for ordering
  alters within their groups or around the ego. common options include
  network statistics like "degree_total" or custom attributes. if NULL
  (default), alters are arranged alphabetically.

- weight_to_distance:

  logical. if TRUE and the network is weighted, use edge weights to
  determine distance from ego (higher weights = closer). for "radial"
  layout only. default is FALSE.

- ring_gap:

  numeric value between 0 and 1 specifying the gap between concentric
  rings as a proportion of the total radius. only used for "concentric"
  layout. default is 0.3.

- ego_size:

  numeric value specifying the relative size of the central area
  reserved for the ego. larger values create more space between ego and
  alters. default is 0.1.

- seed:

  integer for random number generation to ensure reproducible layouts
  when there are ties in ordering. default is 6886.

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

this function provides specialized layouts for ego networks that
emphasize the ego-alter structure:

**layout algorithms:**

- **radial**: places the ego at the origin and arranges alters in a
  circle around it. if grouping is specified, alters are arranged in
  sectors with related nodes near each other. if weight_to_distance is
  TRUE, alters with stronger ties to ego are placed closer to the
  center.

- **concentric**: places the ego at the origin and arranges alters in
  concentric circles. the ring assignment can be based on a grouping
  variable (categorical) or a continuous variable (discretized into
  rings).

- **star**: a simple star layout that places ego at center and
  distributes alters evenly around a single circle. this is equivalent
  to the radial layout without grouping or weighting.

**visual encoding:**

the layouts allow encoding of network properties through spatial
arrangement:

- **distance from ego**: can represent tie strength, frequency of
  interaction, or other dyadic measures

- **angular position**: can group similar alters together (e.g., family,
  friends, colleagues)

- **ring assignment**: can represent categories, levels of importance,
  or discretized continuous variables

**longitudinal networks:**

for longitudinal ego networks, the function maintains consistent angular
positions for alters across time periods when possible, making it easier
to track changes in the ego's network over time.

## Note

this function is designed specifically for ego networks created with
[`ego_netify`](https://netify-dev.github.io/netify/reference/ego_netify.md).
for general network layouts, use
[`get_node_layout`](https://netify-dev.github.io/netify/reference/get_node_layout.md).

the function will issue a warning if used on non-ego networks but will
attempt to proceed by treating the first node as the ego.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
if (FALSE) { # \dontrun{
# create an ego network
ego_net <- ego_netify(my_network, ego = "alice")

# get radial layout with alters grouped by attribute
layout_radial <- get_ego_layout(ego_net,
                               layout = "radial",
                               group_by = "department")

# get concentric layout with rings based on degree
layout_circles <- get_ego_layout(ego_net,
                                layout = "concentric",
                                group_by = "degree_total")

# use with plot
plot(ego_net, point_layout = layout_radial)
} # }
```
