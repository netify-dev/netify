# Calculate ego-centric layout positions for network visualization

`get_ego_layout` computes node positions for ego network visualization
using ego-centric layout algorithms. These layouts place the ego at the
center and arrange alters in meaningful patterns around it, making the
ego's relationships more visually apparent.

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

  A netify object (class "netify") that is an ego network, created using
  [`ego_netify`](https://netify-dev.github.io/netify/reference/ego_netify.md).
  The object must have ego_netify = TRUE attribute.

- layout:

  Character string specifying the ego-centric layout algorithm. Options:

  - `"radial"`: Places ego at center with alters arranged in a circle
    around it. Distance from center can encode relationship strength.

  - `"concentric"`: Places ego at center with alters in concentric
    circles based on a grouping variable or relationship strength.

  - `"star"`: Simple star layout with ego at center and alters equally
    spaced around it (default).

- group_by:

  Character string specifying a nodal attribute to use for grouping
  alters in the layout. For "radial" layout, groups are arranged in
  sectors. For "concentric" layout, groups determine which ring alters
  appear in. If NULL (default), no grouping is applied.

- order_by:

  Character string specifying a nodal attribute to use for ordering
  alters within their groups or around the ego. Common options include
  network statistics like "degree_total" or custom attributes. If NULL
  (default), alters are arranged alphabetically.

- weight_to_distance:

  Logical. If TRUE and the network is weighted, use edge weights to
  determine distance from ego (higher weights = closer). For "radial"
  layout only. Default is FALSE.

- ring_gap:

  Numeric value between 0 and 1 specifying the gap between concentric
  rings as a proportion of the total radius. Only used for "concentric"
  layout. Default is 0.3.

- ego_size:

  Numeric value specifying the relative size of the central area
  reserved for the ego. Larger values create more space between ego and
  alters. Default is 0.1.

- seed:

  Integer for random number generation to ensure reproducible layouts
  when there are ties in ordering. Default is 6886.

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

This function provides specialized layouts for ego networks that
emphasize the ego-alter structure:

**Layout algorithms:**

- **Radial**: Places the ego at the origin and arranges alters in a
  circle around it. If grouping is specified, alters are arranged in
  sectors with related nodes near each other. If weight_to_distance is
  TRUE, alters with stronger ties to ego are placed closer to the
  center.

- **Concentric**: Places the ego at the origin and arranges alters in
  concentric circles. The ring assignment can be based on a grouping
  variable (categorical) or a continuous variable (discretized into
  rings).

- **Star**: A simple star layout that places ego at center and
  distributes alters evenly around a single circle. This is equivalent
  to the radial layout without grouping or weighting.

**Visual encoding:**

The layouts allow encoding of network properties through spatial
arrangement:

- **Distance from ego**: Can represent tie strength, frequency of
  interaction, or other dyadic measures

- **Angular position**: Can group similar alters together (e.g., family,
  friends, colleagues)

- **Ring assignment**: Can represent categories, levels of importance,
  or discretized continuous variables

**Longitudinal networks:**

For longitudinal ego networks, the function maintains consistent angular
positions for alters across time periods when possible, making it easier
to track changes in the ego's network over time.

## Note

This function is designed specifically for ego networks created with
[`ego_netify`](https://netify-dev.github.io/netify/reference/ego_netify.md).
For general network layouts, use
[`get_node_layout`](https://netify-dev.github.io/netify/reference/get_node_layout.md).

The function will issue a warning if used on non-ego networks but will
attempt to proceed by treating the first node as the ego.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
if (FALSE) { # \dontrun{
# Create an ego network
ego_net <- ego_netify(my_network, ego = "Alice")

# Get radial layout with alters grouped by attribute
layout_radial <- get_ego_layout(ego_net, 
                               layout = "radial",
                               group_by = "department")

# Get concentric layout with rings based on degree
layout_circles <- get_ego_layout(ego_net,
                                layout = "concentric", 
                                group_by = "degree_total")

# Use with plot
plot(ego_net, point_layout = layout_radial)
} # }
```
