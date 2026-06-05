# Plotting method for netify objects

creates customizable network visualizations from netify objects using
ggplot2. supports cross-sectional and longitudinal networks with
extensive options for mapping network attributes to visual properties.

## Usage

``` r
# S3 method for class 'netify'
plot(x, auto_format = TRUE, ...)
```

## Arguments

- x:

  a 'netify' object containing network data to visualize.

- auto_format:

  logical. if TRUE (default), automatically adjusts plot parameters
  based on network characteristics such as size, density, and structure.
  this includes "intelligent" defaults for:

  - node size (smaller for larger networks)

  - edge transparency (lower for denser networks)

  - text labels (enabled for small networks)

  - curved edges (for small dense networks)

  - isolate removal (for large networks)

  set to FALSE to disable all automatic formatting. individual
  parameters can still be overridden even when auto_format is TRUE.

- ...:

  additional arguments controlling plot appearance:

## Value

a ggplot2 object that can be further customized with additional layers,
scales, themes, etc. for longitudinal networks, includes facets for each
time period.

if `return_components = TRUE`, returns a list of plot components that
can be manually assembled or modified.

## Details

**naming conventions:**

the function supports two naming styles for parameters:

- **recommended**: use `node_*` for node attributes and `*_by` for
  variable mappings (e.g., `node_size_by = "degree"`)

- **legacy**: use `point_*` for nodes and `*_var` for variables (e.g.,
  `point_size_var = "degree"`)

**default behaviors:**

- for weighted networks, edge transparency maps to weight by default

- for directed networks, arrows are added automatically

- for longitudinal networks, time periods are shown as facets

- isolates are removed by default (set `remove_isolates = FALSE` to
  keep)

**customization tips:**

- use `mutate_weight` to handle skewed weight distributions

- combine fixed and variable aesthetics (e.g., fixed color with variable
  size)

- add ggplot2 layers after the plot call for further customization

- use `select_text` for selective labeling in dense networks

## layout parameters

- `layout`:

  character string specifying the igraph layout algorithm. options
  include: `"nicely"` (default), `"fr"` (fruchterman-reingold), `"kk"`
  (kamada-kawai), `"circle"`, `"star"`, `"grid"`, `"tree"`,
  `"bipartite"` (for bipartite networks), `"randomly"`, and others. for
  ego networks, additional options are available: `"radial"`
  (ego-centric with optional grouping) and `"concentric"` (ego at center
  with alters in rings). see
  [`get_node_layout`](https://netify-dev.github.io/netify/reference/get_node_layout.md)
  and
  [`get_ego_layout`](https://netify-dev.github.io/netify/reference/get_ego_layout.md)
  for full details.

- `point_layout`:

  optional data.frame or list of data.frames containing pre-computed
  node positions with columns 'actor', 'x', and 'y'. overrides `layout`
  if provided.

- `static_actor_positions`:

  logical. for longitudinal networks, should node positions remain
  constant across time? default is `FALSE`.

- `which_static`:

  integer. when `static_actor_positions = TRUE`, which time period's
  layout to use as template? if `NULL` (default), creates composite
  layout from all time periods.

- `seed`:

  integer for reproducible layouts. default is 6886.

## display control

- `add_edges`:

  logical. display edges? default is `TRUE`.

- `add_points`:

  logical. display nodes as points? default is `TRUE`.

- `add_text`:

  logical. add text labels to nodes? default is `FALSE`.

- `add_text_repel`:

  logical. add text labels with automatic repositioning to avoid
  overlaps? default is `FALSE`. when `TRUE`, overrides `add_text`. uses
  ggrepel for positioning.

- `add_label`:

  logical. add boxed labels to nodes? default is `FALSE`.

- `add_label_repel`:

  logical. add boxed labels with automatic repositioning to avoid
  overlaps? default is `FALSE`. when `TRUE`, overrides `add_label`. uses
  ggrepel for positioning.

- `remove_isolates`:

  logical. remove unconnected nodes? default is `TRUE`.

- `curve_edges`:

  logical. use curved edges? default is `FALSE`.

- `use_theme_netify`:

  logical. apply netify theme? default is `TRUE`.

- `facet_type`:

  character. for multilayer longitudinal networks, controls faceting
  style: `"grid"` (default) creates a 2d grid with time x layer,
  `"wrap"` creates wrapped facets with combined time-layer labels.

- `facet_ncol`:

  integer. number of columns for facet_wrap layouts. only used when
  `facet_type = "wrap"` or for single-dimension faceting.

- `rescale_edge_weights`:

  logical. for multilayer networks, should edge weights be rescaled to a
  common 0-1 range across all layers? this is useful when layers have
  very different weight scales. default is `FALSE`.

## subsetting parameters

- `node_filter`:

  a formula, quoted expression, or function to filter nodes. the
  expression can reference any nodal attribute. for example:
  `node_filter = ~ degree_total > 5` shows only nodes with total degree
  greater than 5.

- `edge_filter`:

  a formula, quoted expression, or function to filter edges. the
  expression can reference any edge attribute, including 'weight' for
  weighted networks. for example: `edge_filter = ~ weight > 0.5` shows
  only edges with weight greater than 0.5.

- `time_filter`:

  for longitudinal networks, a vector of time periods to include in the
  plot. can be numeric indices or character labels matching the time
  dimension. if NULL (default), all time periods are plotted. for
  cross-sectional networks, this parameter is ignored.

## node aesthetics

fixed aesthetics (same for all nodes):

- `node_size` or `point_size`:

  numeric. size of all nodes.

- `node_color` or `point_color`:

  color of node borders.

- `node_fill` or `point_fill`:

  fill color of nodes (note that fill will only work with certain
  shapes).

- `node_shape` or `point_shape`:

  shape of nodes (see [`?pch`](https://rdrr.io/r/graphics/points.html)).

- `node_alpha` or `point_alpha`:

  transparency (0-1).

- `node_stroke` or `point_stroke`:

  width of node borders.

variable aesthetics (mapped to data):

- `node_size_by` or `point_size_var`:

  column name for size mapping.

- `node_color_by` or `point_color_var`:

  column name for border color.

- `node_fill_by` or `point_fill_var`:

  column name for fill color (note that fill will only work with certain
  shapes).

- `node_shape_by` or `point_shape_var`:

  column name for shape.

- `node_alpha_by` or `point_alpha_var`:

  column name for transparency.

## edge aesthetics

fixed aesthetics:

- `edge_color`:

  color for all edges. default is "black".

- `edge_linewidth`:

  width for all edges. default is 0.5.

- `edge_linetype`:

  line type (1=solid, 2=dashed, etc.).

- `edge_alpha`:

  transparency (0-1).

- `edge_curvature`:

  curvature amount when `curve_edges = TRUE`.

- `edge_arrow`:

  arrow specification for directed networks. example:
  `arrow(length = unit(0.2, "cm"))`.

variable aesthetics:

- `edge_color_by` or `edge_color_var`:

  column name for color mapping.

- `edge_linewidth_by` or `edge_linewidth_var`:

  column name for width.

- `edge_linetype_by` or `edge_linetype_var`:

  column name for line type.

- `edge_alpha_by` or `edge_alpha_var`:

  column name for transparency. for weighted networks, defaults to the
  weight variable if not specified.

## text and label options

selective labeling:

- `select_text`:

  character vector of node names to show as text. when used, text labels
  will automatically use `geom_text_repel` to avoid overlaps.

- `select_text_display`:

  alternative text to display (same length as `select_text`).

- `select_label`:

  character vector of node names to show with boxes. when used, labels
  will automatically use `geom_label_repel` to avoid overlaps.

- `select_label_display`:

  alternative labels (same length as `select_label`).

text aesthetics:

- `text_size`:

  fixed size for all text. default is 3.88.

- `text_color`:

  fixed color for all text. default is "black".

- `text_alpha`:

  fixed transparency for text.

- `text_size_by`:

  variable to map to text size.

- `text_color_by`:

  variable to map to text color.

label (boxed text) aesthetics have similar parameters with `label_`
prefix.

text repel parameters (when `add_text_repel = TRUE`):

- `text_repel_force`:

  force of repulsion between overlapping text. default is 1.

- `text_repel_max_overlaps`:

  maximum number of overlaps to tolerate. default is 10.

- `text_repel_box_padding`:

  padding around text. default is 0.25.

- `text_repel_point_padding`:

  padding around points. default is 0.

- `text_repel_segment_color`:

  color of connecting segments. default is "grey50".

label repel parameters (when `add_label_repel = TRUE`):

- similar to text_repel but with `label_repel_` prefix:

- `label_repel_label_padding`:

  padding around label boxes. default is 0.25.

- `label_repel_label_r`:

  radius of label box corners. default is 0.15.

## scale labels

customize legend titles:

- `node_size_label` or `point_size_label`:

  legend title for size.

- `node_color_label` or `point_color_label`:

  legend title for color.

- `edge_alpha_label`:

  legend title for edge transparency.

- `edge_color_label`:

  legend title for edge color.

## highlighting parameters

- `highlight`:

  character vector of node names to highlight with different colors.
  non-highlighted nodes will be colored grey. highlighted nodes can also
  be automatically enlarged if `highlight_size_increase` is greater than
  1.

- `highlight_color`:

  named vector of colors for highlighted nodes. if NULL, uses default
  distinct colors (red, blue, green for up to 3 nodes, or a color
  palette for more). names should match the values in the `highlight`
  parameter. example:
  `c('usa' = 'blue', 'china' = 'red', 'russia' = 'green')`.

- `highlight_label`:

  title for the highlight legend. default is "highlighted".

- `highlight_size_increase`:

  numeric factor(s) to increase size of highlighted nodes. can be a
  single value (applied to all highlighted nodes) or a vector of length
  `length(highlight) + 1` where each value corresponds to a highlighted
  node and the last value applies to "other" nodes. default is 1 (no
  size increase). example: `c(3, 1, 1, 0.5)` for 3 highlighted nodes
  where the first is 3x larger, the next two are normal size, and all
  others are half size.

- `show_other_in_legend`:

  logical. include "other" category in legend? default is FALSE. when
  FALSE, only highlighted nodes appear in the legend.

## ego layout parameters

for ego networks (created with
[`ego_netify`](https://netify-dev.github.io/netify/reference/ego_netify.md)),
additional layout options control the ego-centric visualization:

- `ego_group_by`:

  character string specifying a nodal attribute to use for grouping
  alters in ego layouts. for "radial" layout, creates sectors. for
  "concentric" layout, determines ring assignment.

- `ego_order_by`:

  character string specifying a nodal attribute to use for ordering
  alters within groups or rings. common options include "degree_total".

- `ego_weight_to_distance`:

  logical. for weighted networks with "radial" layout, should edge
  weights determine distance from ego? higher weights place alters
  closer to ego. default is `FALSE`.

- `ego_ring_gap`:

  numeric (0-1). gap between concentric rings as proportion of radius.
  only for "concentric" layout. default is 0.3.

- `ego_size`:

  numeric. relative size of central area reserved for ego. larger values
  create more space between ego and alters. default is 0.1.

## special parameters

- `mutate_weight`:

  function to transform edge weights before plotting. example: `log1p`
  for log(x+1) transformation. applied before mapping to aesthetics.

- `return_components`:

  logical. return plot components instead of assembled plot? useful for
  manual customization. default is `FALSE`.

- `style`:

  either a style function (e.g., `style_rose`) that applies a complete
  visual style including colors, shapes, and layout preferences, or the
  string `"heatmap"` to render the adjacency matrix as a tile plot
  instead of a node-link diagram. when `style = "heatmap"` and edge
  weights cross zero (signed network), the fill scale automatically uses
  a diverging palette centred at zero; otherwise a sequential viridis
  ramp is used. optional `low`, `mid`, and `high` arguments override the
  diverging palette endpoints.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# basic cross-sectional network
icews_10 <- icews[icews$year == 2010, ]
net_10 <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# simple plot with auto-formatting (default)
plot(net_10)


# plot without auto-formatting for full control
plot(net_10, auto_format = FALSE)


# add nodal stats to netlet
net_10 <- add_node_vars(
    net_10,
    summary_actor(net_10),
    actor = "actor"
)

# customized plot with new naming convention
plot(net_10,
    edge_color = "lightgrey",
    node_size_by = "degree_total", # maps degree to node size
    node_color = "steelblue",
    edge_alpha_by = "verbCoop", # maps edge weight to alpha
    node_size_label = "degree",
    edge_alpha_label = "verbal cooperation"
)


# \donttest{
# longitudinal network example
net_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    nodal_vars = c("i_polity2", "i_log_gdp")
)

# add network statistics
net_longit <- add_node_vars(
    net_longit,
    summary_actor(net_longit),
    actor = "actor",
    time = "time"
)

# plot with multiple aesthetics
plot(net_longit,
    # edges
    edge_color = "grey70",
    mutate_weight = log1p, # transform weights
    # nodes
    node_size_by = "degree_total",
    node_color_by = "i_polity2",
    # labels
    node_size_label = "total degree",
    node_color_label = "polity score",
    edge_alpha_label = "log(verbal coop.)",
    # layout
    static_actor_positions = TRUE # keep positions constant
)


# selective labeling example
plot(net_10,
    node_size_by = "degree_total",
    select_text = c("united states", "china", "russian federation"),
    text_size = 3,
    text_color = "darkred"
)


# choose alternative labels for selected text
plot(net_10,
    node_size_by = "degree_total",
    select_text = c("united states", "china", "russian federation"),
    select_text_display = c("usa", "chn", "rus"),
    text_size = 3,
    text_color = "darkred"
)


# time subsetting example
plot(net_longit,
    time_filter = c("2010", "2011", "2012")
)


# node subsetting example
# democracies with high gdp
plot(net_longit, node_filter = ~ i_polity2 > 6 & i_log_gdp > 25)


# use return_components=TRUE
# to get back ggplot2 pieces of plot
g10 <- plot(
    net_10,
    node_alpha = .8,
    arrow = ggplot2::arrow(length = ggplot2::unit(0.01, "inches")),
    node_size_by = "degree_total",
    node_size_label = "log(degree)",
    edge_alpha_label = "log(verbal coop.)",
    remove_isolates = TRUE,
    mutate_weight = log1p,
    return_components = TRUE
)

# manually assemble with custom modifications
# to scale aesthetics such as edges
g10$base +
    netify_edge(g10) +
    ggplot2::scale_alpha_continuous(range = c(0.01, 0.2)) +
    netify_node(g10) +
    theme_netify()

# }
```
