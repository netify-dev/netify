# Plotting method for netify objects

Creates customizable network visualizations from netify objects using
ggplot2. Supports cross-sectional and longitudinal networks with
extensive options for mapping network attributes to visual properties.

## Usage

``` r
# S3 method for class 'netify'
plot(x, auto_format = TRUE, ...)
```

## Arguments

- x:

  A 'netify' object containing network data to visualize.

- auto_format:

  Logical. If TRUE (default), automatically adjusts plot parameters
  based on network characteristics such as size, density, and structure.
  This includes "intelligent" defaults for:

  - Node size (smaller for larger networks)

  - Edge transparency (lower for denser networks)

  - Text labels (enabled for small networks)

  - Curved edges (for small dense networks)

  - Isolate removal (for large networks)

  Set to FALSE to disable all automatic formatting. Individual
  parameters can still be overridden even when auto_format is TRUE.

- ...:

  Additional arguments controlling plot appearance:

## Value

A ggplot2 object that can be further customized with additional layers,
scales, themes, etc. For longitudinal networks, includes facets for each
time period.

If `return_components = TRUE`, returns a list of plot components that
can be manually assembled or modified.

## Details

**Naming Conventions:**

The function supports two naming styles for parameters:

- **Recommended**: Use `node_*` for node attributes and `*_by` for
  variable mappings (e.g., `node_size_by = "degree"`)

- **Legacy**: Use `point_*` for nodes and `*_var` for variables (e.g.,
  `point_size_var = "degree"`)

**Default Behaviors:**

- For weighted networks, edge transparency maps to weight by default

- For directed networks, arrows are added automatically

- For longitudinal networks, time periods are shown as facets

- Isolates are removed by default (set `remove_isolates = FALSE` to
  keep)

**Customization Tips:**

- Use `mutate_weight` to handle skewed weight distributions

- Combine fixed and variable aesthetics (e.g., fixed color with variable
  size)

- Add ggplot2 layers after the plot call for further customization

- Use `select_text` for selective labeling in dense networks

## Layout Parameters

- `layout`:

  Character string specifying the igraph layout algorithm. Options
  include: `"nicely"` (default), `"fr"` (Fruchterman-Reingold), `"kk"`
  (Kamada-Kawai), `"circle"`, `"star"`, `"grid"`, `"tree"`,
  `"bipartite"` (for bipartite networks), `"randomly"`, and others. For
  ego networks, additional options are available: `"radial"`
  (ego-centric with optional grouping) and `"concentric"` (ego at center
  with alters in rings). See
  [`get_node_layout`](https://netify-dev.github.io/netify/reference/get_node_layout.md)
  and
  [`get_ego_layout`](https://netify-dev.github.io/netify/reference/get_ego_layout.md)
  for full details.

- `point_layout`:

  Optional data.frame or list of data.frames containing pre-computed
  node positions with columns 'actor', 'x', and 'y'. Overrides `layout`
  if provided.

- `static_actor_positions`:

  Logical. For longitudinal networks, should node positions remain
  constant across time? Default is `FALSE`.

- `which_static`:

  Integer. When `static_actor_positions = TRUE`, which time period's
  layout to use as template? If `NULL` (default), creates composite
  layout from all time periods.

- `seed`:

  Integer for reproducible layouts. Default is 6886.

## Display Control

- `add_edges`:

  Logical. Display edges? Default is `TRUE`.

- `add_points`:

  Logical. Display nodes as points? Default is `TRUE`.

- `add_text`:

  Logical. Add text labels to nodes? Default is `FALSE`.

- `add_text_repel`:

  Logical. Add text labels with automatic repositioning to avoid
  overlaps? Default is `FALSE`. When `TRUE`, overrides `add_text`. Uses
  ggrepel for positioning.

- `add_label`:

  Logical. Add boxed labels to nodes? Default is `FALSE`.

- `add_label_repel`:

  Logical. Add boxed labels with automatic repositioning to avoid
  overlaps? Default is `FALSE`. When `TRUE`, overrides `add_label`. Uses
  ggrepel for positioning.

- `remove_isolates`:

  Logical. Remove unconnected nodes? Default is `TRUE`.

- `curve_edges`:

  Logical. Use curved edges? Default is `FALSE`.

- `use_theme_netify`:

  Logical. Apply netify theme? Default is `TRUE`.

- `facet_type`:

  Character. For multilayer longitudinal networks, controls faceting
  style: `"grid"` (default) creates a 2D grid with time × layer,
  `"wrap"` creates wrapped facets with combined time-layer labels.

- `facet_ncol`:

  Integer. Number of columns for facet_wrap layouts. Only used when
  `facet_type = "wrap"` or for single-dimension faceting.

- `rescale_edge_weights`:

  Logical. For multilayer networks, should edge weights be rescaled to a
  common 0-1 range across all layers? This is useful when layers have
  very different weight scales. Default is `FALSE`.

## Subsetting Parameters

- `node_filter`:

  An expression to filter nodes. The expression can reference any nodal
  attribute. For example: `node_filter = degree_total > 5` to show only
  nodes with total degree greater than 5. The expression is evaluated in
  the context of the nodal data, so any node-level variable can be used.

- `edge_filter`:

  An expression to filter edges. The expression can reference any edge
  attribute (including 'weight' for weighted networks). For example:
  `edge_filter = weight > 0.5` to show only edges with weight greater
  than 0.5. The expression is evaluated in the context of the edge data,
  so any edge-level variable can be used.

- `time_filter`:

  For longitudinal networks, a vector of time periods to include in the
  plot. Can be numeric indices or character labels matching the time
  dimension. If NULL (default), all time periods are plotted. For
  cross-sectional networks, this parameter is ignored.

## Node Aesthetics

Fixed aesthetics (same for all nodes):

- `node_size` or `point_size`:

  Numeric. Size of all nodes.

- `node_color` or `point_color`:

  Color of node borders.

- `node_fill` or `point_fill`:

  Fill color of nodes (note that fill will only work with certain
  shapes).

- `node_shape` or `point_shape`:

  Shape of nodes (see [`?pch`](https://rdrr.io/r/graphics/points.html)).

- `node_alpha` or `point_alpha`:

  Transparency (0-1).

- `node_stroke` or `point_stroke`:

  Width of node borders.

Variable aesthetics (mapped to data):

- `node_size_by` or `point_size_var`:

  Column name for size mapping.

- `node_color_by` or `point_color_var`:

  Column name for border color.

- `node_fill_by` or `point_fill_var`:

  Column name for fill color (note that fill will only work with certain
  shapes).

- `node_shape_by` or `point_shape_var`:

  Column name for shape.

- `node_alpha_by` or `point_alpha_var`:

  Column name for transparency.

## Edge Aesthetics

Fixed aesthetics:

- `edge_color`:

  Color for all edges. Default is "black".

- `edge_linewidth`:

  Width for all edges. Default is 0.5.

- `edge_linetype`:

  Line type (1=solid, 2=dashed, etc.).

- `edge_alpha`:

  Transparency (0-1).

- `edge_curvature`:

  Curvature amount when `curve_edges = TRUE`.

- `edge_arrow`:

  Arrow specification for directed networks. Example:
  `arrow(length = unit(0.2, "cm"))`.

- `adjust_arrow_endpoints`:

  Logical. Should arrow endpoints be adjusted to stop at node
  boundaries? Default is `FALSE`. Only affects directed networks.

- `edge_arrow_gap`:

  Numeric. Additional gap between arrow tip and node boundary as a
  proportion of node radius (0-1). Default is 0.2. Only used when
  `adjust_arrow_endpoints = TRUE`.

- `edge_arrow_size_scale`:

  Numeric. Scale factor for converting node sizes to coordinate units.
  If `NULL` (default), automatically calculated based on plot range.

Variable aesthetics:

- `edge_color_by` or `edge_color_var`:

  Column name for color mapping.

- `edge_linewidth_by` or `edge_linewidth_var`:

  Column name for width.

- `edge_linetype_by` or `edge_linetype_var`:

  Column name for line type.

- `edge_alpha_by` or `edge_alpha_var`:

  Column name for transparency. For weighted networks, defaults to the
  weight variable if not specified.

## Text and Label Options

Selective labeling:

- `select_text`:

  Character vector of node names to show as text. When used, text labels
  will automatically use `geom_text_repel` to avoid overlaps.

- `select_text_display`:

  Alternative text to display (same length as `select_text`).

- `select_label`:

  Character vector of node names to show with boxes. When used, labels
  will automatically use `geom_label_repel` to avoid overlaps.

- `select_label_display`:

  Alternative labels (same length as `select_label`).

Text aesthetics:

- `text_size`:

  Fixed size for all text. Default is 3.88.

- `text_color`:

  Fixed color for all text. Default is "black".

- `text_alpha`:

  Fixed transparency for text.

- `text_size_by`:

  Variable to map to text size.

- `text_color_by`:

  Variable to map to text color.

Label (boxed text) aesthetics have similar parameters with `label_`
prefix.

Text repel parameters (when `add_text_repel = TRUE`):

- `text_repel_force`:

  Force of repulsion between overlapping text. Default is 1.

- `text_repel_max_overlaps`:

  Maximum number of overlaps to tolerate. Default is 10.

- `text_repel_box_padding`:

  Padding around text. Default is 0.25.

- `text_repel_point_padding`:

  Padding around points. Default is 0.

- `text_repel_segment_color`:

  Color of connecting segments. Default is "grey50".

Label repel parameters (when `add_label_repel = TRUE`):

- Similar to text_repel but with `label_repel_` prefix:

- `label_repel_label_padding`:

  Padding around label boxes. Default is 0.25.

- `label_repel_label_r`:

  Radius of label box corners. Default is 0.15.

## Scale Labels

Customize legend titles:

- `node_size_label` or `point_size_label`:

  Legend title for size.

- `node_color_label` or `point_color_label`:

  Legend title for color.

- `edge_alpha_label`:

  Legend title for edge transparency.

- `edge_color_label`:

  Legend title for edge color.

## Highlighting Parameters

- `highlight`:

  Character vector of node names to highlight with different colors.
  Non-highlighted nodes will be colored grey. Highlighted nodes can also
  be automatically enlarged if `highlight_size_increase` is greater than
  1.

- `highlight_color`:

  Named vector of colors for highlighted nodes. If NULL, uses default
  distinct colors (red, blue, green for up to 3 nodes, or a color
  palette for more). Names should match the values in the `highlight`
  parameter. Example:
  `c('USA' = 'blue', 'China' = 'red', 'Russia' = 'green')`.

- `highlight_label`:

  Title for the highlight legend. Default is "Highlighted".

- `highlight_size_increase`:

  Numeric factor(s) to increase size of highlighted nodes. Can be a
  single value (applied to all highlighted nodes) or a vector of length
  `length(highlight) + 1` where each value corresponds to a highlighted
  node and the last value applies to "Other" nodes. Default is 1 (no
  size increase). Example: `c(3, 1, 1, 0.5)` for 3 highlighted nodes
  where the first is 3x larger, the next two are normal size, and all
  others are half size.

- `show_other_in_legend`:

  Logical. Include "Other" category in legend? Default is FALSE. When
  FALSE, only highlighted nodes appear in the legend.

## Ego Layout Parameters

For ego networks (created with
[`ego_netify`](https://netify-dev.github.io/netify/reference/ego_netify.md)),
additional layout options control the ego-centric visualization:

- `ego_group_by`:

  Character string specifying a nodal attribute to use for grouping
  alters in ego layouts. For "radial" layout, creates sectors. For
  "concentric" layout, determines ring assignment.

- `ego_order_by`:

  Character string specifying a nodal attribute to use for ordering
  alters within groups or rings. Common options include "degree_total".

- `ego_weight_to_distance`:

  Logical. For weighted networks with "radial" layout, should edge
  weights determine distance from ego? Higher weights place alters
  closer to ego. Default is `FALSE`.

- `ego_ring_gap`:

  Numeric (0-1). Gap between concentric rings as proportion of radius.
  Only for "concentric" layout. Default is 0.3.

- `ego_size`:

  Numeric. Relative size of central area reserved for ego. Larger values
  create more space between ego and alters. Default is 0.1.

## Special Parameters

- `mutate_weight`:

  Function to transform edge weights before plotting. Example: `log1p`
  for log(x+1) transformation. Applied before mapping to aesthetics.

- `return_components`:

  Logical. Return plot components instead of assembled plot? Useful for
  manual customization. Default is `FALSE`.

- `style`:

  A style function (e.g., `style_budapest`). Applies a complete visual
  style including colors, shapes, and layout preferences.

## Examples

``` r
# Load example data
data(icews)

# Basic cross-sectional network
icews_10 <- icews[icews$year == 2010, ]
net_10 <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop"
)

# Simple plot with auto-formatting (default)
plot(net_10)


# Plot without auto-formatting for full control
plot(net_10, auto_format = FALSE)


# add nodal stats to netlet
net_10 <- add_node_vars(
    net_10,
    summary_actor(net_10),
    "actor"
)

# Customized plot with new naming convention
plot(net_10,
    edge_color = "lightgrey",
    node_size_by = "degree_total", # Instead of point_size_var
    node_color = "steelblue",
    edge_alpha_by = "verbCoop", # Instead of edge_alpha_var
    node_size_label = "Degree",
    edge_alpha_label = "Verbal Cooperation"
)


# Longitudinal network example
net_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j",
    time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    nodal_vars = c("i_polity2", "i_log_gdp")
)

# Add network statistics
net_longit <- add_node_vars(
    net_longit,
    summary_actor(net_longit),
    actor = "actor",
    time = "time"
)

# Plot with multiple aesthetics
plot(net_longit,
    # Edges
    edge_color = "grey70",
    mutate_weight = log1p, # Transform weights
    # Nodes
    node_size_by = "degree_total",
    node_color_by = "i_polity2",
    # Labels
    node_size_label = "Total Degree",
    node_color_label = "Polity Score",
    edge_alpha_label = "Log(Verbal Coop.)",
    # Layout
    static_actor_positions = TRUE # Keep positions constant
)


# Selective labeling example
plot(net_10,
    node_size_by = "degree_total",
    select_text = c("United States", "China", "Russian Federation"),
    text_size = 3,
    text_color = "darkred"
)
#> Warning: Removed 149 rows containing missing values or values outside the scale range
#> (`geom_text_repel()`).


# choose alternative labels for selected text
plot(net_10,
    node_size_by = "degree_total",
    select_text = c("United States", "China", "Russian Federation"),
    select_text_display = c("USA", "CHN", "RUS"),
    text_size = 3,
    text_color = "darkred"
)
#> Warning: Removed 149 rows containing missing values or values outside the scale range
#> (`geom_text_repel()`).



# Time subsetting example
plot(net_longit,
    time_filter = c("2010", "2011", "2012")
)


# Node subsetting example
# democracies with high GDP
plot(net_longit, node_filter = ~ i_polity2 > 6 & i_log_gdp > 25)


# use return_components=TRUE
# to get back ggplot2 pieces of plot
g10 <- plot(
    net_10,
    node_alpha = .8,
    arrow = ggplot2::arrow(length = ggplot2::unit(0.01, "inches")),
    node_size_by = "degree_total",
    node_size_label = "Log(Degree)",
    edge_alpha_label = "Log(Verbal Coop.)",
    remove_isolates = TRUE,
    mutate_weight = log1p,
    return_components = TRUE
)

# Manually assemble with custom modifications
# to scale aesthetics such as edges
g10$base +
    netify_edge(g10) +
    ggplot2::scale_alpha_continuous(range = c(0.01, 0.2)) +
    netify_node(g10) +
    theme_netify()

```
