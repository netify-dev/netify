# Prepare netify data for network visualization

`net_plot_data` processes a netify object and generates all necessary
components for network visualization. this function handles layout
computation, aesthetic parameter organization, and data structuring for
subsequent plotting with ggplot2 or other visualization tools.

## Usage

``` r
net_plot_data(netlet, plot_args = list())
```

## Arguments

- netlet:

  a netify object (class "netify") containing the network to be
  visualized. cross-sectional, longitudinal, and multilayer netify
  objects are supported.

- plot_args:

  a list of plotting arguments controlling visualization appearance and
  behavior. can include:

  **layout parameters:**

  - `point_layout`: pre-computed node positions as a data.frame or list
    of data.frames (for longitudinal networks). if provided, overrides
    layout algorithm selection

  - `layout`: character string specifying the igraph layout algorithm.
    options: "nicely" (default), "fr" (fruchterman-reingold), "kk"
    (kamada-kawai), "circle", "star", "grid", "tree", "bipartite", and
    others. see
    [`get_node_layout`](https://netify-dev.github.io/netify/reference/get_node_layout.md)
    for full list

  - `static_actor_positions`: logical. if TRUE, maintains consistent
    node positions across time periods in longitudinal networks

  - `which_static`: integer specifying which time period to use as the
    template for static positions

  - `seed`: integer for reproducible random layouts

  **display options:**

  - `remove_isolates`: logical. remove unconnected nodes (default: TRUE)

  - `add_edges`: logical. include edges in visualization (default: TRUE)

  - `curve_edges`: logical. use curved edges instead of straight
    (default: FALSE)

  - `add_points`: logical. display nodes as points (default: TRUE)

  - `add_text`: logical. add text labels to nodes (default: FALSE)

  - `add_label`: logical. add boxed labels to nodes (default: FALSE)

  **selective labeling:**

  - `select_text`: character vector of node names to label with text

  - `select_label`: character vector of node names to label with boxes

  additional aesthetic parameters are processed by `adjust_plot_args`
  and `gg_params`.

## Value

a list with three components for creating network visualizations:

- **plot_args**: processed plotting arguments with defaults applied and
  parameters validated. includes all layout and display settings

- **ggnet_params**: organized aesthetic parameters for ggplot2 mapping.
  contains separate specifications for nodes, edges, text, and labels
  with both static and dynamic (data-mapped) aesthetics

- **net_dfs**: data frames ready for plotting:

  - `nodal_data`: node information including positions (x, y),
    attributes, and any additional variables

  - `edge_data`: edge information including endpoint coordinates (x1,
    y1, x2, y2) and edge attributes

## Details

this function serves as the data preparation layer for netify
visualization, performing several operations:

**data validation:**

- ensures the input is a valid netify object

- handles single-layer and multilayer networks

- validates ego networks contain only one ego

**layout computation:**

- generates node positions using specified algorithm if not provided

- calculates edge endpoint coordinates based on node positions

- handles both cross-sectional and longitudinal layouts

**data organization:**

- merges layout information with network attributes

- processes plotting arguments and applies defaults

- organizes aesthetic parameters for ggplot2 compatibility

- removes isolates if requested

**output structure:**

the returned data is structured for direct use with ggplot2 or can be
further customized. the separation of layout, aesthetics, and data
allows for flexible visualization workflows.

## Note

this function is primarily designed for use with netify's plot method
but can be called directly for custom visualization workflows.

for multilayer networks, the returned node and edge data include a
`layer` column.

for ego networks with multiple egos, create separate visualizations and
combine them using packages like patchwork.

## Author

cassy dorff, shahryar minhas
