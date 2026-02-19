# Prepare netify data for network visualization

`net_plot_data` processes a netify object and generates all necessary
components for network visualization. This function handles layout
computation, aesthetic parameter organization, and data structuring for
subsequent plotting with ggplot2 or other visualization tools.

## Usage

``` r
net_plot_data(netlet, plot_args = list())
```

## Arguments

- netlet:

  A netify object (class "netify") containing the network to be
  visualized. Must be a single-layer network (multilayer networks not
  currently supported).

- plot_args:

  A list of plotting arguments controlling visualization appearance and
  behavior. Can include:

  **Layout parameters:**

  - `point_layout`: Pre-computed node positions as a data.frame or list
    of data.frames (for longitudinal networks). If provided, overrides
    layout algorithm selection

  - `layout`: Character string specifying the igraph layout algorithm.
    Options: "nicely" (default), "fr" (Fruchterman-Reingold), "kk"
    (Kamada-Kawai), "circle", "star", "grid", "tree", "bipartite", and
    others. See
    [`get_node_layout`](https://netify-dev.github.io/netify/reference/get_node_layout.md)
    for full list

  - `static_actor_positions`: Logical. If TRUE, maintains consistent
    node positions across time periods in longitudinal networks

  - `which_static`: Integer specifying which time period to use as the
    template for static positions

  - `seed`: Integer for reproducible random layouts

  **Display options:**

  - `remove_isolates`: Logical. Remove unconnected nodes (default: TRUE)

  - `add_edges`: Logical. Include edges in visualization (default: TRUE)

  - `curve_edges`: Logical. Use curved edges instead of straight
    (default: FALSE)

  - `add_points`: Logical. Display nodes as points (default: TRUE)

  - `add_text`: Logical. Add text labels to nodes (default: FALSE)

  - `add_label`: Logical. Add boxed labels to nodes (default: FALSE)

  **Selective labeling:**

  - `select_text`: Character vector of node names to label with text

  - `select_label`: Character vector of node names to label with boxes

  Additional aesthetic parameters are processed by `adjust_plot_args`
  and `gg_params`.

## Value

A list with three components for creating network visualizations:

- **plot_args**: Processed plotting arguments with defaults applied and
  parameters validated. Includes all layout and display settings

- **ggnet_params**: Organized aesthetic parameters for ggplot2 mapping.
  Contains separate specifications for nodes, edges, text, and labels
  with both static and dynamic (data-mapped) aesthetics

- **net_dfs**: Data frames ready for plotting:

  - `nodal_data`: Node information including positions (x, y),
    attributes, and any additional variables

  - `edge_data`: Edge information including endpoint coordinates (x1,
    y1, x2, y2) and edge attributes

## Details

This function serves as the data preparation layer for netify
visualization, performing several operations:

**Data validation:**

- Ensures the input is a valid netify object

- Checks for single-layer networks (multilayer not supported)

- Validates ego networks contain only one ego

**Layout computation:**

- Generates node positions using specified algorithm if not provided

- Calculates edge endpoint coordinates based on node positions

- Handles both cross-sectional and longitudinal layouts

**Data organization:**

- Merges layout information with network attributes

- Processes plotting arguments and applies defaults

- Organizes aesthetic parameters for ggplot2 compatibility

- Removes isolates if requested

**Output structure:**

The returned data is structured for direct use with ggplot2 or can be
further customized. The separation of layout, aesthetics, and data
allows for flexible visualization workflows.

## Note

This function is primarily designed for use with netify's plot method
but can be called directly for custom visualization workflows.

For multilayer networks, use
[`subset_netify`](https://netify-dev.github.io/netify/reference/subset_netify.md)
to extract individual layers before visualization.

For ego networks with multiple egos, create separate visualizations and
combine them using packages like patchwork.

## Author

Cassy Dorff, Shahryar Minhas
