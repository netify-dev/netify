# Create multilayer networks from multiple netify objects

`layer_netify` combines multiple netify objects into a single multilayer
network structure. Each input network becomes a layer in the resulting
multilayer network, enabling analysis of multiple relationship types or
network views simultaneously.

## Usage

``` r
layer_netify(netlet_list, layer_labels = NULL)
```

## Arguments

- netlet_list:

  A list of netify objects to layer together. All objects must have
  compatible dimensions and attributes (see Details).

- layer_labels:

  Character vector specifying names for each layer. If NULL (default),
  uses names from netlet_list or generates generic labels ("layer1",
  "layer2", etc.). Length must match the number of netify objects.

## Value

A multilayer netify object with structure depending on input type:

- **Cross-sectional input**: 3D array `[actors × actors × layers]`

- **Longitudinal array input**: 4D array
  `[actors × actors × layers × time]`

- **Longitudinal list input**: List of 3D arrays, one per time period

The returned object maintains the netify class and includes:

- Combined nodal attributes (if compatible)

- Combined dyadic attributes (if compatible)

- Layer information accessible via `attr(obj, 'layers')`

- All standard netify attributes

## Details

**Compatibility requirements:**

All netify objects in netlet_list must have identical:

- Network type (cross-sectional or longitudinal)

- Dimensions (same actors and time periods)

- Symmetry (all directed or all undirected)

- Mode (all unipartite or all bipartite)

- Actor composition (same actors in same order)

**Attribute handling:**

Nodal and dyadic attributes are combined when possible:

- If attributes are identical across layers, the first layer's
  attributes are used

- If attributes differ but have compatible structure, they are merged

- If attributes are incompatible, a warning is issued and attributes
  must be added manually using
  [`add_node_vars()`](https://netify-dev.github.io/netify/reference/add_node_vars.md)
  or
  [`add_dyad_vars()`](https://netify-dev.github.io/netify/reference/add_dyad_vars.md)

## Note

Memory usage increases with the number of layers. For large networks
with many layers, consider whether all layers are necessary for your
analysis.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
# Load example data
data(icews)

# Example 1: Cross-sectional multilayer network
icews_10 <- icews[icews$year == 2010, ]

# Create separate networks for different interaction types
verbal_coop <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "verbCoop",
    nodal_vars = c("i_log_gdp", "i_log_pop"),
    dyad_vars = "verbConf"
)

material_coop <- netify(
    icews_10,
    actor1 = "i", actor2 = "j",
    symmetric = FALSE,
    weight = "matlCoop",
    nodal_vars = "i_polity2",
    dyad_vars = "matlConf"
)

# Layer them together
coop_multilayer <- layer_netify(
    netlet_list = list(verbal_coop, material_coop),
    layer_labels = c("Verbal", "Material")
)

# Check structure
dim(get_raw(coop_multilayer)) # [actors × actors × 2]
#> [1] 152 152   2
attr(coop_multilayer, "layers") # "Verbal" "Material"
#> [1] "Verbal"   "Material"

# \donttest{
# Example 2: Longitudinal multilayer (array format)
verbal_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "verbCoop",
    output_format = "longit_array"
)

material_longit <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE,
    weight = "matlCoop",
    output_format = "longit_array"
)

# Create longitudinal multilayer
longit_multilayer <- layer_netify(
    list(verbal_longit, material_longit),
    layer_labels = c("Verbal", "Material")
)

dim(get_raw(longit_multilayer)) # [actors × actors × 2 × years]
#> [1] 152 152   2  13
# }
```
