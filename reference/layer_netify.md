# Create multilayer networks from multiple netify objects

`layer_netify` combines multiple netify objects into a single multilayer
network structure. each input network becomes a layer in the resulting
multilayer network, enabling analysis of multiple relationship types or
network views simultaneously.

## Usage

``` r
layer_netify(netlet_list, ..., layer_labels = NULL)
```

## Arguments

- netlet_list:

  a list of netify objects to layer together, or the first netify object
  when passing multiple objects as separate arguments via `...`. all
  objects must have compatible dimensions and attributes (see details).

- ...:

  additional netify objects. when provided, `netlet_list` should be a
  single netify object and all arguments are collected into a list. this
  allows calling `layer_netify(net1, net2, net3)` as a shorthand for
  `layer_netify(list(net1, net2, net3))`.

- layer_labels:

  character vector specifying names for each layer. if NULL (default),
  uses names from netlet_list or generates generic labels ("layer1",
  "layer2", etc.). length must match the number of netify objects.

## Value

a multilayer netify object with structure depending on input type:

- **cross-sectional input**: 3d array `[actors x actors x layers]`

- **longitudinal array input**: 4d array
  `[actors x actors x layers x time]`

- **longitudinal list input**: list of 3d arrays, one per time period

the returned object maintains the netify class and includes:

- combined nodal attributes (if compatible)

- combined dyadic attributes (if compatible)

- layer information accessible via `attr(obj, 'layers')`

- all standard netify attributes

## Details

**compatibility requirements:**

all netify objects in netlet_list must have identical:

- network type (cross-sectional or longitudinal)

- dimensions (same actors and time periods)

- mode (all unipartite or all bipartite)

- actor composition (same actors in same order)

**mixed directedness:**

layers are allowed to have different symmetry settings (e.g., some
symmetric and others directed). when layers have mixed directedness, the
`symmetric` attribute is stored as a named logical vector with one
element per layer.

**attribute handling:**

nodal and dyadic attributes are combined when possible:

- if attributes are identical across layers, the first layer's
  attributes are used

- if attributes differ but have compatible structure, they are merged

- if attributes are incompatible, a warning is issued and attributes
  must be added manually using
  [`add_node_vars()`](https://netify-dev.github.io/netify/reference/add_node_vars.md)
  or
  [`add_dyad_vars()`](https://netify-dev.github.io/netify/reference/add_dyad_vars.md)

## Note

memory usage increases with the number of layers. for large networks
with many layers, consider whether all layers are necessary for your
analysis.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
# load example data
data(icews)

# example 1: cross-sectional multilayer network
icews_10 <- icews[icews$year == 2010, ]

# create separate networks for different interaction types
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

# layer them together
coop_multilayer <- layer_netify(
    netlet_list = list(verbal_coop, material_coop),
    layer_labels = c("verbal", "material")
)

# check structure
dim(get_raw(coop_multilayer)) # [actors x actors x 2]
#> [1] 152 152   2
attr(coop_multilayer, "layers") # "verbal" "material"
#> [1] "verbal"   "material"

# \donttest{
# example 2: longitudinal multilayer (array format)
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

# create longitudinal multilayer
longit_multilayer <- layer_netify(
    list(verbal_longit, material_longit),
    layer_labels = c("verbal", "material")
)

dim(get_raw(longit_multilayer)) # [actors x actors x 2 x years]
#> [1] 152 152   2  13
# }
```
