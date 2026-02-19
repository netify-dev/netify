# Create ego-centric layouts for ego networks

These functions create specialized layouts for ego networks that place
the ego at the center and arrange alters in meaningful ways around it.

## Usage

``` r
create_hierarchical_ego_layout(
  netlet,
  min_radius = 2,
  max_radius = 5,
  seed = 123
)

create_radial_ego_layout(
  netlet,
  ego_name = NULL,
  n_rings = 4,
  min_radius = 1.5,
  max_radius = 5,
  seed = 123
)

create_ego_centric_layout(
  netlet,
  buffer_radius = 1.5,
  transition_zone = 0.5,
  seed = 123
)
```

## Arguments

- netlet:

  A netify object created with ego_netify()

- min_radius:

  Minimum distance from ego to any alter

- max_radius:

  Maximum distance from ego to any alter

- seed:

  Random seed for reproducible layouts

- ego_name:

  Name of the ego node (extracted from netlet attributes if not
  provided)

- n_rings:

  Number of concentric rings for radial layout

- buffer_radius:

  Minimum distance from ego for ego_centric layout

- transition_zone:

  Smooth transition zone width for ego_centric layout

## Value

A list of data frames with x,y coordinates for each time period

## Details

Three layout algorithms are provided:

**Hierarchical Layout**: Places ego at center and arranges alters in
concentric circles based on their network centrality. More central
alters are placed closer to the ego.

**Radial Layout**: Places ego at center and arranges alters in
concentric rings based on connection strength quartiles. Stronger
connections are placed in inner rings.

**Ego Centric Layout**: Uses force-directed layout but ensures ego
remains at center with a buffer zone. Preserves natural clustering while
maintaining ego visibility.

## Examples

``` r
if (FALSE) { # \dontrun{
# Create ego network
ego_net <- ego_netify(my_network, ego = "Pakistan")

# Hierarchical layout
layout <- create_hierarchical_ego_layout(ego_net)
plot(ego_net, point_layout = layout)

# Radial layout with custom rings
layout <- create_radial_ego_layout(ego_net, n_rings = 5)
plot(ego_net, point_layout = layout)
} # }
```
