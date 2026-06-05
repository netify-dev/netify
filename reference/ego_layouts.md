# Create ego-centric layouts for ego networks

these functions create specialized layouts for ego networks that place
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

  a netify object created with ego_netify()

- min_radius:

  minimum distance from ego to any alter

- max_radius:

  maximum distance from ego to any alter

- seed:

  random seed for reproducible layouts

- ego_name:

  name of the ego node (extracted from netlet attributes if not
  provided)

- n_rings:

  number of concentric rings for radial layout

- buffer_radius:

  minimum distance from ego for ego_centric layout

- transition_zone:

  smooth transition zone width for ego_centric layout

## Value

a list of data frames with x,y coordinates for each time period

## Details

three layout algorithms are provided:

**hierarchical layout**: places ego at center and arranges alters in
concentric circles based on their network centrality. more central
alters are placed closer to the ego.

**radial layout**: places ego at center and arranges alters in
concentric rings based on connection strength quartiles. stronger
connections are placed in inner rings.

**ego centric layout**: uses force-directed layout but ensures ego
remains at center with a buffer zone. preserves natural clustering while
maintaining ego visibility.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
if (FALSE) { # \dontrun{
# create ego network
ego_net <- ego_netify(my_network, ego = "pakistan")

# hierarchical layout
layout <- create_hierarchical_ego_layout(ego_net)
plot(ego_net, point_layout = layout)

# radial layout with custom rings
layout <- create_radial_ego_layout(ego_net, n_rings = 5)
plot(ego_net, point_layout = layout)
} # }
```
