# Create ego network from a netify object

`ego_netify` extracts an ego network from a netify object. an ego
network consists of a focal node (ego) and its immediate neighbors
(alters). for weighted networks, users can define neighborhoods using
edge weight thresholds. the function returns a netify object
representing the ego network.

## Usage

``` r
ego_netify(
  netlet,
  ego,
  threshold = NULL,
  ngbd_direction = "any",
  include_ego = TRUE
)
```

## Arguments

- netlet:

  a netify object (class "netify") from which to extract the ego
  network.

- ego:

  character string specifying the name of the ego for whom to create the
  ego network. must match an actor name in the netify object.

- threshold:

  numeric value or vector specifying the threshold for including alters
  in the ego network based on edge weights. for longitudinal networks,
  can be a vector with length equal to the number of time periods to
  apply different thresholds over time. if NULL (default), uses 0 for
  unweighted networks and the mean edge weight for weighted networks.

- ngbd_direction:

  character string specifying which neighbors to include for directed
  networks. options are:

  - `"out"`: include alters that ego has outgoing ties to

  - `"in"`: include alters that ego has incoming ties from

  - `"any"`: include alters with any tie to/from ego (default)

- include_ego:

  logical. if TRUE (default), the ego node is included in the ego
  network. if FALSE, only alters are included.

## Value

a netify object representing the ego network. for longitudinal networks,
returns a list of netify objects with one ego network per time period.

each returned netify object includes additional attributes:

- `ego_netify`: TRUE (indicator that this is an ego network)

- `ego_id`: identifier of the ego

- `threshold`: threshold value(s) used

- `ngbd_direction`: direction specification used

- `include_ego`: whether ego was included

## Details

the function extracts an ego network by identifying all nodes connected
to the specified ego based on the given criteria:

**neighborhood definition:**

- for unweighted networks: all nodes with edges to/from ego (threshold =
  0)

- for weighted networks: all nodes with edge weights exceeding the
  threshold

- direction matters only for directed networks (controlled by
  ngbd_direction)

**threshold behavior:**

- if not specified, defaults to 0 for unweighted networks

- if not specified for weighted networks, uses the mean edge weight

- for longitudinal networks, can vary by time period if a vector is
  provided

- edges with weights \> threshold are included (not \>=)

**output structure:**

the function preserves all attributes from the original netify object,
including nodal and dyadic variables, but subsets them to include only
ego and its neighbors. for longitudinal networks, ego networks may vary
in composition across time periods as relationships change.

**limitations:**

- currently does not support multilayer networks

- currently does not support bipartite networks

## Note

to create ego networks for multiple egos, use `lapply` or a loop to call
this function for each ego separately.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
# cross-sectional ego network from the bundled classroom data
data(classroom_edges)
data(classroom_nodes)
net <- netify(
    classroom_edges,
    actor1 = "from", actor2 = "to",
    symmetric = TRUE,
    nodal_data = classroom_nodes
)
s07_ego <- ego_netify(net, ego = "s07")
print(s07_ego)
#> ✔ Neighborhood network created for ego(s) (s07).
#> • Type: Ego Network
#> • Ego: s07
#> • Direction: Any ties (in or out)
#> • Ego included: Yes
#> • Unipartite
#> • Symmetric
#> • Binary Weights
#> • Cross-Sectional
#> • # Unique Egos: 1 | # Unique Alters: 5
#> Neighborhood Network Summary Statistics:
#>      dens miss trans
#> s07 0.333    0     0
#> • Nodal Features: gender, grade, gpa
#> • Dyad Features: None

if (FALSE) { # \dontrun{
# longitudinal ego network with a weighted, directed netlet
data(icews)
netlet <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    weight = "verbCoop"
)
pakistan_ego <- ego_netify(netlet, ego = "pakistan")
summary(pakistan_ego)
} # }
```
