# Create ego network from a netify object

`ego_netify` extracts an ego network from a netify object. An ego
network consists of a focal node (ego) and its immediate neighbors
(alters). For weighted networks, users can define neighborhoods using
edge weight thresholds. The function returns a netify object
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

  A netify object (class "netify") from which to extract the ego
  network.

- ego:

  Character string specifying the name of the ego for whom to create the
  ego network. Must match an actor name in the netify object.

- threshold:

  Numeric value or vector specifying the threshold for including alters
  in the ego network based on edge weights. For longitudinal networks,
  can be a vector with length equal to the number of time periods to
  apply different thresholds over time. If NULL (default), uses 0 for
  unweighted networks and the mean edge weight for weighted networks.

- ngbd_direction:

  Character string specifying which neighbors to include for directed
  networks. Options are:

  - `"out"`: Include alters that ego has outgoing ties to

  - `"in"`: Include alters that ego has incoming ties from

  - `"any"`: Include alters with any tie to/from ego (default)

- include_ego:

  Logical. If TRUE (default), the ego node is included in the ego
  network. If FALSE, only alters are included.

## Value

A netify object representing the ego network. For longitudinal networks,
returns a list of netify objects with one ego network per time period.

Each returned netify object includes additional attributes:

- `ego_netify`: TRUE (indicator that this is an ego network)

- `ego_id`: Identifier of the ego

- `threshold`: Threshold value(s) used

- `ngbd_direction`: Direction specification used

- `include_ego`: Whether ego was included

## Details

The function extracts an ego network by identifying all nodes connected
to the specified ego based on the given criteria:

**Neighborhood definition:**

- For unweighted networks: All nodes with edges to/from ego (threshold =
  0)

- For weighted networks: All nodes with edge weights exceeding the
  threshold

- Direction matters only for directed networks (controlled by
  ngbd_direction)

**Threshold behavior:**

- If not specified, defaults to 0 for unweighted networks

- If not specified for weighted networks, uses the mean edge weight

- For longitudinal networks, can vary by time period if a vector is
  provided

- Edges with weights \> threshold are included (not ≥)

**Output structure:**

The function preserves all attributes from the original netify object,
including nodal and dyadic variables, but subsets them to include only
ego and its neighbors. For longitudinal networks, ego networks may vary
in composition across time periods as relationships change.

**Limitations:**

- Currently does not support multilayer networks

- Currently does not support bipartite networks

## Note

To create ego networks for multiple egos, use `lapply` or a loop to call
this function for each ego separately.

## Author

Cassy Dorff, Shahryar Minhas

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
#> ✔ Hello, you have created a neighborhood network for ego(s) (s07), yay!
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
#>          dens miss trans
#> weight1 0.333    0     0
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
pakistan_ego <- ego_netify(netlet, ego = "Pakistan")
summary(pakistan_ego)
} # }
```
