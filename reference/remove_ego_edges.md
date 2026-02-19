# Remove ego-alter edges from ego network

This function removes all edges between the ego and alters in an ego
network, leaving only the alter-alter connections visible. This is
useful for visualizing the structure among alters without the clutter of
ego connections.

## Usage

``` r
remove_ego_edges(netlet)
```

## Arguments

- netlet:

  An ego network created with ego_netify()

## Value

A modified netify object with ego edges removed

## Examples

``` r
if (FALSE) { # \dontrun{
ego_net <- ego_netify(my_network, ego = "Alice")
alter_only_net <- remove_ego_edges(ego_net)
plot(alter_only_net)
} # }
```
