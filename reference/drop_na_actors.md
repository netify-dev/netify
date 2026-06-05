# Drop actors with NA covariates from a netify object

removes actors whose `nodal_data` carries `na` in one or more covariate
columns. ergm terms like `nodecov()` and `nodematch()` reject na-bearing
vertex attributes, so this helper is handy upstream of
[`netify_to_statnet`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md).
works for cross-sectional, longitudinal, and bipartite netlets.

## Usage

``` r
drop_na_actors(netlet, cols = NULL)
```

## Arguments

- netlet:

  a netify object with a `nodal_data` attribute.

- cols:

  character vector of column names in `nodal_data` to check for `na`.
  `NULL` (the default) checks every non-bookkeeping column (everything
  except `actor`, `time`, and `layer`).

## Value

a netify object equivalent to
`subset_netify(netlet, actors = clean_actors)` after dropping any actor
whose nodal rows contain `na` in the inspected columns. if no nas are
found the input is returned unchanged.

## Details

for longitudinal netlets an actor is dropped from every period if any of
its rows in `nodal_data` carry `na` in the inspected columns. for
bipartite netlets only actors in the mode that the nodal table covers
are filtered; the other mode passes through untouched.

corner cases:

- if `cols` references a name that is not in `nodal_data`, the call
  aborts with a clear message listing the missing columns.

- if no actor carries `na` in the inspected columns, the input netlet is
  returned unchanged (no inform).

- if *every* actor carries `na` (so the cleaned netlet would have zero
  actors), the call aborts rather than silently returning an empty
  netify, which would break downstream
  [`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
  / `ergm()` pipelines.

- if the netlet has no `nodal_data` attribute attached, the input is
  returned unchanged.

## Author

shahryar minhas

## Examples

``` r
# \donttest{
data(icews)
net <- netify(
    icews,
    actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "verbCoop",
    nodal_vars = c("i_polity2", "i_log_gdp", "i_log_pop")
)
clean <- drop_na_actors(net, cols = c("i_polity2", "i_log_gdp"))
#> ℹ Dropped 8 of 152 actors with NA covariates: "Afghanistan", "Bosnia And
#>   Herzegovina", "Djibouti", "Iraq", "Korea, Democratic People's Republic Of",
#>   "Lebanon", "Solomon Islands", and "Somalia".
#> This message is displayed once per session.
# }
```
