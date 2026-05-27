# Drop actors with NA covariates from a netify object

Removes actors whose `nodal_data` carries `NA` in one or more covariate
columns. ERGM terms like `nodecov()` and `nodematch()` reject NA-bearing
vertex attributes, so this helper is handy upstream of
[`netify_to_statnet`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md).
Works for cross-sectional, longitudinal, and bipartite netlets.

## Usage

``` r
drop_na_actors(netlet, cols = NULL)
```

## Arguments

- netlet:

  A netify object with a `nodal_data` attribute.

- cols:

  Character vector of column names in `nodal_data` to check for `NA`.
  `NULL` (the default) checks every non-bookkeeping column (everything
  except `actor`, `time`, and `layer`).

## Value

A netify object equivalent to
`subset_netify(netlet, actors = clean_actors)` after dropping any actor
whose nodal rows contain `NA` in the inspected columns. If no NAs are
found the input is returned unchanged.

## Details

For longitudinal netlets an actor is dropped from every period if any of
its rows in `nodal_data` carry `NA` in the inspected columns. For
bipartite netlets only actors in the mode that the nodal table covers
are filtered; the other mode passes through untouched.

Corner cases:

- If `cols` references a name that is not in `nodal_data`, the call
  aborts with a clear message listing the missing columns.

- If no actor carries `NA` in the inspected columns, the input netlet is
  returned unchanged (no inform).

- If *every* actor carries `NA` (so the cleaned netlet would have zero
  actors), the call aborts rather than silently returning an empty
  netify, which would break downstream
  [`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
  / `ergm()` pipelines.

- If the netlet has no `nodal_data` attribute attached, the input is
  returned unchanged.

## Author

Shahryar Minhas

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
