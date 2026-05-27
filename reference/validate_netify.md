# Deep coherence check on a netify object

`netify_check()` validates only class membership. `validate_netify()`
goes further: it verifies that the netify's internal pieces still agree
with each other after any user-side surgery (e.g., manually edited
`attr(., "nodal_data")`,
[`subset()`](https://rdrr.io/r/base/subset.html) followed by an
attribute overwrite, etc.). Use this when you've hand-modified a netlet
and want to confirm it's still well-formed before passing to
[`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
/
[`to_amen()`](https://netify-dev.github.io/netify/reference/netify_to_amen.md)
/ [`plot()`](https://rdrr.io/r/graphics/plot.default.html).

## Usage

``` r
validate_netify(netlet, verbose = TRUE)
```

## Arguments

- netlet:

  A netify object.

- verbose:

  Logical. If `TRUE` (default), print a per-check status banner;
  otherwise return silently.

## Value

Invisibly returns a list with one logical per check (`TRUE` = passed).
The function
[`cli::cli_abort()`](https://cli.r-lib.org/reference/cli_abort.html)s on
any failure unless `verbose = TRUE`, in which case failures are reported
per-check and the function returns invisibly with the full failure list.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
if (FALSE) { # \dontrun{
data(icews)
net <- netify(icews[icews$year == 2010, ],
actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "verbCoop")
validate_netify(net)
} # }
```
