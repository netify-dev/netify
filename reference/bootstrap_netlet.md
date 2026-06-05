# Bootstrap any user-supplied function of a netify object

resamples actors with replacement (snijders & borgatti 1999 vertex
bootstrap) per panel, rebuilds the netlet on each draw, applies the
user-supplied `fn`, and returns the per-draw values plus percentile
confidence intervals.

## Usage

``` r
bootstrap_netlet(
  netlet,
  fn,
  n_boot = 200L,
  alpha = 0.05,
  seed = NULL,
  verbose = TRUE
)
```

## Arguments

- netlet:

  a `netify` object.

- fn:

  function. takes a netlet, returns a single numeric value or a named
  numeric vector. called once per bootstrap draw.

- n_boot:

  integer. number of bootstrap replicates (default `200`).

- alpha:

  numeric in (0, 1). two-sided percentile-ci alpha (default `0.05` -\>
  95% intervals).

- seed:

  optional integer. if supplied, sets a local rng seed and restores the
  user's global stream afterward. if `NULL`, bootstrap draws use and
  advance the current rng stream normally.

- verbose:

  logical. if `TRUE` (default), print a progress ticker every 50 draws.

## Value

a `data.frame` with one row per element of `fn(netlet)` and columns:

- `metric`:

  name of the output element (or `"value"` for scalar fn).

- `point`:

  point estimate from `fn(netlet)` on the original (un-resampled)
  netlet.

- `n_valid`:

  number of non-`na` bootstrap draws used for that metric.

- `mean`:

  bootstrap mean.

- `sd`:

  bootstrap standard deviation.

- `lower`, `upper`:

  lower / upper percentile ci bounds.

the full per-draw matrix is stashed as `attr(out, "bootstrap_draws")`
for callers who want the empirical distribution (e.g., for
kernel-density plots).

## Details

this is the general bootstrap interface for any scalar or named numeric
vector summary of a netify. it is useful for homophily, mixing-matrix,
centrality, and downstream fit summaries (e.g., the coefficient of a
`to_igraph` -\> `igraph::cluster_*` -\> modularity pipeline).

resampling is **vertex-level**, not edge-level: actors are sampled with
replacement and the netlet's adjacency is sliced accordingly. for
longitudinal netlets, the same resampled index set is applied to every
period (preserves within-actor dependence). multilayer netlets are
unsupported – bootstrap each layer independently via
`subset_netify(layers = ...)` first. the resampled netlets contain the
resampled adjacency only. statistics that require attached nodal or
dyadic attributes should recreate those attributes inside `fn`, or use a
NULL-model workflow such as
`compare_to_null(..., model = "dyad_permutation")` when the goal is an
attribute-aware randomization.

## parallel execution

`bootstrap_netlet` runs serially. when `n_boot > 50` and the netlet is
large enough that each draw is non-trivial (rule of thumb: n \> ~1000
with the default centrality-heavy `fn`), parallelism helps. there is no
built-in `parallel =` argument; instead drive the loop yourself with
`future.apply::future_lapply()`, which respects whatever
`future::plan()` the caller set:


    library(future); library(future.apply)
    plan(multisession)        # or multicore on linux/macos
    draws <- future_lapply(1:n_boot, function(b) {
      idx <- sample(actors, length(actors), replace = TRUE)
      fn(subset_netify(net, actors = idx))
    }, future.seed = TRUE)

then summarize `draws` exactly as `bootstrap_netlet` does internally.
wrapping the inner body of `bootstrap_netlet` in a future-aware loop is
a small refactor and is the recommended path for ~15k-node weekly
snapshots where the serial pass would tie up a single core for hours.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
if (FALSE) { # \dontrun{
data(icews)
net <- netify(icews[icews$year == 2010, ],
actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "verbCoop")
# bootstrap a custom scalar: mean(closeness)
my_stat <- function(net) {
sa <- summary_actor(net, stats = "all")
c(mean_closeness = mean(sa$closeness_all, na.rm = TRUE))
}
boot_out <- bootstrap_netlet(net, fn = my_stat, n_boot = 100, seed = 1)
boot_out
} # }
```
