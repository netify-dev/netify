# Bootstrap any user-supplied function of a netify object

Resamples actors with replacement (Snijders & Borgatti 1999 vertex
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

  A `netify` object.

- fn:

  Function. Takes a netlet, returns a single numeric value or a named
  numeric vector. Called once per bootstrap draw.

- n_boot:

  Integer. Number of bootstrap replicates (default `200`).

- alpha:

  Numeric in (0, 1). Two-sided percentile-CI alpha (default `0.05` → 95%
  intervals).

- seed:

  Optional integer. If supplied, sets a local RNG seed without leaking
  into the user's global stream.

- verbose:

  Logical. If `TRUE` (default), print a progress ticker every 50 draws.

## Value

A `data.frame` with one row per element of `fn(netlet)` and columns:

- `metric`:

  Name of the output element (or `"value"` for scalar fn).

- `point`:

  Point estimate from `fn(netlet)` on the original (un-resampled)
  netlet.

- `mean`:

  Bootstrap mean.

- `sd`:

  Bootstrap standard deviation.

- `lower`, `upper`:

  Lower / upper percentile CI bounds.

The full per-draw matrix is stashed as `attr(out, "bootstrap_draws")`
for callers who want the empirical distribution (e.g., for
kernel-density plots).

## Details

This generalizes the per-graph-stat bootstrap that
`summary(net, bootstrap = TRUE)` runs internally: any scalar / named
numeric vector summary of a netify can be wrapped. Useful for homophily
/ mixing-matrix / centrality stats that don't expose their own
bootstrap, and for downstream fit summaries (e.g., the coefficient of a
`to_igraph` -\> `igraph::cluster_*` -\> modularity pipeline).

Resampling is **vertex-level**, not edge-level: actors are sampled with
replacement and the netlet's adjacency is sliced accordingly. For
longitudinal netlets, the same resampled index set is applied to every
period (preserves within-actor dependence). Multilayer netlets are
unsupported — bootstrap each layer independently via
`subset_netify(layers = ...)` first.

## Parallel execution

`bootstrap_netlet` runs serially. When `n_boot > 50` and the netlet is
large enough that each draw is non-trivial (rule of thumb: N \> ~1000
with the default centrality-heavy `fn`), parallelism helps. There is no
built-in `parallel =` argument; instead drive the loop yourself with
`future.apply::future_lapply()`, which respects whatever
`future::plan()` the caller set:


    library(future); library(future.apply)
    plan(multisession)        # or multicore on Linux/macOS
    draws <- future_lapply(1:n_boot, function(b) {
      idx <- sample(actors, length(actors), replace = TRUE)
      fn(subset_netify(net, actors = idx))
    }, future.seed = TRUE)

Then summarize `draws` exactly as `bootstrap_netlet` does internally.
Wrapping the inner body of `bootstrap_netlet` in a future-aware loop is
a small refactor and is the recommended path for ~15K-node weekly
snapshots where the serial pass would tie up a single core for hours.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
if (FALSE) { # \dontrun{
data(icews)
net <- netify(icews[icews$year == 2010, ],
actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "verbCoop")
# bootstrap a custom scalar: mean(closeness)
my_stat <- function(net) {
sa <- summary_actor(net, stats = "closeness")
c(mean_closeness = mean(sa$closeness_all, na.rm = TRUE))
}
boot_out <- bootstrap_netlet(net, fn = my_stat, n_boot = 100, seed = 1)
boot_out
} # }
```
