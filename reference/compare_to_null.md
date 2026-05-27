# Test an observed network statistic against a null distribution

Simulates `n_sim` networks from a null model (default Erdős-Rényi at the
observed density), applies the user-supplied statistic to each, and
reports how the observed statistic compares — point estimate, percentile
of the null distribution, and two-sided permutation p-value.

## Usage

``` r
compare_to_null(
  netlet,
  fn,
  n_sim = 200L,
  model = c("erdos_renyi", "configuration", "dyad_permutation"),
  alpha = 0.05,
  seed = NULL,
  verbose = TRUE
)
```

## Arguments

- netlet:

  The observed netify object.

- fn:

  Function. Takes a **netify object** (not an igraph or matrix) and
  returns a single numeric value or a named numeric vector. Called once
  on the observed netlet and `n_sim` times on the simulated draws. Wrap
  igraph helpers with
  [`to_igraph()`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md)
  inside `fn`, e.g.
  `fn = function(net) igraph::transitivity(to_igraph(net))`.

- n_sim:

  Integer. Number of null-model draws (default `200`).

- model:

  Character. Null-model family; passed to
  [`simulate.netify()`](https://netify-dev.github.io/netify/reference/simulate.netify.md).
  One of `"erdos_renyi"` (default), `"configuration"`,
  `"dyad_permutation"`.

  **Picking a null model.** For ERGM-trained users:

  `erdos_renyi`

  :   Random graph at observed density. Use when the null hypothesis is
      "no structure beyond density."

  `configuration`

  :   Random graph preserving the observed degree sequence. Use when you
      want to ask "is my observed structure surprising *given* the
      degree distribution?" — already conditions on degree.

  `dyad_permutation`

  :   Vertex-label permutation. Preserves the full degree distribution
      and weight values; use when comparing attribute-aware structure
      (homophily, mixing) against actor relabelings.

  For weighted netlets, the simulator draws weights from the empirical
  non-zero weight distribution so observed-vs-null stats are computed on
  comparable scales (`dyad_permutation` is exact — it preserves weights
  via relabel).

- alpha:

  Numeric in (0, 1). Two-sided alpha for the percentile CI of the null
  distribution (default `0.05` → 95%).

- seed:

  Optional integer. Local RNG seed (does not leak into the user's global
  stream).

- verbose:

  Logical. Progress ticker every 50 draws.

## Value

A `data.frame` (also class `"netify_null_test"`) with one row per stat
and columns:

- `metric`:

  Name of the statistic.

- `observed`:

  Observed value on `netlet`.

- `null_mean`, `null_sd`:

  Moments of the null distribution.

- `null_lower`, `null_upper`:

  Percentile CI bounds.

- `p_value`:

  Two-sided permutation p-value.

- `extreme`:

  Logical: is observed outside `[null_lower, null_upper]`?

## Details

Combines
[`simulate.netify()`](https://netify-dev.github.io/netify/reference/simulate.netify.md) +
a vectorized `fn` loop into the single-call entry users actually want:
"is my observed transitivity surprising vs. a random graph?"

## Parallel execution

`compare_to_null` runs serially. At `n_sim > 50` with large N, the
per-draw `fn()` cost (igraph community detection, full `summary_actor`)
dominates wall-clock. There is no built-in `parallel =` argument; drive
the loop manually with `future.apply::future_lapply()`:


    library(future); library(future.apply)
    plan(multisession)
    sims <- simulate(net, nsim = n_sim, model = "erdos_renyi", seed = 1)
    null_draws <- future_lapply(sims, fn, future.seed = TRUE)

Summarize `null_draws` as `compare_to_null` does internally (`rowMeans`,
quantiles, two-sided p). For 15K-node weekly snapshots with
`n_sim = 500`, a 4-core `multisession` plan cuts wall-clock by roughly
3.5x.

## See also

[`simulate.netify()`](https://netify-dev.github.io/netify/reference/simulate.netify.md)
for the null-model engine,
[`bootstrap_netlet()`](https://netify-dev.github.io/netify/reference/bootstrap_netlet.md)
for sampling-variation CIs around the observed stat itself.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
if (FALSE) { # \dontrun{
data(icews)
net <- netify(icews[icews$year == 2010, ],
actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "verbCoop")
my_stat <- function(net) {
s = suppressMessages(summary(net))
c(transitivity = s$transitivity, reciprocity = s$reciprocity)
}
compare_to_null(net, my_stat, n_sim = 200, seed = 1)
} # }
```
