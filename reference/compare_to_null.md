# Test an observed network statistic against a NULL distribution

simulates `n_sim` networks from a NULL model (default erdos-renyi at the
observed density), applies the user-supplied statistic to each, and
reports how the observed statistic compares – point estimate, percentile
of the NULL distribution, and two-sided monte carlo p-value against the
selected NULL model.

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

  the observed netify object.

- fn:

  function. takes a **netify object** (not an igraph or matrix) and
  returns a single numeric value or a named numeric vector. called once
  on the observed netlet and `n_sim` times on the simulated draws. wrap
  igraph helpers with
  [`to_igraph()`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md)
  inside `fn`, e.g.
  `fn = function(net) igraph::transitivity(to_igraph(net))`.

- n_sim:

  integer. number of NULL-model draws (default `200`).

- model:

  character. NULL-model family; passed to
  [`simulate.netify()`](https://netify-dev.github.io/netify/reference/simulate.netify.md).
  one of `"erdos_renyi"` (default), `"configuration"`,
  `"dyad_permutation"`.

  **picking a NULL model.** for ergm-trained users:

  `erdos_renyi`

  :   random graph at observed density. use when the NULL hypothesis is
      "no structure beyond density."

  `configuration`

  :   random graph preserving the observed degree sequence. use when you
      want to ask "is my observed structure surprising *given* the
      degree distribution?" – already conditions on degree.

  `dyad_permutation`

  :   vertex-label permutation. preserves the full degree distribution
      and weight values; use when comparing attribute-aware structure
      (homophily, mixing) against actor relabelings.

  for weighted netlets, the simulator draws weights from the empirical
  non-zero weight distribution so observed-vs-NULL stats are computed on
  comparable scales (`dyad_permutation` is exact – it preserves weights
  via relabel).

- alpha:

  numeric in (0, 1). two-sided alpha for the percentile ci of the NULL
  distribution (default `0.05` -\> 95%).

- seed:

  optional integer. if supplied, sets a local rng seed and restores the
  user's global stream afterward. if `NULL`, NULL draws use and advance
  the current rng stream normally.

- verbose:

  logical. progress ticker every 50 draws.

## Value

a `data.frame` (also class `"netify_null_test"`) with one row per stat
and columns:

- `metric`:

  name of the statistic.

- `observed`:

  observed value on `netlet`.

- `n_valid`:

  number of non-`na` simulated draws used for that metric.

- `null_mean`, `null_sd`:

  moments of the NULL distribution.

- `null_lower`, `null_upper`:

  percentile ci bounds.

- `p_value`:

  two-sided monte carlo p-value using the simulated NULL draws.

- `extreme`:

  logical: is observed outside `[null_lower, null_upper]`?

## Details

combines
[`simulate.netify()`](https://netify-dev.github.io/netify/reference/simulate.netify.md) +
a vectorized `fn` loop into the single-call entry users actually want:
"is my observed transitivity surprising vs. a random graph?"

## parallel execution

`compare_to_null` runs serially. at `n_sim > 50` with large n, the
per-draw `fn()` cost (igraph community detection, full `summary_actor`)
dominates wall-clock. there is no built-in `parallel =` argument; drive
the loop manually with `future.apply::future_lapply()`:


    library(future); library(future.apply)
    plan(multisession)
    sims <- simulate(net, nsim = n_sim, model = "erdos_renyi", seed = 1)
    null_draws <- future_lapply(sims, fn, future.seed = TRUE)

summarize `null_draws` as `compare_to_null` does internally (`rowmeans`,
quantiles, two-sided p). for 15k-node weekly snapshots with
`n_sim = 500`, a 4-core `multisession` plan cuts wall-clock by roughly
3.5x.

## See also

[`simulate.netify()`](https://netify-dev.github.io/netify/reference/simulate.netify.md)
for the NULL-model engine,
[`bootstrap_netlet()`](https://netify-dev.github.io/netify/reference/bootstrap_netlet.md)
for sampling-variation cis around the observed stat itself.

## Author

cassy dorff, shahryar minhas

## Examples

``` r
if (FALSE) { # \dontrun{
data(icews)
net <- netify(icews[icews$year == 2010, ],
actor1 = "i", actor2 = "j", symmetric = FALSE, weight = "verbCoop")
my_stat <- function(net) {
s = suppressmessages(summary(net))
c(transitivity = s$transitivity, reciprocity = s$reciprocity)
}
compare_to_null(net, my_stat, n_sim = 200, seed = 1)
} # }
```
