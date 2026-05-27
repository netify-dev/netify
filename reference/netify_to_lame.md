# Convert a netify object to the format expected by `lame::ame()`

`netify_to_lame()` (also available as `to_lame()`) is a thin
specialization of `\link{netify_to_amen}` for the sibling `lame`
package, which is not on CRAN. Install from GitHub:
`remotes::install_github("netify-dev/lame")`. `lame::ame()` accepts the
same `Y` / `Xdyad` / `Xrow` / `Xcol` skeleton as
[`amen::ame()`](https://rdrr.io/pkg/amen/man/ame.html) but adds two
things netify users care about:

## Usage

``` r
netify_to_lame(
  netlet,
  lame = FALSE,
  family = NULL,
  pad = TRUE,
  fit_method = c("gibbs", "als"),
  bootstrap = 0L
)

to_lame(
  netlet,
  lame = FALSE,
  family = NULL,
  pad = TRUE,
  fit_method = c("gibbs", "als"),
  bootstrap = 0L
)
```

## Arguments

- netlet:

  A netify object.

- lame:

  Logical. As in
  [`netify_to_amen`](https://netify-dev.github.io/netify/reference/netify_to_amen.md):
  pass `TRUE` when actor composition varies over time. Default `FALSE`.

- family:

  Optional character. AME family to use. If `NULL` (default), inferred
  from the netify: `"binary"` for binary networks, `"normal"` for
  weighted, with a once-per-session info message naming the choice.

- pad:

  Logical. When `lame = TRUE` and the per-period list is returned (the
  function never actually pads in place; it always returns a list),
  controls whether the once-per-session info message points users at the
  `lame::list_to_array()` + `lame::lame()` snippet baked into
  `nl$ame_call`. Default `TRUE` (emit the message); set `FALSE` to
  silence it.

- fit_method:

  One of `"gibbs"` (default — MCMC posterior via `lame::ame()` /
  `lame::lame()`) or `"als"` (fast alternating- least-squares point
  estimate via `lame::ame_als()`). For bipartite + binary networks where
  MCMC is slow, ALS with `bootstrap > 0` gives a fast point estimate
  plus parametric/block bootstrap uncertainty intervals in a single
  call. The choice only affects the generated `ame_call` snippet — the
  `Y`/`Xdyad`/ `Xrow`/`Xcol` payload is identical.

- bootstrap:

  Integer. Number of bootstrap replicates for ALS uncertainty intervals.
  Ignored when `fit_method = "gibbs"` (MCMC draws ARE the uncertainty
  representation there). Default `0` (no bootstrap). Pass `200` for a
  reasonable interval estimate.

## Value

A list with the same shape as
[`netify_to_amen`](https://netify-dev.github.io/netify/reference/netify_to_amen.md)
output, plus two helpful extras:

- `mode`:

  Character: `"unipartite"` or `"bipartite"`. Pass this directly to
  `lame::ame(mode = .)`.

- `family`:

  Character: the suggested family (`"binary"` or `"normal"`). Pass to
  `lame::ame(family = .)`.

- `ame_call`:

  Character: a literal `lame::ame()` call string the user can
  copy-paste.

## Details

- rectangular `Y` for **bipartite** networks via `mode = "bipartite"` —
  the standard [`amen::ame()`](https://rdrr.io/pkg/amen/man/ame.html)
  rejects this.

- ragged longitudinal panels via `lame::list_to_array()` →
  `lame::ame_rep_*` — `amen` requires constant actor composition.

This function wraps `\link{netify_to_amen}` and:

1.  picks a `family` default appropriate to the netify (`"binary"` for
    binary nets, `"normal"` for weighted),

2.  suggests the correct `mode` argument for `lame::ame()`
    (`"unipartite"` / `"bipartite"`),

3.  for `lame = TRUE` longitudinal output, emits a copy-paste
    `lame::list_to_array()` + `lame::lame()` snippet in the returned
    `ame_call` slot. The function itself does **not** pad the list — run
    the snippet (or the literal call) to do that step yourself.

**Bipartite + binary: the case
[`to_amen()`](https://netify-dev.github.io/netify/reference/netify_to_amen.md)
cannot fit.** For a bipartite weighted-binary netify (e.g., person ×
event attendance, 0/1), this is the exact pipeline:


    bp <- netify(df, actor1 = "person", actor2 = "event",
    mode = "bipartite", weight = "attended")
    nl <- to_lame(bp) # auto-detects binary
    fit <- lame::ame(
    Y = nl$Y, Xrow = nl$Xrow, Xcol = nl$Xcol,
    mode = nl$mode, family = nl$family,
    nscan = 1000, burn = 500
    )
    # uncertainty: posterior intervals are in fit$BETA / fit$VC

**Ragged longitudinal panels.** When `lame = TRUE` and actors enter /
exit the network, this function:

1.  builds the per-period list via `netify_to_amen(lame=TRUE)`,

2.  bakes a `lame::list_to_array(actors = U, Y = nl$Y, ...)`
    pad-then-fit snippet into `nl$ame_call` that the user runs to
    materialize the 3D `[n, n, T]` array and fit `lame::lame()`.

The returned `Y` / `Xdyad` / `Xrow` / `Xcol` are always the per-period
list — `pad` only controls whether the once-per- session info message
reminding the user of the snippet fires.

## See also

[`netify_to_amen`](https://netify-dev.github.io/netify/reference/netify_to_amen.md)
(the underlying converter),
[`netify_to_statnet`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md),
[`netify_to_dbn`](https://netify-dev.github.io/netify/reference/netify_to_dbn.md),
[`netify_to_igraph`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md).

## Author

Shahryar Minhas

Cassy Dorff, Shahryar Minhas
