# Convert a netify object to the format expected by `lame::ame()`

`netify_to_lame()` (also available as `to_lame()`) is a thin
specialization of `\link{netify_to_amen}` for the optional `lame`
workflow. `lame::ame()` accepts the same `y` / `xdyad` / `xrow` / `xcol`
skeleton as [`amen::ame()`](https://rdrr.io/pkg/amen/man/ame.html) but
adds two things netify users care about:

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

  a netify object.

- lame:

  logical. as in
  [`netify_to_amen`](https://netify-dev.github.io/netify/reference/netify_to_amen.md):
  pass `TRUE` when actor composition varies over time. default `FALSE`.

- family:

  optional character. ame family to use. if `NULL` (default), inferred
  from the netify: `"binary"` for binary networks, `"normal"` for
  weighted, with a once-per-session info message naming the choice.

- pad:

  logical. when `lame = TRUE` and the per-period list is returned (the
  function never actually pads in place; it always returns a list),
  controls whether the once-per-session info message points users at the
  padding + `lame::lame()` snippet baked into `nl$ame_call`. default
  `TRUE` (emit the message); set `FALSE` to silence it.

- fit_method:

  one of `"gibbs"` (default – mcmc posterior via `lame::ame()` /
  `lame::lame()`) or `"als"` (fast alternating- least-squares point
  estimate via `lame::ame_als()`). for bipartite + binary networks where
  mcmc is slow, als with `bootstrap > 0` gives a fast point estimate
  plus parametric/block bootstrap uncertainty intervals in a single
  call. the choice only affects the generated `ame_call` snippet – the
  `y`/`xdyad`/ `xrow`/`xcol` payload is identical.

- bootstrap:

  integer. number of bootstrap replicates for als uncertainty intervals.
  ignored when `fit_method = "gibbs"` (mcmc draws are the uncertainty
  representation there). default `0` (no bootstrap). pass `200` for a
  reasonable interval estimate.

## Value

a list with the same shape as
[`netify_to_amen`](https://netify-dev.github.io/netify/reference/netify_to_amen.md)
output, plus two helpful extras:

- `mode`:

  character: `"unipartite"` or `"bipartite"`. pass this directly to
  `lame::ame(mode = .)`.

- `family`:

  character: the suggested family (`"binary"` or `"normal"`). pass to
  `lame::ame(family = .)`.

- `ame_call`:

  character: a literal `lame::ame()` call string the user can
  copy-paste.

## Details

- rectangular `y` for **bipartite** networks via `mode = "bipartite"` –
  the standard [`amen::ame()`](https://rdrr.io/pkg/amen/man/ame.html)
  rejects this.

- ragged longitudinal panels via a generated padding snippet – `amen`
  requires constant actor composition.

this function wraps `\link{netify_to_amen}` and:

1.  picks a `family` default appropriate to the netify (`"binary"` for
    binary nets, `"normal"` for weighted),

2.  suggests the correct `mode` argument for `lame::ame()`
    (`"unipartite"` / `"bipartite"`),

3.  for `lame = TRUE` longitudinal output, emits a copy-paste padding +
    `lame::lame()` snippet in the returned `ame_call` slot. the function
    itself does **not** pad the list – run the snippet (or the literal
    call) to do that step yourself.

**bipartite + binary: the case
[`to_amen()`](https://netify-dev.github.io/netify/reference/netify_to_amen.md)
cannot fit.** for a bipartite weighted-binary netify (e.g., person x
event attendance, 0/1), this is the exact pipeline:


    bp <- netify(df, actor1 = "person", actor2 = "event",
    mode = "bipartite", weight = "attended")
    nl <- to_lame(bp) # auto-detects binary
    fit <- lame::ame(
    y = nl$y, xrow = nl$xrow, xcol = nl$xcol,
    mode = nl$mode, family = nl$family,
    nscan = 1000, burn = 500
    )
    # uncertainty: posterior intervals are in fit$beta / fit$vc

**ragged longitudinal panels.** when `lame = TRUE` and actors enter /
exit the network, this function:

1.  builds the per-period list via `netify_to_amen(lame=TRUE)`,

2.  bakes a pad-then-fit snippet into `nl$ame_call` that the user runs
    to materialize the 3d `[n, n, t]` array and fit `lame::lame()`.
    unipartite snippets use `lame::list_to_array()`; bipartite snippets
    pad rectangular row-by-column arrays directly.

the returned `y` / `xdyad` / `xrow` / `xcol` are always the per-period
list – `pad` only controls whether the once-per- session info message
reminding the user of the snippet fires.

## See also

[`netify_to_amen`](https://netify-dev.github.io/netify/reference/netify_to_amen.md)
(the underlying converter),
[`netify_to_statnet`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md),
[`netify_to_dbn`](https://netify-dev.github.io/netify/reference/netify_to_dbn.md),
[`netify_to_igraph`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md).

## Author

shahryar minhas

cassy dorff, shahryar minhas
