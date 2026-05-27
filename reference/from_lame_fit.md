# Convert a fitted `lame`/`amen` AME object back into a netify

Takes the posterior-mean (or point-estimate) prediction matrix from a
`lame::ame()` / `lame::ame_als()` /
[`amen::ame()`](https://rdrr.io/pkg/amen/man/ame.html) fit and wraps it
as a netify so the predictions can be summarized, plotted, or compared
to the observed netlet via
[`compare_networks()`](https://netify-dev.github.io/netify/reference/compare_networks.md).

## Usage

``` r
from_lame_fit(
  fit,
  value = c("fitted", "residual", "prob", "prob_lower", "prob_upper", "fitted_lower",
    "fitted_upper"),
  symmetric = NULL,
  alpha = 0.05
)
```

## Arguments

- fit:

  A fitted object from `lame::ame()`, `lame::ame_als()`, `lame::lame()`,
  or [`amen::ame()`](https://rdrr.io/pkg/amen/man/ame.html). Must expose
  a fitted-value matrix (e.g., `fit$EZ`, `fit$ZpostMean`, or
  `fitted(fit)`). For `value` in `"prob_lower"` / `"prob_upper"` /
  `"fitted_lower"` / `"fitted_upper"` the fit must additionally expose a
  per-draw array of fitted values. The slots searched, in order, are:
  `fit$BOOT$EZ` and `fit$BOOT$Y_hat` (lame ALS parametric / block
  bootstrap), then `fit$EZ_draws`, `fit$EZps`, and `fit$Z_draws` (Gibbs
  posterior draws), with shape `[n, n, B]` or `[n, n, T, B]`.

- value:

  One of `"fitted"` (default — posterior-mean linear predictor
  `EZ`/`ZpostMean`), `"residual"` (observed - fitted), `"prob"`
  (logistic / probit -\> probability scale, when the family supports
  it), or `"prob_lower"` / `"prob_upper"` / `"fitted_lower"` /
  `"fitted_upper"` (per-cell `alpha`/`1-alpha` quantiles across
  bootstrap or posterior draws). When `lame` exposes a
  [`fitted()`](https://rdrr.io/r/stats/fitted.values.html) method for
  the object, that's preferred. For `value = "prob"` with a binary
  family, link detection follows this priority: (1) any explicit `link`
  slot on the fit (or `fit$control$link`); (2) ALS class (any token
  containing "als") or `fit$fit_method = "als"` -\> probit, matching
  `lame::ame_als()`; (3) `lame` class -\> logit, matching `lame::lame()`
  Gibbs default; (4) `ame` / `amen` class (and no `fit_method = "logit"`
  override) -\> probit, matching
  [`amen::ame()`](https://rdrr.io/pkg/amen/man/ame.html) Gibbs
  convention; (5) otherwise logit fallback.

- symmetric:

  Logical. Override the inferred symmetry; default reads from the fit's
  stored `mode`/`Y`.

- alpha:

  Numeric in (0, 0.5). Tail probability for the `*_lower` / `*_upper`
  quantiles. Default `0.05` -\> 90% interval (lower = 0.025, upper =
  0.975 when interpreted as a two-sided CI; here we use lower = alpha/2,
  upper = 1 - alpha/2).

## Value

A cross-sectional netify object whose underlying matrix is the
fitted-value (or residual) matrix at the same actor ordering.

## Details

Useful for posterior-predictive checks: build the observed netlet, run
an AME fit, then `from_lame_fit(fit) |> plot(style = "heatmap")` to
visualize the fitted intensity matrix.

## Author

Cassy Dorff, Shahryar Minhas

## Examples

``` r
if (FALSE) { # \dontrun{
lm_in <- to_lame(net, fit_method = "als", bootstrap = 200)
fit <- lame::ame_als(Y = lm_in$Y, mode = "bipartite", family = "binary",
bootstrap = 200)
# round-trip predictions back into a netify for plotting
pred_net <- from_lame_fit(fit, value = "prob")
plot(pred_net, style = "heatmap")
} # }
```
