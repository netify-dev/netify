# Changelog

## netify 1.5

- New worked example dataset (`classroom_edges`, `classroom_nodes`) and
  a matching short-form vignette (`quickstart_inference`) for quick
  onboarding.
- Index of common pipelines available at
  [`?netify_workflows`](https://netify-dev.github.io/netify/reference/netify_workflows.md),
  covering unipartite, bipartite, longitudinal, and multilayer setups,
  plus handoff to `statnet`, `amen`, `lame`, `dbn`, and `igraph`.
- [`to_lame()`](https://netify-dev.github.io/netify/reference/netify_to_lame.md)
  now bundles posterior round-tripping via
  [`from_lame_fit()`](https://netify-dev.github.io/netify/reference/from_lame_fit.md),
  including bootstrap and per-cell credible-interval bounds for binary
  bipartite fits.
- [`bind_netifies()`](https://netify-dev.github.io/netify/reference/bind_netifies.md)
  gains `align_actors = c("none", "union", "intersection")` for
  combining cross-sections with mismatched actor sets.
- New helpers:
  [`drop_na_actors()`](https://netify-dev.github.io/netify/reference/drop_na_actors.md),
  [`binarize()`](https://netify-dev.github.io/netify/reference/binarize.md),
  [`validate_netify()`](https://netify-dev.github.io/netify/reference/validate_netify.md),
  [`bootstrap_netlet()`](https://netify-dev.github.io/netify/reference/bootstrap_netlet.md),
  [`compare_to_null()`](https://netify-dev.github.io/netify/reference/compare_to_null.md),
  [`simulate.netify()`](https://netify-dev.github.io/netify/reference/simulate.netify.md),
  plus type predicates
  ([`is_binary()`](https://netify-dev.github.io/netify/reference/netify_predicates.md),
  [`is_bipartite()`](https://netify-dev.github.io/netify/reference/netify_predicates.md),
  [`is_longitudinal()`](https://netify-dev.github.io/netify/reference/netify_predicates.md),
  [`is_multilayer()`](https://netify-dev.github.io/netify/reference/netify_predicates.md),
  [`n_actors()`](https://netify-dev.github.io/netify/reference/netify_predicates.md),
  [`n_periods()`](https://netify-dev.github.io/netify/reference/netify_predicates.md),
  [`n_layers()`](https://netify-dev.github.io/netify/reference/netify_predicates.md))
  and accessors
  ([`nodal_data()`](https://netify-dev.github.io/netify/reference/netify_predicates.md)).
- tidyverse and broom interop:
  [`as_tibble.netify()`](https://netify-dev.github.io/netify/reference/as_tibble.netify.md),
  [`tidy.netify()`](https://netify-dev.github.io/netify/reference/tidy.netify.md),
  [`glance.netify()`](https://netify-dev.github.io/netify/reference/glance.netify.md),
  plus
  [`as.igraph.netify()`](https://netify-dev.github.io/netify/reference/as.igraph.netify.md)
  and
  [`as.network.netify()`](https://netify-dev.github.io/netify/reference/as.network.netify.md)
  S3 methods registered automatically when the relevant package is
  attached.
- Plotting picks up `style = "heatmap"` for adjacency-matrix views,
  auto-diverging palettes for signed weights, smarter label-repel for
  small networks, and two extra themes
  ([`theme_publication_netify()`](https://netify-dev.github.io/netify/reference/theme_publication_netify.md),
  [`theme_publication_netify_ts()`](https://netify-dev.github.io/netify/reference/theme_publication_netify_ts.md)).
- [`summary_actor()`](https://netify-dev.github.io/netify/reference/summary_actor.md)
  gains a `stats = "fast"` mode (skips the expensive centralities) and
  auto-promotes for networks above
  `getOption("netify.fast_threshold", 1500)`.
- Vignettes expanded and reorganized: `pipeline_netify_ergm`,
  `pipeline_lame_dbn`, `multilayer_networks`, `tracking_changes`,
  `attribute_analysis`, `manual_plotting`, and `internals` are all new
  or substantially rewritten.
