# netify 1.5

* New worked example dataset (`classroom_edges`, `classroom_nodes`) and a
  matching short-form vignette (`foundations_lite`) for quick onboarding.
* Index of common pipelines available at `?netify_workflows`, covering
  unipartite, bipartite, longitudinal, and multilayer setups, plus
  handoff to `statnet`, `amen`, `lame`, `dbn`, and `igraph`.
* `to_lame()` now bundles posterior round-tripping via `from_lame_fit()`,
  including bootstrap and per-cell credible-interval bounds for binary
  bipartite fits.
* `bind_netifies()` gains `align_actors = c("none", "union",
  "intersection")` for combining cross-sections with mismatched actor
  sets.
* New helpers: `drop_na_actors()`, `binarize()`, `validate_netify()`,
  `bootstrap_netlet()`, `compare_to_null()`, `simulate.netify()`, plus
  type predicates (`is_binary()`, `is_bipartite()`, `is_longitudinal()`,
  `is_multilayer()`, `n_actors()`, `n_periods()`, `n_layers()`) and
  accessors (`nodal_data()`).
* tidyverse and broom interop: `as_tibble.netify()`, `tidy.netify()`,
  `glance.netify()`, plus `as.igraph.netify()` and `as.network.netify()`
  S3 methods registered automatically when the relevant package is
  attached.
* Plotting picks up `style = "heatmap"` for adjacency-matrix views,
  auto-diverging palettes for signed weights, smarter label-repel for
  small networks, and two extra themes (`theme_publication_netify()`,
  `theme_publication_netify_ts()`).
* `summary_actor()` gains a `stats = "fast"` mode (skips the expensive
  centralities) and auto-promotes for networks above
  `getOption("netify.fast_threshold", 1500)`.
* Vignettes expanded and reorganized: `pipeline_netify_ergm`,
  `pipeline_lame_dbn`, `multilayer_networks`, `tracking_changes`,
  `attribute_analysis`, `manual_plotting`, and `internals` are all new
  or substantially rewritten.
