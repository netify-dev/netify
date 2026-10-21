# netify 1.5.4

## Behavior changes

These fixes change numbers that earlier versions reported.

* `summary()` density excludes dyads with missing values from the
  denominator, matching `sna::gden()`. Networks without missing dyads are
  unaffected.
* Matrix, array, and list inputs whose diagonal is entirely zero are now read
  as having no self-loops (`diag_to_NA = TRUE`), matching the igraph import
  path. Pass `diag_to_NA = FALSE` to keep self-loop cells in density and
  `summary_actor()` `prop_ties`.
* `to_netify()` sets the diagonal of imported statnet `network` objects that
  do not allow loops to `NA`, matching the igraph import. Density for these
  networks now agrees with `sna::gden()`, and netify to statnet to netify
  round trips keep their density.
* `netify(..., output_format = "longit_array")` respects `actor_pds` and
  `actor_time_uniform = FALSE`. Actors outside their active periods are `NA`
  in those periods.
* Supplying `actor_pds` with gapped numeric time no longer creates empty
  periods for every integer in the range.
* The stored `actor_pds` (and `get_actor_time_info(net)`) uses time labels in
  the class of the time variable rather than integer positions.
* `mixing_matrix()` counts each undirected tie once, so `total_ties` is
  halved for symmetric networks and raw between-group cells hold half a tie
  in each mirrored cell. Proportions, assortativity, and modularity are
  unchanged.
* `compare_networks()` counts each undirected edge once in `edge_changes`.
  For networks with different actors, correlation, Jaccard, Hamming, QAP,
  and spectral distance are computed on the shared actors, with a message
  saying how many actors were left out.
* `bootstrap_netlet()` resamples longitudinal netlets by actor name, so the
  same actors are drawn in every period. Pairs formed by two copies of the
  same actor are `NA` rather than 0, removing a downward bias in density.
* `subset(from =, to =)` on unipartite networks returns a square matrix over
  the union of the selected actors, with ties outside the `from` x `to` block
  set to `NA`.

## New warnings and messages

* `netify()` warns when `symmetric = TRUE` and a dyad appears in both
  directions within a period with different weights. The value from the last
  row is still used; use `sum_dyads = TRUE`, `aggregate_dyad(..., symmetric =
  TRUE)`, or `symmetric = FALSE` to resolve it.
* `nodal_vars` is documented as describing `actor1`, and `netify()` reports
  actors that only appear as `actor2`, since they receive `NA`.

## Bug fixes

### Subsetting and transformations

* `subset()` / `subset_netify()` on longitudinal networks clips actor period
  bounds to the retained periods. Subsetting to several periods without both
  endpoints previously made `plot(x, time_filter = ...)`,
  `decompose_netify()`, and `add_node_vars()` fail with "Actor period bounds
  are not in the netlet time labels."
* `subset()` returns requested periods in netlet order without duplicates.
* `subset()` down to a single period keys dyadic data as cross-sectional, so
  `dyad_correlation()`, `attribute_report()`, `add_dyad_vars()`,
  `mutate_weights()`, and `pivot_dyad_to_network()` find existing dyadic
  variables.
* `subset()` of a multilayer longitudinal array to one period returns a
  cross-sectional `[actor, actor, layer]` array.
* `subset()` on a bipartite network gives a clear error when the selected
  actors come from only one mode.
* `ego_netify()` subsets `actor_pds`, nodal, and dyadic attributes to the ego
  network's actors and periods, and keeps the period name when only one
  period remains.
* `mutate_weights()` with only `add_constant` changes longitudinal arrays,
  updates weight attributes on every period of a longitudinal list, renames
  the layer of single-layer networks with `new_name`, and works on multilayer
  networks. `binarize()` also works on multilayer networks.

### Construction and attributes

* `nodal_vars`, `dyad_vars`, `add_node_vars()`, and `add_dyad_vars()` work
  with POSIXct time.

### Plotting

* `plot()` draws each tie once in undirected networks.
* `plot()` works for bipartite networks where an actor appears in both modes
  (for example `mexico`). `summary_actor()` returns plain actor names for
  these networks with an `actor_mode` column.
* `plot(..., static_actor_positions = TRUE)` works for longitudinal bipartite
  networks.
* `layout = "sugiyama"` works in `plot()` and `get_node_layout()`.
* `point_layout` accepts a single data.frame reused across periods.
* `plot_homophily(type = "distribution")` works for longitudinal networks.
* `label_size_by`, `label_color_by`, `label_fill_by`, and `label_alpha_by` no
  longer trigger an "Unknown parameter" warning.
* `node_filter` in longitudinal plots removes edges only in the periods where
  an endpoint is filtered out.
* `time_filter` accepts numeric time labels such as `time_filter = 2012` when
  they are not valid indices, and lists the available periods when nothing
  matches.

### Analysis

* `homophily()`, `mixing_matrix()`, `attribute_report()`, and
  `dyad_correlation()` work on multilayer longitudinal netlets stored as
  lists.
* `homophily(method = "hamming")` and `method = "jaccard"` accept character,
  factor, and logical attributes.
* `dyad_correlation()` aligns dyadic covariates to network actors by name.
* `compare_networks(by = ...)` works for `what = "structure"`, `"nodes"`, and
  `"attributes"`, and gives a clear error for edge comparisons.

### Converters

* `netify_to_statnet()` accepts factor and Date nodal attributes.
* `netify_to_amen(lame = TRUE)` and `to_lame(lame = TRUE)` order `Xrow` and
  `Xcol` rows to match each period's actors.
* The `to_lame()$ame_call` snippet runs with current lame, and
  `from_lame_fit()` returns a longitudinal netify for longitudinal
  `lame::lame()` fits.

### Documentation

* The `animate_netify()` example runs only when gganimate is installed.
* The `peek()` example uses actor names that exist in `icews`.

# netify 1.5.3

* CRAN resubmission: shorten `plot.netify()`, `compare_networks()`, and
  `netify_to_statnet()` examples so they run quickly on CRAN Windows checks.

# netify 1.5.2

* CRAN resubmission: remove the obsolete C++11 standard request and
  update DESCRIPTION wording flagged by CRAN incoming checks.

# netify 1.5.1

* Fixed `unnetify()` and `plot()` for bipartite networks so ties in
  same-position row and column entries are kept. These entries are valid
  cross-mode ties, not diagonal self-ties.

# netify 1.5

* New worked example dataset (`classroom_edges`, `classroom_nodes`) and a
  matching short-form vignette (`quickstart_inference`) for quick onboarding.
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
