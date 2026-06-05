#' Common netify workflows -- index of patterns
#'
#' this is a documentation-only topic ("`?netify_workflows`") that
#' indexes the standard pipelines netify users actually run, with
#' the function calls in order. new users opening `?netify` see the
#' constructor; opening this topic sees the recipes.
#'
#' @section from a friendship edgelist:
#' \preformatted{
#' library(netify)
#' net <- netify(friends, actor1 = "from", actor2 = "to",
#' symmetric = TRUE, nodal_data = people)
#' summary(net)
#' plot(net, node_color_by = "gender")
#' }
#' a ready-to-run worked example ships with the package:
#' \preformatted{
#' data(classroom_edges); data(classroom_nodes)
#' net <- netify(classroom_edges, actor1 = "from", actor2 = "to",
#' symmetric = TRUE, nodal_data = classroom_nodes)
#' summary(net)
#' plot(net, node_color_by = "gender", node_size_by = "gpa")
#' }
#'
#' @section from a bipartite (two-mode) edgelist:
#' a bipartite edgelist links \emph{two different kinds of things}
#' (e.g. companies and jurisdictions, movies and actors, students and
#' classes). build with `mode = "bipartite"` and pass row-mode /
#' col-mode covariates separately.
#' \preformatted{
#' net <- netify(filings, actor1 = "company", actor2 = "jurisdiction",
#' mode = "bipartite", weight = "n_filings")
#' net <- add_node_vars(net, company_meta,
#' actor = "company",
#' node_vars = c("industry", "hq"))
#' net <- add_node_vars(net, jurisdiction_meta,
#' actor = "jurisdiction",
#' node_vars = "tax_haven")
#' plot(net, style = "heatmap")
#' }
#'
#' @section from a longitudinal dyadic event dataset (icews-style):
#' \preformatted{
#' net <- netify(icews, actor1 = "i", actor2 = "j", time = "year",
#' symmetric = FALSE, weight = "verbCoop")
#' net <- add_node_vars(net, country_panel,
#' actor = "actor", time = "year",
#' node_vars = c("polity", "gdp_log"))
#' s <- summary(net)
#' plot(s)
#' }
#'
#' @section multilayer (one call per layer):
#' \preformatted{
#' # build one netify per layer, then combine with layer_netify()
#' channels <- sort(unique(slack_long$channel))
#' nets <- lapply(channels, function(ch) {
#'   netify(slack_long[slack_long$channel == ch, ],
#'          actor1 = "from", actor2 = "to",
#'          time = "month", weight = "n_messages")
#' })
#' ml <- layer_netify(setNames(nets, channels))
#' }
#'
#' @section to ergm (`statnet`):
#' \preformatted{
#' ntwk <- to_statnet(net)
#' library(ergm)
#' fit <- ergm(ntwk ~ edges + nodematch("gender") + nodecov("gpa"))
#' summary(fit)
#' }
#'
#' @section to longitudinal ergm (`tergm 4.x`):
#' \preformatted{
#' nl <- to_statnet(longit_net) # network.list
#' library(tergm)
#' fit <- tergm(nl ~ form(~ edges + nodematch("gender")) +
#' persist(~ edges + mutual),
#' estimate = "cmle", times = seq_along(nl))
#' }
#'
#' @section to ame (gibbs / mcmc posterior):
#' \preformatted{
#' lm_in <- to_lame(net) # auto-picks family from binary/weighted
#' library(lame)
#' # copy-paste the printed `lm_in$ame_call`:
#' fit <- lame::ame(y = lm_in$y, xdyad = lm_in$xdyad,
#' xrow = lm_in$xrow, xcol = lm_in$xcol,
#' family = "binary", nscan = 1000, burn = 500)
#' }
#'
#' @section to ame (binary bipartite + als + bootstrap uncertainty):
#' \preformatted{
#' # fast point estimate + parametric bootstrap cis in one call
#' lm_in <- to_lame(net_bp, fit_method = "als", bootstrap = 200)
#' library(lame)
#' fit <- lame::ame_als(y = lm_in$y, mode = "bipartite",
#' family = "binary", bootstrap = 200l)
#' confint(fit) # bootstrap cis
#' pred <- from_lame_fit(fit, value = "prob")
#' plot(pred, style = "heatmap")
#' }
#'
#' @section NULL-model hypothesis test:
#' \preformatted{
#' # is observed transitivity surprising vs erdos-renyi at this density?
#' compare_to_null(net,
#' fn = function(n) c(transitivity = summary(n)$transitivity),
#' n_sim = 200, model = "erdos_renyi", seed = 1)
#' }
#'
#' @section bootstrap any custom statistic:
#' \preformatted{
#' my_stat <- function(net) {
#' sa <- summary_actor(net, stats = "all")
#' c(max_betw = max(sa$betweenness, na.rm = TRUE))
#' }
#' bootstrap_netlet(net, fn = my_stat, n_boot = 200, seed = 1)
#' }
#'
#' @section very large n (~10k+ actors):
#' netify stores adjacencies densely, so an n x n matrix consumes
#' 8 * n^2 bytes (1.7 gb at n = 15k, 20 gb at n = 50k) per snapshot.
#' the recipes below avoid the dense bottleneck where possible.
#' \preformatted{
#' # 1. build directly from an edgelist data.frame -- skip any
#' #    pre-built sparse / dense matrix intermediate.
#' net <- netify(edges, actor1 = "from", actor2 = "to",
#'               weight = "n", symmetric = FALSE)
#'
#' # 2. use the fast actor-summary path -- skips closeness /
#' #    betweenness / eigen / hits, which dominate wall-clock at
#' #    large n. netify auto-promotes at n >= netify.fast_threshold
#' #    (default 1500); tune via options(netify.fast_threshold = ...).
#' sa <- summary_actor(net)            # auto-promotes
#'
#' # 3. for community detection / shortest paths / clustering, hand
#' #    off to igraph. its c backend handles 50k+ nodes comfortably.
#' ig <- to_igraph(net)
#' cl <- igraph::cluster_louvain(ig)
#'
#' # 4. when you need a sparse object for downstream code (e.g.
#' #    spectral methods, irlba, glmnet), exit via to_igraph() and
#' #    grab the sparse adjacency from there.
#' ig <- to_igraph(net)
#' m  <- igraph::as_adjacency_matrix(ig, sparse = TRUE)  # requires matrix
#'
#' # 5. if you must accept a pre-built sparse matrix, density < 1%
#' #    with n > 5,000 aborts to protect against accidental
#' #    gigabyte allocations -- pass force_dense = TRUE to override.
#' net <- netify(m_sparse, force_dense = TRUE)
#' }
#'
#' @section round-trip to / from other formats:
#' \preformatted{
#' # to igraph and back
#' ig <- to_igraph(net)
#' back <- netify(ig) # preserves directedness + weight
#'
#' # to long data frame
#' edges <- unnetify(net, remove_zeros = TRUE)
#' tb <- tibble::as_tibble(net) # same thing, tibble class
#'
#' # to base matrix
#' m <- as.matrix(net) # strips netify class
#' }
#'
#' @section stacking:
#' \preformatted{
#' # combine two cross-sec into a 2-period longit
#' combined <- bind_netifies(n_2020, n_2021, names = c("2020", "2021"))
#' }
#'
#' @seealso [netify()] for the constructor; [summary.netify()] for
#' graph-level stats; [plot.netify()] for visualization;
#' [to_statnet()] / [to_amen()] / [to_lame()] / [to_igraph()] for
#' handoff to modeling packages; [bootstrap_netlet()] /
#' [compare_to_null()] / [simulate.netify()] for inference; and
#' [bind_netifies()] for combining.
#'
#' @name netify_workflows
#' @keywords documentation
#'
#' @author cassy dorff, shahryar minhas
#'
NULL
