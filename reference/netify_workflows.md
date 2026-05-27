# Common netify workflows — index of patterns

This is a documentation-only topic ("`?netify_workflows`") that indexes
the standard pipelines netify users actually run, with the function
calls in order. New users opening
[`?netify`](https://netify-dev.github.io/netify/reference/netify.md) see
the constructor; opening this topic sees the recipes.

## From a friendship edgelist


    library(netify)
    net <- netify(friends, actor1 = "from", actor2 = "to",
    symmetric = TRUE, nodal_data = people)
    summary(net)
    plot(net, node_color_by = "gender")

A ready-to-run worked example ships with the package:


    data(classroom_edges); data(classroom_nodes)
    net <- netify(classroom_edges, actor1 = "from", actor2 = "to",
    symmetric = TRUE, nodal_data = classroom_nodes)
    summary(net)
    plot(net, node_color_by = "gender", node_size_by = "gpa")

## From a bipartite (two-mode) edgelist

A bipartite edgelist links *two different kinds of things* (e.g.
companies and jurisdictions, movies and actors, students and classes).
Build with `mode = "bipartite"` and pass row-mode / col-mode covariates
separately.


    net <- netify(filings, actor1 = "company", actor2 = "jurisdiction",
    mode = "bipartite", weight = "n_filings")
    net <- add_node_vars(net, company_meta,
    actor = "company", mode = "row",
    node_vars = c("industry", "hq"))
    net <- add_node_vars(net, jurisdiction_meta,
    actor = "jurisdiction", mode = "col",
    node_vars = "tax_haven")
    plot(net, style = "heatmap")

## From a longitudinal dyadic event dataset (ICEWS-style)


    net <- netify(icews, actor1 = "i", actor2 = "j", time = "year",
    symmetric = FALSE, weight = "verbCoop")
    net <- add_node_vars(net, country_panel,
    actor = "actor", time = "year",
    node_vars = c("polity", "gdp_log"))
    summary(net, tidy = TRUE) |> ggplot(aes(time, value)) +
    geom_line() + facet_wrap(~ metric)

## Multilayer (one call per layer)


    # build as a single multilayer netify by splitting on `channel`
    ml <- netify(slack_long, actor1 = "from", actor2 = "to",
    time = "month", weight = "n_messages",
    layer = "channel")
    # or build per-layer + combine with layer_netify()
    nets <- lapply(channels, function(ch) {
    netify(slack_long[slack_long$channel == ch, ], ...)
    })
    ml <- layer_netify(setNames(nets, channels))

## To ERGM (`statnet`)


    ntwk <- to_statnet(net)
    library(ergm)
    fit <- ergm(ntwk ~ edges + nodematch("gender") + nodecov("gpa"))
    summary(fit)

## To longitudinal ERGM (`tergm 4.x`)


    nl <- to_statnet(longit_net) # network.list
    library(tergm)
    fit <- tergm(nl ~ Form(~ edges + nodematch("gender")) +
    Persist(~ edges + mutual),
    estimate = "CMLE", times = seq_along(nl))

## To AME (Gibbs / MCMC posterior)


    lm_in <- to_lame(net) # auto-picks family from binary/weighted
    library(lame)
    # copy-paste the printed `lm_in$ame_call`:
    fit <- lame::ame(Y = lm_in$Y, Xdyad = lm_in$Xdyad,
    Xrow = lm_in$Xrow, Xcol = lm_in$Xcol,
    family = "binary", nscan = 1000, burn = 500)

## To AME (binary bipartite + ALS + bootstrap uncertainty)


    # fast point estimate + parametric bootstrap CIs in one call
    lm_in <- to_lame(net_bp, fit_method = "als", bootstrap = 200)
    library(lame)
    fit <- lame::ame_als(Y = lm_in$Y, mode = "bipartite",
    family = "binary", bootstrap = 200L)
    confint(fit) # bootstrap CIs
    from_lame_fit(fit, value = "prob") |> plot(style = "heatmap")

## Null-model hypothesis test


    # is observed transitivity surprising vs Erdős-Rényi at this density?
    compare_to_null(net,
    fn = function(n) c(transitivity = summary(n)$transitivity),
    n_sim = 200, model = "erdos_renyi", seed = 1)

## Bootstrap any custom statistic


    my_stat <- function(net) {
    sa <- summary_actor(net, stats = "betweenness")
    c(max_betw = max(sa$betweenness, na.rm = TRUE))
    }
    bootstrap_netlet(net, fn = my_stat, n_boot = 200, seed = 1)

## Very large N (~10K+ actors)

netify stores adjacencies densely, so an N x N matrix consumes 8 \* N^2
bytes (1.7 GB at N = 15K, 20 GB at N = 50K) per snapshot. The recipes
below avoid the dense bottleneck where possible.


    # 1. build directly from an edgelist data.frame -- skip any
    #    pre-built sparse / dense matrix intermediate.
    net <- netify(edges, actor1 = "from", actor2 = "to",
                  weight = "n", symmetric = FALSE)

    # 2. use the fast actor-summary path -- skips closeness /
    #    betweenness / eigen / HITS, which dominate wall-clock at
    #    large N. netify auto-promotes at N >= netify.fast_threshold
    #    (default 1500); tune via options(netify.fast_threshold = ...).
    sa <- summary_actor(net)            # auto-promotes

    # 3. for community detection / shortest paths / clustering, hand
    #    off to igraph. its C backend handles 50K+ nodes comfortably.
    ig <- to_igraph(net)
    cl <- igraph::cluster_louvain(ig)

    # 4. when you need a sparse object for downstream code (e.g.
    #    spectral methods, irlba, glmnet), exit via to_igraph() and
    #    grab the sparse adjacency from there.
    ig <- to_igraph(net)
    M  <- igraph::as_adjacency_matrix(ig, sparse = TRUE)  # requires Matrix

    # 5. if you must accept a pre-built sparse matrix, density < 1
    #    with N > 5,000 aborts to protect against accidental
    #    gigabyte allocations -- pass force_dense = TRUE to override.
    net <- netify(m_sparse, force_dense = TRUE)

## Round-trip to / from other formats


    # to igraph and back
    ig <- to_igraph(net)
    back <- netify(ig) # preserves directedness + weight

    # to long data frame
    edges <- unnetify(net, remove_zeros = TRUE)
    tb <- tibble::as_tibble(net) # same thing, tibble class

    # to base matrix
    M <- as.matrix(net) # strips netify class

## Stacking


    # combine two cross-sec into a 2-period longit
    combined <- bind_netifies(n_2020, n_2021, names = c("2020", "2021"))

## See also

[`netify()`](https://netify-dev.github.io/netify/reference/netify.md)
for the constructor;
[`summary.netify()`](https://netify-dev.github.io/netify/reference/summary.netify.md)
for graph-level stats;
[`plot.netify()`](https://netify-dev.github.io/netify/reference/plot.netify.md)
for visualization;
[`to_statnet()`](https://netify-dev.github.io/netify/reference/netify_to_statnet.md)
/
[`to_amen()`](https://netify-dev.github.io/netify/reference/netify_to_amen.md)
/
[`to_lame()`](https://netify-dev.github.io/netify/reference/netify_to_lame.md)
/
[`to_igraph()`](https://netify-dev.github.io/netify/reference/netify_to_igraph.md)
for handoff to modeling packages;
[`bootstrap_netlet()`](https://netify-dev.github.io/netify/reference/bootstrap_netlet.md)
/
[`compare_to_null()`](https://netify-dev.github.io/netify/reference/compare_to_null.md)
/
[`simulate.netify()`](https://netify-dev.github.io/netify/reference/simulate.netify.md)
for inference; and
[`bind_netifies()`](https://netify-dev.github.io/netify/reference/bind_netifies.md)
for combining.

## Author

Cassy Dorff, Shahryar Minhas
