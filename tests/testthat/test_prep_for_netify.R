# library(netify)
# library(igraph)
# library(testthat)
# devtools::load_all('~/Research/netify_dev/netify')

set.seed(6886)

test_that("prep_for_netify handles igraph objects, single and list", {
  # single igraph
  g <- igraph::make_ring(5)
  igraph::E(g)$myweight <- 1:5
  net_g <- prep_for_netify(g, weight = "myweight")
  expect_s3_class(net_g, "netify")
  expect_equal(attr(net_g, "weight"), "myweight")

  # list of igraph
  g2 <- igraph::make_star(6)
  igraph::E(g2)$edge_color <- "red"
  igraph::E(g2)$myweight <- 6:10
  net_g_list <- prep_for_netify(
    list(g, g2), weight = "myweight")
  expect_s3_class(net_g_list, "netify")
  expect_equal(
    attr(
        net_g_list, "netify_type"), "longit_list")
  # check that adjacency was built for each
  expect_length(net_g_list, 2)
})

test_that("prep_for_netify handles network objects, single and list", {
  # single network
  nw <- network::network(
    matrix(
        rbinom(25, 1, 0.3), 5, 5), directed = FALSE)
  network::set.edge.attribute(
    nw, "myweight", 
    1:network::network.size(nw))
  net_nw <- prep_for_netify(nw, weight="myweight")
  expect_s3_class(net_nw, "netify")
  expect_equal(attr(net_nw, "weight"), "myweight")

  # list of networks
  nw2 <- network::network(
    matrix(
        rbinom(36, 1, 0.5), 6, 6), directed = TRUE)
  network::set.edge.attribute(
    nw2, "myweight", 
    1:network::network.size(nw))
  net_nw_list <- prep_for_netify(
    list(nw, nw2), weight="myweight")
  expect_s3_class(net_nw_list, "netify")
  expect_equal(attr(net_nw_list, "netify_type"), "longit_list")
  expect_length(net_nw_list, 2)
})

# library(netify)

# # load data
# data(icews)

# # cross-sectional case
# icews_10 <- icews[icews$year == 2010,]

# # create netify object
# dvars = c('matlCoop', 'verbConf', 'matlConf')
# nvars = c('i_polity2', 'i_log_gdp', 'i_log_pop')
# verbCoop_net = netify(
#   icews_10,
#   actor1 = 'i', actor2 = 'j',
#   symmetric = FALSE,
#   weight = 'verbCoop',
#   dyad_vars = dvars,
#   dyad_vars_symmetric = rep(FALSE, length(dvars)),
#   nodal_vars = nvars
# )

# # convert to igraph object
# igrph <- prep_for_igraph(verbCoop_net)
# igrph

# # longitudinal case
# verbCoop_longit_net = netify(
#   icews,
#   actor1 = 'i', actor2 = 'j', time = 'year',
#   symmetric = FALSE,
#   weight = 'verbCoop',
#   dyad_vars = dvars,
#   dyad_vars_symmetric = rep(FALSE, length(dvars)),
#   nodal_vars = nvars
# )

# # convert to igraph object
# igrph_longit <- prep_for_igraph(verbCoop_longit_net)

# # output in the longitudinal case is 
# # a list of igraph objects
# class(igrph_longit)
# names(igrph_longit)
# igrph_longit[['2002']]

# # cross-sectional case
# icews_10 <- icews[icews$year == 2010,]

# # create netify object
# dvars = c('matlCoop', 'verbConf', 'matlConf')
# nvars = c('i_polity2', 'i_log_gdp', 'i_log_pop')
# verbCoop_net = netify(
#   icews_10,
#   actor1 = 'i', actor2 = 'j',
#   symmetric = FALSE,
#   weight = 'verbCoop',
#   dyad_vars = dvars,
#   dyad_vars_symmetric = rep(FALSE, length(dvars)),
#   nodal_vars = nvars
# )

# ## prep a regular mat 
# imat = igraph::as_adjacency_matrix(igrph, sparse = FALSE)

# ## prep a regular list of mats
# ilist = lapply(igrph_longit, function(x){
#     igraph::as_adjacency_matrix(x, sparse = FALSE)})

# ## turn into arr
# iarr = array(unlist(ilist), dim = c(nrow(imat), ncol(imat), length(ilist)))

# # convert to a statnet network object
# ntwk <- prep_for_statnet(verbCoop_net)
# ntwk

# # longitudinal case
# verbCoop_longit_net = netify(
#   icews,
#   actor1 = 'i', actor2 = 'j', time = 'year',
#   symmetric = FALSE,
#   weight = 'verbCoop',
#   dyad_vars = dvars,
#   dyad_vars_symmetric = rep(FALSE, length(dvars)),
#   nodal_vars = nvars
# )

# # convert to a statnet network object
# ntwk_longit <- prep_for_statnet(verbCoop_longit_net)

# # output in the longitudinal case is 
# # a list of statnet network objects
# class(ntwk_longit)
# names(ntwk_longit)
# ntwk_longit[['2002']]
