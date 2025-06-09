set.seed(6886)

# library(netify)
# library(testthat)

################################################
# load relevant datasets from package
data(icews)

# create cross-sectional versions
icews_10 <- icews[icews$year=='2010', ]

# create individual layer networks for testing
# Cross-sectional networks
icews_verbCoop_10 <- netify(
    icews_10, actor1='i', actor2='j', 
    symmetric=FALSE, weight='verbCoop' )
icews_matlCoop_10 <- netify(
    icews_10, actor1='i', actor2='j', 
    symmetric=FALSE, weight='matlCoop' )
icews_verbConf_10 <- netify(
    icews_10, actor1='i', actor2='j', 
    symmetric=FALSE, weight='verbConf' )
icews_matlConf_10 <- netify(
    icews_10, actor1='i', actor2='j', 
    symmetric=FALSE, weight='matlConf' )

# Longitudinal list format networks
icews_verbCoop_l <- netify(
    icews, actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='verbCoop', output_format='longit_list')
icews_matlCoop_l <- netify(
    icews, actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='matlCoop', output_format='longit_list')
icews_verbConf_l <- netify(
    icews, actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='verbConf', output_format='longit_list')
icews_matlConf_l <- netify(
    icews, actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='matlConf', output_format='longit_list')

# Longitudinal array format networks
icews_verbCoop_a <- netify(
    icews, actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='verbCoop', output_format='longit_array')
icews_matlCoop_a <- netify(
    icews, actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='matlCoop', output_format='longit_array')
icews_verbConf_a <- netify(
    icews, actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='verbConf', output_format='longit_array')
icews_matlConf_a <- netify(
    icews, actor1='i', actor2='j', time='year',
    symmetric=FALSE, weight='matlConf', output_format='longit_array')

# Networks with attributes for testing attribute handling
icews_verbCoop_attrs <- netify(
    icews_10, actor1='i', actor2='j', 
    symmetric=FALSE, weight='verbCoop',
    nodal_vars = c('i_polity2', 'i_log_gdp'),
    dyad_vars = c('matlCoop', 'verbConf'))

icews_matlCoop_attrs <- netify(
    icews_10, actor1='i', actor2='j', 
    symmetric=FALSE, weight='matlCoop',
    nodal_vars = c('i_polity2', 'i_log_gdp'),
    dyad_vars = c('verbCoop', 'matlConf'))
################################################

################################################
# Test basic functionality for cross-sectional networks
test_that(
    'layer_netify: cross-sectional basic functionality', {
        
        # Create multilayer network
        multi_net <- layer_netify(
            netlet_list = list(icews_verbCoop_10, icews_matlCoop_10),
            layer_labels = c('Verbal', 'Material')
        )
        
        # Check class
        expect_s3_class(multi_net, 'netify')
        
        # Check dimensions
        expect_equal(length(dim(multi_net)), 3)
        expect_equal(dim(multi_net)[3], 2)
        
        # Check layer labels
        expect_equal(attr(multi_net, 'layers'), c('Verbal', 'Material'))
        
        # Check that netify_type is still cross_sec
        expect_equal(attr(multi_net, 'netify_type'), 'cross_sec')
    })

test_that(
    'layer_netify: cross-sectional with 4 layers', {
        
        # Create multilayer network with 4 layers
        multi_net <- layer_netify(
            netlet_list = list(
                icews_verbCoop_10, icews_matlCoop_10,
                icews_verbConf_10, icews_matlConf_10
            ),
            layer_labels = c('verbCoop', 'matlCoop', 'verbConf', 'matlConf')
        )
        
        # Check dimensions
        expect_equal(dim(multi_net)[3], 4)
        
        # Check all layer labels
        expect_equal(
            attr(multi_net, 'layers'), 
            c('verbCoop', 'matlCoop', 'verbConf', 'matlConf')
        )
        
        # Check weight attribute combines all weights
        expect_true(grepl('verbCoop', attr(multi_net, 'weight')))
        expect_true(grepl('matlConf', attr(multi_net, 'weight')))
    })

################################################
# Test longitudinal list format
test_that(
    'layer_netify: longitudinal list basic functionality', {
        
        # Create multilayer network
        multi_net_l <- layer_netify(
            netlet_list = list(icews_verbCoop_l, icews_matlCoop_l),
            layer_labels = c('Verbal', 'Material')
        )
        
        # Check it's still a list
        expect_type(multi_net_l, 'list')
        expect_s3_class(multi_net_l, 'netify')
        
        # Check each time period is now 3D
        first_period <- multi_net_l[[1]]
        expect_equal(length(dim(first_period)), 3)
        expect_equal(dim(first_period)[3], 2)
        
        # Check netify_type
        expect_equal(attr(multi_net_l, 'netify_type'), 'longit_list')
        
        # Check time periods preserved
        expect_equal(length(multi_net_l), 13)  # 13 years in icews data
        expect_equal(names(multi_net_l), as.character(2002:2014))
    })

test_that(
    'layer_netify: longitudinal list with 4 layers', {
        
        # Create multilayer network
        multi_net_l <- layer_netify(
            netlet_list = list(
                icews_verbCoop_l, icews_matlCoop_l,
                icews_verbConf_l, icews_matlConf_l
            ),
            layer_labels = c('verbCoop', 'matlCoop', 'verbConf', 'matlConf')
        )
        
        # Check dimensions of first time period
        expect_equal(dim(multi_net_l[[1]])[3], 4)
        
        # Check layer labels preserved in each time period
        expect_equal(
            dimnames(multi_net_l[[1]])[[3]], 
            c('verbCoop', 'matlCoop', 'verbConf', 'matlConf')
        )
    })

################################################
# Test longitudinal array format
test_that(
    'layer_netify: longitudinal array basic functionality', {
        
        # Create multilayer network
        multi_net_a <- layer_netify(
            netlet_list = list(icews_verbCoop_a, icews_matlCoop_a),
            layer_labels = c('Verbal', 'Material')
        )
        
        # Check it's a 4D array
        expect_equal(length(dim(multi_net_a)), 4)
        expect_equal(dim(multi_net_a)[3], 2)  # 2 layers
        expect_equal(dim(multi_net_a)[4], 13)  # 13 time periods
        
        # Check netify_type
        expect_equal(attr(multi_net_a, 'netify_type'), 'longit_array')
        
        # Check dimnames
        expect_equal(dimnames(multi_net_a)[[3]], c('Verbal', 'Material'))
        expect_equal(dimnames(multi_net_a)[[4]], as.character(2002:2014))
    })

################################################
# Test automatic layer labeling
test_that(
    'layer_netify: automatic layer labeling', {
        
        # Test with unnamed list (should generate layer1, layer2, etc.)
        unnamed_list <- list(icews_verbCoop_10, icews_matlCoop_10)
        multi_net <- layer_netify(unnamed_list)
        expect_equal(attr(multi_net, 'layers'), c('layer1', 'layer2'))
        
        # Test with explicit NULL layer_labels
        multi_net2 <- layer_netify(
            netlet_list = list(icews_verbCoop_10, icews_matlCoop_10),
            layer_labels = NULL
        )
        expect_equal(attr(multi_net2, 'layers'), c('layer1', 'layer2'))
    })

################################################

################################################
# Test attribute handling
test_that(
    'layer_netify: identical attributes preserved', {
        
        # Create multilayer with identical attributes
        multi_net <- layer_netify(
            list(icews_verbCoop_attrs, icews_matlCoop_attrs),
            layer_labels = c('Verbal', 'Material')
        )
        
        # Check nodal data preserved
        expect_false(is.null(attr(multi_net, 'nodal_data')))
        
        # Check dyad data preserved
        expect_false(is.null(attr(multi_net, 'dyad_data')))
    })

################################################
# Test weight handling
test_that(
    'layer_netify: weight attributes correctly combined', {
        
        multi_net <- layer_netify(
            list(icews_verbCoop_10, icews_matlCoop_10,
                 icews_verbConf_10, icews_matlConf_10)
        )
        
        # Check combined weight string
        weight_str <- attr(multi_net, 'weight')
        expect_equal(weight_str, 'verbCoop, matlCoop, verbConf, matlConf')
        
        # Check weight_binary vector
        expect_length(attr(multi_net, 'weight_binary'), 4)
        expect_type(attr(multi_net, 'weight_binary'), 'logical')
    })

################################################
# Test subsetting multilayer networks
test_that(
    'layer_netify: subset multilayer networks', {
        
        # Create multilayer
        multi_net <- layer_netify(
            list(icews_verbCoop_10, icews_matlCoop_10,
                 icews_verbConf_10, icews_matlConf_10),
            layer_labels = c('verbCoop', 'matlCoop', 'verbConf', 'matlConf')
        )
        
        # Subset to specific layers
        subset_multi <- subset_netify(
            multi_net,
            layers = c('verbCoop', 'matlCoop')
        )
        
        # Check it's still multilayer with 2 layers
        expect_equal(dim(subset_multi)[3], 2)
        expect_equal(attr(subset_multi, 'layers'), c('verbCoop', 'matlCoop'))
        
        # Subset to single layer (should become regular 2D)
        single_layer <- subset_netify(
            multi_net,
            layers = 'verbCoop'
        )
        
        # Check it's now 2D
        expect_equal(length(dim(single_layer)), 2)
        expect_equal(attr(single_layer, 'layers'), 'verbCoop')
    })

################################################
# Test data integrity
test_that(
    'layer_netify: data integrity maintained', {
        
        # Create multilayer
        multi_net <- layer_netify(
            list(icews_verbCoop_10, icews_matlCoop_10),
            layer_labels = c('Verbal', 'Material')
        )
        
        # Extract raw data
        raw_multi <- get_raw(multi_net)
        raw_verb <- get_raw(icews_verbCoop_10)
        raw_matl <- get_raw(icews_matlCoop_10)
        
        # Check that data matches
        expect_equal(raw_multi[,,1], raw_verb)
        expect_equal(raw_multi[,,2], raw_matl)
        
        # Check actors preserved
        expect_equal(dimnames(raw_multi)[[1]], rownames(raw_verb))
        expect_equal(dimnames(raw_multi)[[2]], colnames(raw_verb))
    })

################################################