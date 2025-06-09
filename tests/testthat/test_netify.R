set.seed(6886)
# load relevant datasets from package
data(icews)

# subset to a particular country in event data
nigeria <- icews[icews$verbConf>0,]
nigeria <- nigeria[,c('i', 'j', 'year')]
names(nigeria) <- c('actor1', 'actor2', 'year')

# subset to particular year in icews
icews_10 <- icews[icews$year=='2010', ]

## comparison of netify and get_adjacency
test_that(
	'netify: cross-sectional, undirected, unweighted network', {

        # get_adj call
        nigeria_unweighted <- get_adjacency(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2',
            symmetric = TRUE)

        # netify call
        nigeria_unweighted2  <- netify(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2',
            symmetric = TRUE)

        #
        expect_identical(nigeria_unweighted, nigeria_unweighted2)
    })

test_that(
	'netify: cross-sectional, undirected, weighted network that sums across dyads', {

        # get_adj call
        nigeria_weighted <- get_adjacency(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2',
            symmetric = TRUE, sum_dyads = TRUE)

        # netify call
        nigeria_weighted2  <- netify(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2',
            symmetric = TRUE, sum_dyads=TRUE)

        #
        expect_identical(nigeria_weighted, nigeria_weighted2)
    })

test_that(
	'netify: cross-sectional, directed, weighted network that uses a weight var', {

        # get_adj call
        icews_verbCoop <- get_adjacency(
            icews_10, actor1='i', actor2='j',
            symmetric=FALSE, weight='verbCoop' )

        # netify call
        icews_verbCoop2 <- netify(
            icews_10, actor1='i', actor2='j',
            symmetric=FALSE, weight='verbCoop' )

        #
        expect_identical(icews_verbCoop, icews_verbCoop2)
    })

test_that(
	'netify: cross-sectional, directed, weighted network that uses a weight var 2', {

        # get_adj call
        icews_matlConf <- get_adjacency(
            icews_10, actor1='i', actor2='j',
            symmetric=FALSE, weight='matlConf' )

        # netify call
        icews_matlConf2 <- netify(
            icews_10, actor1='i', actor2='j',
            symmetric=FALSE, weight='matlConf' )

        #
        expect_identical(icews_matlConf, icews_matlConf2)
    })

## comparison of netify and get_adjacency_array
test_that(
    'netify: longitudinal array, directed, weighted network that uses a weight var', {

        # get_adj call
        icews_matlConf <- get_adjacency_array(
            icews, 
            actor1='i', actor2='j', time='year',
            symmetric=FALSE, weight='matlConf' )
        
        # netify call
        icews_matlConf2 <- netify(
            icews, 
            actor1='i', actor2='j', time='year',
            symmetric=FALSE, weight='matlConf',
            output_format='longit_array' )
        
        #
        expect_identical(icews_matlConf, icews_matlConf2)
    })

## comparison of netify and get_adjacency_list
test_that(
    'netify: longitudinal list, undirected, and unweighted network, same actor comp', {

        # get_adj call
        nigeria_unweighted <- get_adjacency_list(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2', time='year',
            symmetric = TRUE, actor_time_uniform = TRUE)

        # netify call
        nigeria_unweighted2 <- netify(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2', time='year',
            symmetric = TRUE, actor_time_uniform = TRUE)

        #
        expect_identical(nigeria_unweighted, nigeria_unweighted2)
    })

test_that(
    'netify: longitudinal list, undirected, and weighted network that sums across dyads, same actor comp', {

        # get_adj call
        nigeria_weighted <- get_adjacency_list(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2', time='year',
            symmetric = TRUE, actor_time_uniform = TRUE, sum_dyads = TRUE)

        # netify call
        nigeria_weighted2 <- netify(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2', time='year',
            symmetric = TRUE, actor_time_uniform = TRUE, sum_dyads = TRUE)

        #
        expect_identical(nigeria_weighted, nigeria_weighted2)
    })

test_that(
    'netify: longitudinal list, undirected, and unweighted network, diff actor comp', {

        # get_adj call
        nigeria_unweighted <- get_adjacency_list(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2', time='year',
            symmetric = TRUE, actor_time_uniform = FALSE)

        # netify call
        nigeria_unweighted2 <- netify(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2', time='year',
            symmetric = TRUE, actor_time_uniform = FALSE)

        #
        expect_identical(nigeria_unweighted, nigeria_unweighted2)
    })

test_that(
    'netify: longitudinal list, undirected, and weighted network that sums across dyads, diff actor comp', {

        # get_adj call
        nigeria_weighted <- get_adjacency_list(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2', time='year',
            symmetric = TRUE, actor_time_uniform = FALSE, sum_dyads = TRUE)

        # netify call
        nigeria_weighted2 <- netify(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2', time='year',
            symmetric = TRUE, actor_time_uniform = FALSE, sum_dyads = TRUE)

        #
        expect_identical(nigeria_weighted, nigeria_weighted2)
    })

test_that(
    'netify: longitudinal list, undirected, and weighted network that sums across dyads, user specified actor comp', {

        # user specified actor comp
        actor_comp <- data.frame(
        actor = c(
            'United States',
            'Afghanistan',
            'United Kingdom',
            'China',
            'Russian Federation',
            'India'
            ), stringsAsFactors = FALSE )
        actor_comp$min_time = c(2001, 2001, 2001, 2001, 2003, 2002)
        actor_comp$max_time = c(2014, 2014, 2014, 2014, 2013, 2010)        

        # get_adj call
        nigeria_weighted_specific_actors <- get_adjacency_list(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2', time='year',
            symmetric = TRUE, actor_pds = actor_comp, sum_dyads = TRUE )

        # netify call
        nigeria_weighted_specific_actors2 <- netify(
            nigeria, 
            actor1= 'actor1', actor2= 'actor2', time='year',
            symmetric = TRUE, actor_pds = actor_comp, sum_dyads = TRUE )

        #
        expect_identical(nigeria_weighted_specific_actors, nigeria_weighted_specific_actors2)
    })
