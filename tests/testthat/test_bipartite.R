# library(testthat)
# library(netify)
# devtools::load_all('~/Research/netify_dev/netify')

set.seed(6886)

test_that(
    'get_adjacency, bipartite: cross-sectional, asymmetric, weighted network', {

    # create data
    ar <- letters[1:3] ; nr <- length(ar)
    ac <- letters[23:26] ; nc <- length(ac)
    asym_weight_df <- expand.grid(
            actor1=ar,actor2=ac,stringsAsFactors=FALSE)
    asym_weight_df$value <- rnorm(nrow(asym_weight_df))
    asym_weight_df <- asym_weight_df[asym_weight_df$actor1!=asym_weight_df$actor2,]

    # create matrix
    asym_weight_matrix <- matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac))
    for(ii in 1:nrow(asym_weight_df)){
        asym_weight_matrix[asym_weight_df$actor1[ii],asym_weight_df$actor2[ii]] = asym_weight_df$value[ii] }

    # get adjacency
    a_matrix <- get_adjacency(
        dyad_data=asym_weight_df,
        actor1='actor1', actor2='actor2',
        weight='value', symmetric=FALSE,
        mode='bipartite' )

    # the test
    expect_identical(get_raw(a_matrix), asym_weight_matrix)
    })

test_that(
  'get_adjacency, bipartite: cross-sectional, asymmetric, weighted network, sum dyads', {

    # create data
    ar <- letters[1:7] ; nr <- length(ar)
    ac <- letters[23:26] ; nc <- length(ac)
    asym_weight_df <- expand.grid(
            actor1=ar,actor2=ac,stringsAsFactors=FALSE)
    asym_weight_df$value <- rnorm(nrow(asym_weight_df))            
    asym_weight_df <- asym_weight_df[asym_weight_df$actor1!=asym_weight_df$actor2,]
    asym_weight_agg_df <- aggregate(
        value ~ actor1 + actor2, data = asym_weight_df, sum)

    # create matrix
    asym_weight_matrix <- matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac))
    for(ii in 1:nrow(asym_weight_df)){
        old_edge_val = asym_weight_matrix[asym_weight_agg_df$actor1[ii],asym_weight_agg_df$actor2[ii]]
        new_edge_val = asym_weight_agg_df$value[ii] 
        asym_weight_matrix[asym_weight_agg_df$actor1[ii],asym_weight_agg_df$actor2[ii]] = old_edge_val + new_edge_val }

    # get adjacency
    a_matrix <- get_adjacency(
        dyad_data=asym_weight_df,
        actor1='actor1', actor2='actor2',
        weight='value', symmetric=FALSE,
        sum_dyads=TRUE,
        mode = 'bipartite' )

    # the test
    expect_identical(get_raw(a_matrix), asym_weight_matrix)
  })

test_that(
	'get_adjacency, bipartite: cross-sectional, asymmetric, non-weighted network', {

  # create data that is cross-sectional, asymmetric, and non-weighted
    ar <- letters[1:7] ; nr <- length(ar)
    ac <- letters[23:26] ; nc <- length(ac)
    asym_non_weight_df <- expand.grid(
            actor1=ar,actor2=ac,stringsAsFactors=FALSE)
    asym_non_weight_df$value <- rbinom(nrow(asym_non_weight_df), 1, .5)
    asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$actor1!=asym_non_weight_df$actor2,]

    # filter out zero responses, equivalent to an event dataset
    asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$value>0,]

    # create matrix
    asym_non_weight_matrix <- matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac))
    for(ii in 1:nrow(asym_non_weight_df)){
        asym_non_weight_matrix[asym_non_weight_df$actor1[ii],asym_non_weight_df$actor2[ii]] = asym_non_weight_df$value[ii] }

    # get adjacency
    a_matrix <- get_adjacency(
        dyad_data=asym_non_weight_df,
        actor1='actor1', actor2='actor2',
        weight=NULL, symmetric=FALSE,
        mode='bipartite' )

    # the test
    expect_identical(get_raw(a_matrix), asym_non_weight_matrix)
    })

test_that(
	'get_adjacency, bipartite: cross-sectional, asymmetric, non-weighted network, sum dyads', {

    # create data that is cross-sectional, asymmetric, and non-weighted
    ar <- letters[1:7] ; nr <- length(ar)
    ac <- letters[23:26] ; nc <- length(ac)    
    asym_non_weight_df <- expand.grid(
        actor1=ar,actor2=ac,stringsAsFactors=FALSE)
    asym_non_weight_df$value <- rbinom(nrow(asym_non_weight_df), 1, .7)
    asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$actor1!=asym_non_weight_df$actor2,]
    asym_non_weight_df <- rbind(
        asym_non_weight_df, 
        data.frame('actor1'='a','actor2'='z','value'=1))

    # filter out zero responses, equivalent to an event dataset
    asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$value>0,]

    # create matrix
    asym_non_weight_matrix <- matrix(
        0, nrow=nr, ncol=nc, dimnames=list(ar, ac))
    for(ii in 1:nrow(asym_non_weight_df)){
        old_edge_val = asym_non_weight_matrix[asym_non_weight_df$actor1[ii],asym_non_weight_df$actor2[ii]]
        new_edge_val = asym_non_weight_df$value[ii]
        asym_non_weight_matrix[asym_non_weight_df$actor1[ii],asym_non_weight_df$actor2[ii]] = old_edge_val + new_edge_val }

    # get adjacency
    a_matrix <- get_adjacency(
        dyad_data=asym_non_weight_df,
        actor1='actor1', actor2='actor2',
        sum_dyads = TRUE,
        weight=NULL, symmetric=FALSE,
        mode='bipartite' )

    # the test
    expect_identical(get_raw(a_matrix), asym_non_weight_matrix)
})

# cross-sectional, symmetric weighted network, not doing other tests
# because we force symm to FALSE for the purpose of matrix construction
test_that(
	'get_adjacency, bipartite: cross-sectional, symmetric, weighted network', {

    # create symmetric dat
    sym_weight_df <- data.frame(
        actor1 = c('a', 'b', 'c', 'c'),
        actor2 = c('z', 'z', 'x', 'y'),
        stringsAsFactors = FALSE)
    sym_weight_df$value <- rnorm(nrow(sym_weight_df))

    # create matrix
    ar <- sort(unique(sym_weight_df$actor1)) ; nr <- length(ar)
    ac <- sort(unique(sym_weight_df$actor2)) ; nc <- length(ac)
    sym_weight_matrix <- matrix(
        0, nrow=nr, ncol=nc, dimnames=list(ar, ac))
    for(ii in 1:nrow(sym_weight_df)){
        sym_weight_matrix[sym_weight_df$actor1[ii],sym_weight_df$actor2[ii]] = sym_weight_df$value[ii] }

    # get adjacency
    a_matrix <- get_adjacency(
        dyad_data=sym_weight_df,
        actor1='actor1', actor2='actor2',
        weight='value', symmetric=FALSE,
        sum_dyads = FALSE,
        mode = 'bipartite' )

    # the test
    expect_identical(get_raw(a_matrix), sym_weight_matrix)
})

# longitudinal, asymmetric weighted network
test_that(
	'get_adjacency_list, bipartite: longitudinal, asymmetric weighted network', {

	# create symmetric long data
    ar <- letters[1:7] ; nr <- length(ar)
    ac <- letters[23:26] ; nc <- length(ac)
	long_asym_weight_df <- expand.grid(actor1=ar,actor2=ac, time = 1:10, stringsAsFactors=FALSE)
    long_asym_weight_df$value <- rnorm(nrow(long_asym_weight_df))
    long_asym_weight_df <- long_asym_weight_df[long_asym_weight_df$actor1!=long_asym_weight_df$actor2,]

    # create matrix
    long_asym_weight_matrix <- matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac))

    # create an empty list object to store matrices
    times <- sort(unique(long_asym_weight_df[,"time"]))
    result <- lapply(times, function(t){

        # slice to relevant time period
        slice <- long_asym_weight_df[long_asym_weight_df[,"time"] == t,]

        # fill in
            for(ii in 1:nrow(slice)){
            long_asym_weight_matrix[slice$actor1[ii],slice$actor2[ii]] = slice$value[ii] }

        #
        return(long_asym_weight_matrix) })
    names(result) <- times

    # get adjacency
    a_matrix <- get_adjacency_list(
        dyad_data=long_asym_weight_df,
        actor1='actor1', actor2='actor2', time='time',
            actor_time_uniform=TRUE, actor_pds=NULL,
        weight='value', symmetric=FALSE,
        mode='bipartite', missing_to_zero=TRUE)

        # the test
        expect_identical(get_raw(a_matrix), result)
})

# longitudinal, asymmetric non-weighted network
test_that(
	'get_adjacency_list, bipartite: longitudinal, asymmetric non-weighted network', {

 	# create df
    ar <- letters[1:7] ; nr <- length(ar)
    ac <- letters[23:26] ; nc <- length(ac)    
    long_asym_non_weight_df <- expand.grid(actor1=ar,actor2=ac, time = 1:3, stringsAsFactors=FALSE)
    long_asym_non_weight_df$value <- rbinom(nrow(long_asym_non_weight_df), 1, 0.5)
    long_asym_non_weight_df <- long_asym_non_weight_df[long_asym_non_weight_df$actor1!=long_asym_non_weight_df$actor2,]
    long_asym_non_weight_df <- long_asym_non_weight_df[long_asym_non_weight_df$value>0,]

    # create an empty list object to store matrices
    result <- list()
    times = 1:3
    for(i in times) {
        # create new matrix at the beginning of each loop
        long_asym_non_weight_mat <- matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac))
        slice <- long_asym_non_weight_df[long_asym_non_weight_df[,"time"] == i,]
        if(dim(slice)[1] != 0){
            for(ii in 1:nrow(slice)){
            long_asym_non_weight_mat[slice$actor1[ii],slice$actor2[ii]] = slice$value[ii] }}
        result[[i]] <- long_asym_non_weight_mat }
    names(result) <- times

    # get adjacency
    a_matrix <- get_adjacency_list(
        dyad_data=long_asym_non_weight_df,
        actor1='actor1', actor2='actor2', time='time',
            actor_time_uniform=TRUE, actor_pds=NULL,
        weight=NULL, symmetric=FALSE,
        mode='bipartite', missing_to_zero=TRUE)

    # the test
    expect_identical(get_raw(a_matrix), result)
})


test_that('add_nodal, bipartite: cross_sec', {

    # create data
    ar = c('a', 'b') ; nr = length(ar)
    ac = c('x', 'y', 'z') ; nc = length(ac)
    dd <- expand.grid(actor1=ar, actor2=ac)
    dd$actor1 <- as.character(dd$actor1)
    dd$actor2 <- as.character(dd$actor2)
    dd$value <- rbinom(nrow(dd), 1, 0.5)
    
    # nodal_data
    actors <- c(ar, ac)
    node_data = data.frame(
        actor = actors,
        var1 = rnorm(length(actors)),
        r1 = rnorm(length(actors)),
        c1 = rnorm(length(actors)),
        stringsAsFactors = FALSE )
    node_data$r1[node_data$actor %in% ac] <- NA
    node_data$c1[node_data$actor %in% ar] <- NA

    # create adj
    net_adj <- netify(
        dd, actor1='actor1', actor2='actor2', weight='value', mode='bipartite')

    # add_node_vars()
    net_adj = add_node_vars(
        net_adj,
        node_data,
        actor = 'actor'
    )

    # the test
    expect_identical(attr(net_adj, 'nodal_data'), node_data)
})

test_that('add_nodal in stages, bipartite: cross_sec', {

    # create data
    ar = c('a', 'b') ; nr = length(ar)
    ac = c('x', 'y', 'z') ; nc = length(ac)
    dd <- expand.grid(actor1=ar, actor2=ac)
    dd$actor1 <- as.character(dd$actor1)
    dd$actor2 <- as.character(dd$actor2)
    dd$value <- rbinom(nrow(dd), 1, 0.5)
    
    # nodal_data
    actors <- c(ar, ac)
    node_data = data.frame(
        actor = actors,
        var1 = rnorm(length(actors)),
        r1 = rnorm(length(actors)),
        c1 = rnorm(length(actors)),
        stringsAsFactors = FALSE )
    node_data$r1[node_data$actor %in% ac] <- NA
    node_data$c1[node_data$actor %in% ar] <- NA

    # row data
    row_col_data <- node_data[,c('actor', 'var1')]
    row_data <- node_data[
        node_data$actor %in% ar,-c(2, ncol(node_data))]
    col_data <- node_data[
        node_data$actor %in% ac,-c(2,3)]

    # create adj
    net_adj <- netify(
        dd, actor1='actor1', actor2='actor2', weight='value', mode='bipartite')

    # add row and then col
    net_adj = add_node_vars( net_adj, row_col_data, actor = 'actor' )
    net_adj = add_node_vars( 
        net_adj, row_data, actor = 'actor',
        replace_existing = TRUE )
    net_adj = add_node_vars( 
        net_adj, col_data, actor = 'actor', 
        replace_existing = TRUE ) 

    # the test
    expect_identical(attr(net_adj, 'nodal_data'), node_data)
})

test_that('add_nodal, bipartite: longit_list, actor uniform FALSE', {

    # create data
    ar = c('a', 'b', 'c', 'd', 'e') ; nr = length(ar)
    ac = c('x', 'y', 'z') ; nc = length(ac)
    t = 1:3 ; nt = length(t)
    dd <- expand.grid(actor1=ar, actor2=ac, time=t)
    dd$actor1 <- as.character(dd$actor1)
    dd$actor2 <- as.character(dd$actor2)
    dd$value <- rbinom(nrow(dd), 1, 0.5)

    # create fake nodal data
    actors <- c(ar, ac)
    node_data = data.frame(
        actor = rep(actors, each=3),
        time = rep(1:3, length(actors)),
        stringsAsFactors = FALSE )
    node_data$r1 = rnorm(nrow(node_data))
    node_data$r1[node_data$actor %in% ac] <- NA
    node_data$r2 = rnorm(nrow(node_data))
    node_data$r2[node_data$actor %in% ac] <- NA    
    node_data$c1 = rnorm(nrow(node_data))
    node_data$c1[node_data$actor %in% ar] <- NA
    node_data$c2 = rnorm(nrow(node_data))
    node_data$c2[node_data$actor %in% ar] <- NA

    # get rid of actor e in time 3 and actor z in time 1
    node_data <- node_data[!(node_data$actor == 'e' & node_data$time == 3),]
    node_data <- node_data[!(node_data$actor == 'z' & node_data$time == 1),]
    dd <- dd[!(dd$actor1 == 'e' & dd$time == 3),]
    dd <- dd[!(dd$actor2 == 'z' & dd$time == 1),]

    # cleanup
    rownames(node_data) <- NULL

    # create netify obj
    net_adj <- netify(
        dd, 
        actor1='actor1', 
        actor2='actor2',
        time='time',
        actor_time_uniform=FALSE,
        weight='value', 
        mode='bipartite'
        )

    # add_node_vars()
    net_adj = add_node_vars(
        net_adj,
        node_data,
        actor = 'actor',
        time = 'time'
    )

    # the test
    expect_identical(
        attr(net_adj, 'nodal_data'), 
        node_data)
})

test_that('add_nodal, bipartite: longit_list, actor uniform TRUE', {

    # create data
    ar = c('a', 'b', 'c', 'd', 'e') ; nr = length(ar)
    ac = c('x', 'y', 'z') ; nc = length(ac)
    t = 1:3 ; nt = length(t)
    dd <- expand.grid(actor1=ar, actor2=ac, time=t)
    dd$actor1 <- as.character(dd$actor1)
    dd$actor2 <- as.character(dd$actor2)
    dd$value <- rnorm(nrow(dd))

    # create fake nodal data
    actors <- c(ar, ac)
    node_data = data.frame(
        actor = rep(actors, each=3),
        time = rep(1:3, length(actors)),
        stringsAsFactors = FALSE )
    node_data$r1 = rnorm(nrow(node_data))
    node_data$r1[node_data$actor %in% ac] <- NA
    node_data$r2 = rnorm(nrow(node_data))
    node_data$r2[node_data$actor %in% ac] <- NA    
    node_data$c1 = rnorm(nrow(node_data))
    node_data$c1[node_data$actor %in% ar] <- NA
    node_data$c2 = rnorm(nrow(node_data))
    node_data$c2[node_data$actor %in% ar] <- NA

    # cleanup
    rownames(node_data) <- NULL

    # create netify obj
    net_adj <- netify(
        dd, 
        actor1='actor1', 
        actor2='actor2',
        time='time',
        weight='value', 
        mode='bipartite'
        )

    # add_node_vars()
    net_adj = add_node_vars(
        net_adj,
        node_data,
        actor = 'actor',
        time = 'time'
    )

    # the test
    expect_identical(
        attr(net_adj, 'nodal_data'), 
        node_data)
})

test_that(
	'add_dyad, bipartite: no time ID, weighted, asymmetric dyad vars', {

	# create fake dyad data for cross-sectional case
    ar = letters[1:3] ; nr = length(ar)
    ac = letters[22:26] ; nc = length(ac)
	fake_dyads <- expand.grid( actor1 = ar, actor2 = ac )
    fake_dyads$dv = rbinom(nrow(fake_dyads), 1, 0.5)
	fake_dyads$var1 <- rnorm(nrow(fake_dyads))
	fake_dyads$var2 <- rnorm(nrow(fake_dyads))
	fake_dyads$var3 <- rnorm(nrow(fake_dyads))
	fake_dyads$var4 <- rnorm(nrow(fake_dyads))
	fake_dyads$year <- 2312
	fake_dyads$actor1 = as.character(fake_dyads$actor1)
	fake_dyads$actor2 = as.character(fake_dyads$actor2)
	fake_dyads <- fake_dyads[fake_dyads$actor1!=fake_dyads$actor2,]

	# convert to conflictNet object
	a_matrix <- get_adjacency(
	  dyad_data=fake_dyads,
	  actor1='actor1', actor2='actor2', symmetric=TRUE,
	  weight='dv',
	  mode='bipartite' )

	# add dyad variables in fake data as a dyadic attribute
	a_matrix = add_dyad_vars(
	  a_matrix, fake_dyads,
	  'actor1', 'actor2', NULL,
	  c('var1', 'var2', 'var3', 'var4'),
	  c(FALSE, FALSE, FALSE, FALSE))

	# manually convert dyadic variables into NEW list of matrices format
	# Initialize with 0s to match get_matrix behavior when missing_to_zero=TRUE
	manualList <- list(
	  "1" = list(
	    var1 = matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac)),
	    var2 = matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac)),
	    var3 = matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac)),
	    var4 = matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac))
	  )
	)
	
	# fill in matrices (asymmetric bipartite case)
	for( v in paste0('var', 1:4) ){
	  for( ii in 1:nrow(fake_dyads) ){
	    a1 <- fake_dyads$actor1[ii]
	    a2 <- fake_dyads$actor2[ii]
	    val <- fake_dyads[ii,v]
	    manualList[["1"]][[v]][a1, a2] <- val
	  }
	}

  # test if identical
	expect_equal( attributes(a_matrix)$dyad_data , manualList )
})

test_that(
	'add_dyad, bipartite: no time ID, unweighted, asymmetric dyad vars', {

	# create fake dyad data for cross-sectional case
    ar = letters[1:3] ; nr = length(ar)
    ac = letters[22:26] ; nc = length(ac)
	fake_dyads <- expand.grid( actor1 = ar, actor2 = ac )
    set.seed(6886)
    fake_dyads$dv = rbinom(nrow(fake_dyads), 1, 0.8)
	fake_dyads$var1 <- rnorm(nrow(fake_dyads))
	fake_dyads$var2 <- rnorm(nrow(fake_dyads))
	fake_dyads$var3 <- rnorm(nrow(fake_dyads))
	fake_dyads$var4 <- rnorm(nrow(fake_dyads))
	fake_dyads$year <- 2312
	fake_dyads$actor1 = as.character(fake_dyads$actor1)
	fake_dyads$actor2 = as.character(fake_dyads$actor2)
	fake_dyads <- fake_dyads[fake_dyads$actor1!=fake_dyads$actor2,]
    fake_dyads = fake_dyads[fake_dyads$dv==1,]

	# convert to conflictNet object
	a_matrix <- get_adjacency(
	  dyad_data=fake_dyads,
	  actor1='actor1', actor2='actor2', symmetric=TRUE,
	  weight=NULL,
	  mode='bipartite' )

	# add dyad variables in fake data as a dyadic attribute
	a_matrix = add_dyad_vars(
	  a_matrix, fake_dyads,
	  'actor1', 'actor2', NULL,
	  c('var1', 'var2', 'var3', 'var4'),
	  c(FALSE, FALSE, FALSE, FALSE))

	# manually convert dyadic variables into NEW list of matrices format
	# Initialize with 0s to match get_matrix behavior when missing_to_zero=TRUE
	manualList <- list(
	  "1" = list(
	    var1 = matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac)),
	    var2 = matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac)),
	    var3 = matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac)),
	    var4 = matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac))
	  )
	)
	
	# fill in matrices (asymmetric bipartite case - only observed edges)
	for( v in paste0('var', 1:4) ){
	  for( ii in 1:nrow(fake_dyads) ){
	    a1 <- fake_dyads$actor1[ii]
	    a2 <- fake_dyads$actor2[ii]
	    val <- fake_dyads[ii,v]
	    manualList[["1"]][[v]][a1, a2] <- val
	  }
	}

  # test if identical
	expect_equal( attributes(a_matrix)$dyad_data , manualList )
})

test_that(
  'add_dyad, bipartite: supplied time ID, asymmetric dyad vars', {

	# create fake dyad data for longitudinal case
    ar = letters[1:3] ; nr = length(ar)
    ac = letters[22:26] ; nc = length(ac)
	fake_dyads <- expand.grid( 
        actor1 = ar, actor2 = ac, time=1:2 )
    fake_dyads$dv <- rnorm(nrow(fake_dyads))    
    fake_dyads$var1 <- rnorm(nrow(fake_dyads))
    fake_dyads$var2 <- rnorm(nrow(fake_dyads))
    fake_dyads$var3 <- rnorm(nrow(fake_dyads))
    fake_dyads$var4 <- rnorm(nrow(fake_dyads))
	fake_dyads$actor1 = as.character(fake_dyads$actor1)
	fake_dyads$actor2 = as.character(fake_dyads$actor2)
	fake_dyads <- fake_dyads[fake_dyads$actor1!=fake_dyads$actor2,]

	# convert to conflictNet object
	a_matrix <- netify(
	  fake_dyads,
	  actor1='actor1', actor2='actor2', time='time',
	  symmetric=TRUE,
	  weight='dv',
	  mode='bipartite' )

	# add dyad variables in fake data as a dyadic attribute
	a_matrix = add_dyad_vars(
	  a_matrix, fake_dyads,
	  'actor1', 'actor2', 'time',
		c('var1', 'var2', 'var3', 'var4'),
		c(FALSE, FALSE, FALSE, FALSE))

	# manually convert dyadic variables into NEW list of matrices format
	manualList <- lapply( unique(fake_dyads$time), function(timePd){

	  # create list of individual matrices for this time period
	  # Initialize with 0s to match get_matrix behavior when missing_to_zero=TRUE
	  time_matrices <- list(
	    var1 = matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac)),
	    var2 = matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac)),
	    var3 = matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac)),
	    var4 = matrix(0, nrow=nr, ncol=nc, dimnames=list(ar, ac))
	  )

	  # subset inputted data by timepd
	  slice <- fake_dyads[fake_dyads$time==timePd,]

	  # fill in via for loop (asymmetric bipartite case)
	  for( v in paste0('var', 1:4) ){
	    for( ii in 1:nrow(slice) ){
	      a1 <- slice$actor1[ii]
	      a2 <- slice$actor2[ii]
	      val <- slice[ii,v]
	      time_matrices[[v]][a1, a2] <- val
	    }
	  }

	  return(time_matrices)
	})
	names(manualList) <- as.character(unique(fake_dyads$time))

    # test if identical
    expect_equal( manualList, attributes(a_matrix)$dyad_data )
})

test_that('add_dyad, bipartite: mixed variable types', {
  
  ar = letters[1:3] ; nr = length(ar)
  ac = letters[22:24] ; nc = length(ac)
  fake_dyads <- expand.grid(actor1 = ar, actor2 = ac)
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads <- fake_dyads[fake_dyads$actor1 != fake_dyads$actor2,]
  
  fake_dyads$numeric_var <- rnorm(nrow(fake_dyads))
  fake_dyads$integer_var <- as.integer(round(runif(nrow(fake_dyads), 1, 10)))
  fake_dyads$logical_var <- sample(c(TRUE, FALSE), nrow(fake_dyads), replace = TRUE)
  fake_dyads$character_var <- sample(c("high", "low"), nrow(fake_dyads), replace = TRUE)
  
  a_matrix <- get_adjacency(
    fake_dyads, actor1 = 'actor1', actor2 = 'actor2', 
    symmetric = FALSE, weight = NULL, mode = 'bipartite', diag_to_NA = FALSE
  )
  
  a_matrix <- add_dyad_vars(
    a_matrix, fake_dyads, 'actor1', 'actor2', NULL,
    c('numeric_var', 'integer_var', 'logical_var', 'character_var'),
    c(FALSE, FALSE, FALSE, FALSE)
  )
  
  dyad_data <- attr(a_matrix, 'dyad_data')
  expect_equal(storage.mode(dyad_data[["1"]][["numeric_var"]]), "double")
  expect_equal(storage.mode(dyad_data[["1"]][["integer_var"]]), "integer") 
  expect_equal(storage.mode(dyad_data[["1"]][["logical_var"]]), "logical")
  expect_equal(storage.mode(dyad_data[["1"]][["character_var"]]), "character")
  
  # Check bipartite dimensions
  expect_equal(dim(dyad_data[["1"]][["numeric_var"]]), c(nr, nc))
})

test_that('get_adjacency, bipartite: empty actor2 set', {
  
  # Create data where all actor2 values get filtered out
  ar = letters[1:3]
  ac = letters[22:24]
  fake_dyads <- expand.grid(actor1 = ar, actor2 = ac)
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads$value <- rbinom(nrow(fake_dyads), 1, 0.1)
  
  # Filter to empty set
  fake_dyads <- fake_dyads[fake_dyads$value == 1 & fake_dyads$actor1 == "z",]
  
  if(nrow(fake_dyads) == 0) {
    # Should handle empty data gracefully
    expect_error({
      a_matrix <- netify(
        fake_dyads, actor1 = 'actor1', actor2 = 'actor2',
        symmetric = FALSE, weight = 'value', mode = 'bipartite'
      )
    }, 0) # 0 means error expected
  }
})

# unbalanced bipartite dimensions
test_that('get_adjacency, bipartite: very unbalanced dimensions', {
  
  # Many row actors, few column actors
  ar = letters[1:20]  # 20 row actors
  ac = letters[25:26]  # 2 column actors
  fake_dyads <- expand.grid(actor1 = ar, actor2 = ac)
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads$value <- rnorm(nrow(fake_dyads))
  
  a_matrix <- get_adjacency(
    fake_dyads, actor1 = 'actor1', actor2 = 'actor2',
    symmetric = FALSE, weight = 'value', mode = 'bipartite'
  )
  
  expect_equal(dim(get_raw(a_matrix)), c(20, 2))
  expect_equal(rownames(get_raw(a_matrix)), ar)
  expect_equal(colnames(get_raw(a_matrix)), ac)
})

# bipartite with actor time varying (longitudinal)
test_that('add_dyad, bipartite: longitudinal with varying actors', {
  
  ar = letters[1:4]
  ac = letters[22:25]
  
  # Time 1: all actors present
  t1_data <- expand.grid(actor1 = ar, actor2 = ac, time = 1)
  # Time 2: missing some actors
  t2_data <- expand.grid(actor1 = ar[1:3], actor2 = ac[1:3], time = 2)
  
  fake_dyads <- rbind(t1_data, t2_data)
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads <- fake_dyads[fake_dyads$actor1 != fake_dyads$actor2,]
  fake_dyads$var1 <- rnorm(nrow(fake_dyads))
  fake_dyads$dv <- rnorm(nrow(fake_dyads))
  
  a_matrix <- netify(
    fake_dyads, actor1 = 'actor1', actor2 = 'actor2', time = 'time',
    symmetric = FALSE, weight = 'dv', mode = 'bipartite', actor_time_uniform = FALSE
  )
  
  a_matrix <- add_dyad_vars(
    a_matrix, fake_dyads, 'actor1', 'actor2', 'time', 'var1', FALSE
  )
  
  dyad_data <- attr(a_matrix, 'dyad_data')
  
  # Should have different dimensions for different time periods
  expect_true("1" %in% names(dyad_data))
  expect_true("2" %in% names(dyad_data))
  expect_true("var1" %in% names(dyad_data[["1"]]))
  expect_true("var1" %in% names(dyad_data[["2"]]))
})

# large bipartite network performance
test_that('get_adjacency, bipartite: large sparse network', {
  
  # Create large but sparse bipartite network
  ar = paste0("R", 1:100)  # 100 row actors
  ac = paste0("C", 1:50)   # 50 column actors
  
  # Only create a small fraction of possible edges
  fake_dyads <- expand.grid( actor1 = ar, actor2 = ac )
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads <- fake_dyads[fake_dyads$actor1 != fake_dyads$actor2,]
  fake_dyads$value <- rnorm(nrow(fake_dyads))
  
  start_time <- Sys.time()
  
  a_matrix <- get_adjacency(
    fake_dyads, actor1 = 'actor1', actor2 = 'actor2',
    symmetric = FALSE, weight = 'value', mode = 'bipartite'
  )
  
  end_time <- Sys.time()
  
  # Should complete quickly and have correct dimensions
  expect_lt(as.numeric(end_time - start_time), 5)
  expect_equal(dim(get_raw(a_matrix)), c(100, 50))
})

# bipartite with missing values in dyadic attributes
test_that('add_dyad, bipartite: missing values in variables', {
  
  ar = letters[1:3]
  ac = letters[22:24]
  fake_dyads <- expand.grid(actor1 = ar, actor2 = ac)
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads <- fake_dyads[fake_dyads$actor1 != fake_dyads$actor2,]
  
  # Add variables with some missing values
  fake_dyads$var1 <- rnorm(nrow(fake_dyads))
  fake_dyads$var1[c(1,3)] <- NA  # introduce some NAs
  
  a_matrix <- get_adjacency(
    fake_dyads, actor1 = 'actor1', actor2 = 'actor2',
    symmetric = FALSE, weight = NULL, mode = 'bipartite', diag_to_NA = FALSE
  )
  
  a_matrix <- add_dyad_vars(
    a_matrix, fake_dyads, 'actor1', 'actor2', NULL, 'var1', FALSE
  )
  
  dyad_data <- attr(a_matrix, 'dyad_data')
  
  # Should handle NAs appropriately
  expect_true(any(is.na(dyad_data[["1"]][["var1"]])))
})

# replace existing dyadic variables in bipartite
test_that('add_dyad, bipartite: replace existing variables', {
  
  ar = letters[1:3]
  ac = letters[22:24]
  fake_dyads <- expand.grid(actor1 = ar, actor2 = ac)
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads <- fake_dyads[fake_dyads$actor1 != fake_dyads$actor2,]
  fake_dyads$var1 <- 1:nrow(fake_dyads)
  
  a_matrix <- get_adjacency(
    fake_dyads, actor1 = 'actor1', actor2 = 'actor2',
    symmetric = FALSE, weight = NULL, mode = 'bipartite', diag_to_NA = FALSE
  )
  
  # Add variable first time
  a_matrix <- add_dyad_vars(a_matrix, fake_dyads, 'actor1', 'actor2', NULL, 'var1', FALSE)
  
  # Change values and replace
  fake_dyads$var1 <- (1:nrow(fake_dyads)) * 100
  a_matrix <- add_dyad_vars(a_matrix, fake_dyads, 'actor1', 'actor2', NULL, 'var1', FALSE, 
                      replace_existing = TRUE)
  
  dyad_data <- attr(a_matrix, 'dyad_data')
  expect_true(any(dyad_data[["1"]][["var1"]] >= 100))
})