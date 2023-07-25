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

    # add_nodal()
    net_adj = add_nodal(
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
    net_adj = add_nodal( net_adj, row_col_data, actor = 'actor' )
    net_adj = add_nodal( 
        net_adj, row_data, actor = 'actor',
        replace_existing = TRUE )
    net_adj = add_nodal( 
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

    # add_nodal()
    net_adj = add_nodal(
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

    # add_nodal()
    net_adj = add_nodal(
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
	'add_dyad, bipartite: zno time ID, weighted, asymmetric dyad vars', {

	# create fake dyad data for cross-sectional case
    ar = letters[1:3] ; nr = length(ar)
    ac = letters[22:26] ; nc = length(ac)
	fakeDyads <- expand.grid( actor1 = ar, actor2 = ac )
    fakeDyads$dv = rbinom(nrow(fakeDyads), 1, 0.5)
	fakeDyads$var1 <- rnorm(nrow(fakeDyads))
	fakeDyads$var2 <- rnorm(nrow(fakeDyads))
	fakeDyads$var3 <- rnorm(nrow(fakeDyads))
	fakeDyads$var4 <- rnorm(nrow(fakeDyads))
	fakeDyads$year <- 2312
	fakeDyads$actor1 = as.character(fakeDyads$actor1)
	fakeDyads$actor2 = as.character(fakeDyads$actor2)
	fakeDyads <- fakeDyads[fakeDyads$actor1!=fakeDyads$actor2,]

	# convert to conflictNet object
	a_matrix <- get_adjacency(
	  dyad_data=fakeDyads,
	  actor1='actor1', actor2='actor2', symmetric=TRUE,
	  weight='dv',
	  mode='bipartite' )

	# add dyad variables in fake data as a dyadic attribute
	a_matrix = add_dyad(
	  a_matrix, fakeDyads,
	  'actor1', 'actor2', NULL,
	  c('var1', 'var2', 'var3', 'var4'),
	  c(FALSE, FALSE, FALSE, FALSE))

	# manually convert dyadic variables into list of array format
	manualArray <- array(NA, dim=c(nr, nc, 4),
	  dimnames=list(
	    ar,
	    ac,
	    paste0('var', 1:4)
	    ))
	for( v in paste0('var', 1:4) ){
	  for( ii in 1:nrow(fakeDyads) ){
	    a1 <- fakeDyads$actor1[ii]
	    a2 <- fakeDyads$actor2[ii]
	    val <- fakeDyads[ii,v]
	    manualArray[a1, a2, v] <- val
	  }
	}
	manualList <- list(manualArray)
	names(manualList) <- '1'

  # test if identical
	expect_equal( attributes(a_matrix)$dyad_data , manualList )
})

test_that(
	'add_dyad, bipartite: zno time ID, unweighted, asymmetric dyad vars', {

	# create fake dyad data for cross-sectional case
    ar = letters[1:3] ; nr = length(ar)
    ac = letters[22:26] ; nc = length(ac)
	fakeDyads <- expand.grid( actor1 = ar, actor2 = ac )
    set.seed(6886)
    fakeDyads$dv = rbinom(nrow(fakeDyads), 1, 0.8)
	fakeDyads$var1 <- rnorm(nrow(fakeDyads))
	fakeDyads$var2 <- rnorm(nrow(fakeDyads))
	fakeDyads$var3 <- rnorm(nrow(fakeDyads))
	fakeDyads$var4 <- rnorm(nrow(fakeDyads))
	fakeDyads$year <- 2312
	fakeDyads$actor1 = as.character(fakeDyads$actor1)
	fakeDyads$actor2 = as.character(fakeDyads$actor2)
	fakeDyads <- fakeDyads[fakeDyads$actor1!=fakeDyads$actor2,]
    fakeDyads = fakeDyads[fakeDyads$dv==1,]

	# convert to conflictNet object
	a_matrix <- get_adjacency(
	  dyad_data=fakeDyads,
	  actor1='actor1', actor2='actor2', symmetric=TRUE,
	  weight=NULL,
	  mode='bipartite' )

	# add dyad variables in fake data as a dyadic attribute
	a_matrix = add_dyad(
	  a_matrix, fakeDyads,
	  'actor1', 'actor2', NULL,
	  c('var1', 'var2', 'var3', 'var4'),
	  c(FALSE, FALSE, FALSE, FALSE))

	# manually convert dyadic variables into list of array format
	manualArray <- array(NA, dim=c(nr, nc, 4),
	  dimnames=list(
	    ar,
	    ac,
	    paste0('var', 1:4)
	    ))
	for( v in paste0('var', 1:4) ){
	  for( ii in 1:nrow(fakeDyads) ){
	    a1 <- fakeDyads$actor1[ii]
	    a2 <- fakeDyads$actor2[ii]
	    val <- fakeDyads[ii,v]
	    manualArray[a1, a2, v] <- val
	  }
	}
	manualList <- list(manualArray)
	names(manualList) <- '1'

  # test if identical
	expect_equal( attributes(a_matrix)$dyad_data , manualList )
})

test_that(
  'add_dyad, bipartite: supplied time ID, asymmetric dyad vars', {

	# create fake dyad data for longitudinal case
    ar = letters[1:3] ; nr = length(ar)
    ac = letters[22:26] ; nc = length(ac)
	fakeDyads <- expand.grid( 
        actor1 = ar, actor2 = ac, time=1:2 )
    fakeDyads$dv <- rnorm(nrow(fakeDyads))    
    fakeDyads$var1 <- rnorm(nrow(fakeDyads))
    fakeDyads$var2 <- rnorm(nrow(fakeDyads))
    fakeDyads$var3 <- rnorm(nrow(fakeDyads))
    fakeDyads$var4 <- rnorm(nrow(fakeDyads))
	fakeDyads$actor1 = as.character(fakeDyads$actor1)
	fakeDyads$actor2 = as.character(fakeDyads$actor2)
	fakeDyads <- fakeDyads[fakeDyads$actor1!=fakeDyads$actor2,]

	# convert to conflictNet object
	a_matrix <- netify(
	  dyad_data=fakeDyads,
	  actor1='actor1', actor2='actor2', time='time',
	  symmetric=TRUE,
	  weight='dv',
	  mode='bipartite' )

	# add dyad variables in fake data as a dyadic attribute
	a_matrix = add_dyad(
	  a_matrix, fakeDyads,
	  'actor1', 'actor2', 'time',
		c('var1', 'var2', 'var3', 'var4'),
		c(FALSE, FALSE, FALSE, FALSE))

	# manually convert dyadic variables into list of array format
	manualList <- lapply( unique(fakeDyads$time), function(timePd){

	  # construct manual array
	  manualArray <- array(NA, dim=c(nr, nc, 4),
	    dimnames=list(
	      ar,
	      ac,
	      paste0('var', 1:4)
	      ))

	  # subset inputted data by timepd
	  slice <- fakeDyads[fakeDyads$time==timePd,]

	  # fill in via for loop
	  for( v in paste0('var', 1:4) ){
	    for( ii in 1:nrow(slice) ){
	      a1 <- slice$actor1[ii]
	      a2 <- slice$actor2[ii]
	      val <- slice[ii,v]
	      manualArray[a1, a2, v] <- val
	    }
	  }

	  #
	  return(manualArray) })
	names(manualList) <- unique(fakeDyads$time)

    # test if identical
    expect_equal( manualList, attributes(a_matrix)$dyad_data )
})
