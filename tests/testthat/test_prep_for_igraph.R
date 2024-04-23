set.seed(6886)
# library(igraph)
library(reshape2)

test_that('prep_for_igraph: unweighted cross-sec, asymmetric', {

    # use igraph example to generate some data
     adjm <- matrix(
        as.numeric(sample(0:1, 100, replace=TRUE, prob=c(0.5, .5))), 
        ncol=10)
     rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
     g1 <- igraph::graph_from_adjacency_matrix( adjm )

    # convert to dyadic so we can pass into netify
    df <- melt(adjm)
    df <- df[df$value>0,]
    df$Var1 <- as.character(df$Var1)
    df$Var2 <- as.character(df$Var2)

    # convert to netify object
    a_matrix <- netify(
      dyad_data=df,
      actor1='Var1', actor2='Var2', symmetric=FALSE,
      weight=NULL,
      diag_to_NA=FALSE )

    # convert to igraph object
    ng <- prep_for_igraph(a_matrix)

    # compare
    expect_identical(ng[,], g1[,])
})

test_that('prep_for_igraph: weighted cross-sec, asymmetric', {

    # use igraph example to generate some data
     adjm <- matrix( rnorm(10^2), ncol=10)
     rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
     g1 <- igraph::graph_from_adjacency_matrix( adjm, weighted=TRUE )

    # convert to dyadic so we can pass into netify
    df <- melt(adjm)
    df$Var1 <- as.character(df$Var1)
    df$Var2 <- as.character(df$Var2)

    # convert to netify object
    a_matrix <- netify(
      dyad_data=df,
      actor1='Var1', actor2='Var2', symmetric=FALSE,
      weight='value',
      diag_to_NA=FALSE )

    # convert to igraph object
    ng <- prep_for_igraph(a_matrix)

    # compare
    expect_identical(ng[,], g1[,])
})

test_that('prep_for_igraph: unweighted cross-sec, symmetric', {

    # use igraph example to generate some data
     adjm <- matrix( rnorm(10^2), ncol=10)
     adjm <- (adjm + t(adjm))/2
     adjm[adjm>0] <- 1
     adjm[adjm<0] <- 0
     diag(adjm) <- NA
     rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
     g1 <- igraph::graph_from_adjacency_matrix( adjm, diag=FALSE )

    # convert to dyadic so we can pass into netify
    df <- melt(adjm)
    df$Var1 <- as.character(df$Var1)
    df$Var2 <- as.character(df$Var2)
    df = df[df$Var1 != df$Var2,]
    df = df[df$value==1,]

    # convert to netify object
    a_matrix <- netify(
      dyad_data=df,
      actor1='Var1', actor2='Var2', symmetric=TRUE,
      weight=NULL,
      diag_to_NA=TRUE )

    # convert to igraph object
    ng <- prep_for_igraph(a_matrix)

    # compare
    expect_identical(ng[,], g1[,])
})

test_that('prep_for_igraph: weighted cross-sec, symmetric', {

    # use igraph example to generate some data
     adjm <- matrix( rnorm(10^2), ncol=10)
     adjm <- (adjm + t(adjm))/2
     diag(adjm) <- NA
     rownames(adjm) = colnames(adjm) = letters[1:nrow(adjm)]
     g1 <- igraph::graph_from_adjacency_matrix( adjm, weighted=TRUE, diag=FALSE )

    # convert to dyadic so we can pass into netify
    df <- melt(adjm)
    df$Var1 <- as.character(df$Var1)
    df$Var2 <- as.character(df$Var2)
    df = df[df$Var1 != df$Var2,]

    # convert to netify object
    a_matrix <- netify(
      dyad_data=df,
      actor1='Var1', actor2='Var2', symmetric=TRUE,
      weight='value',
      diag_to_NA=TRUE )

    # convert to igraph object
    ng <- prep_for_igraph(a_matrix)

    # compare
    expect_identical(ng[,], g1[,])
})

test_that('prep_for_igraph, bipartite: unweighted cross-sec, asymmetric', {

    # use igraph example to generate some data
    adjm <- matrix(
        as.numeric(sample(0:1, 100, replace=TRUE, prob=c(0.5, .5))), ncol=10)
    adjm = adjm[1:5,]
    rownames(adjm) = letters[1:5]
    colnames(adjm) = letters[(length(letters)-9):length(letters)]

    # create igraph object that we'll compare with
    g1 <- igraph::graph_from_biadjacency_matrix( adjm )
    g1_raw <- data.matrix(g1[,])
    vtype <- igraph::V(g1)$type
    g1_raw <- g1_raw[!vtype,vtype]

    # convert to dyadic so we can pass into netify
    df <- melt(adjm)
    df <- df[df$value>0,]
    df$Var1 <- as.character(df$Var1)
    df$Var2 <- as.character(df$Var2)

    # convert to netify object
    a_matrix <- netify(
      dyad_data=df,
      actor1='Var1', actor2='Var2', symmetric=FALSE,
      weight=NULL,
      mode='bipartite' )

    # convert to igraph object
    ng <- prep_for_igraph(a_matrix)
    ng_raw <- data.matrix(ng[,])
    vtype <- igraph::V(ng)$type
    ng_raw <- ng_raw[!vtype,vtype]

    # compare
    expect_identical(ng_raw, g1_raw)
})

test_that('prep_for_igraph, bipartite: weighted cross-sec, asymmetric', {

    # use igraph example to generate some data
    adjm <- matrix( rnorm(10^2), ncol=10)
    adjm = adjm[1:5,]
    rownames(adjm) = letters[1:5]
    colnames(adjm) = letters[(length(letters)-9):length(letters)]

    # create igraph object that we'll compare with
    g1 <- igraph::graph_from_biadjacency_matrix( adjm, weighted=TRUE )
    g1_raw <- data.matrix(g1[,])
    vtype <- igraph::V(g1)$type
    g1_raw <- g1_raw[!vtype,vtype]

    # convert to dyadic so we can pass into netify
    df <- melt(adjm)
    df$Var1 <- as.character(df$Var1)
    df$Var2 <- as.character(df$Var2)

    # convert to netify object
    a_matrix <- netify(
      dyad_data=df,
      actor1='Var1', actor2='Var2', symmetric=FALSE,
      weight='value',
      mode='bipartite' )

    # convert to igraph object
    ng <- prep_for_igraph(a_matrix)
    ng_raw <- data.matrix(ng[,])
    vtype <- igraph::V(ng)$type
    ng_raw <- ng_raw[!vtype,vtype]

    # compare
    expect_identical(ng_raw, g1_raw)
})

test_that(
	'prep_for_igraph: weighted cross-sec, dyad and nodal attribs', {

		###################################
		# create fake dyad data for cross-sectional case
		fakeDyads <- expand.grid( actor1 = letters[1:3], actor2 = letters[1:3] )
		fakeDyads$weight <- rnorm(nrow(fakeDyads))
		fakeDyads$var2 <- rnorm(nrow(fakeDyads))
		fakeDyads$var3 <- rnorm(nrow(fakeDyads))
		fakeDyads$var4 <- rnorm(nrow(fakeDyads))
		fakeDyads$year <- 2312
		fakeDyads$actor1 = as.character(fakeDyads$actor1)
		fakeDyads$actor2 = as.character(fakeDyads$actor2)
		fakeDyads <- fakeDyads[fakeDyads$actor1!=fakeDyads$actor2,]

		# create fake node data for cross-sectional case
		fakeNodes <- data.frame( actor1 = letters[1:3], var1 = rnorm(3), var2 = rnorm(3) )

		# convert to netify object
		a_matrix <- netify(
		  dyad_data=fakeDyads,
		  actor1='actor1', actor2='actor2', symmetric=FALSE,
		  weight='weight',
		  diag_to_NA=FALSE )

		# add dyad variables in fake data as a dyadic attribute
		a_matrix = add_dyad(
		  a_matrix, fakeDyads,
		  'actor1', 'actor2', NULL,
		  c('var2', 'var3', 'var4'),
		  c(FALSE, FALSE, FALSE, FALSE))
		# add node variables in fake data
		a_matrix = add_nodal(
			a_matrix, fakeNodes,
			'actor1', NULL, NULL)
		###################################

		###################################
		# gen igraph versions
		prepped_g <- prep_for_igraph(a_matrix)
		g <- igraph::graph_from_data_frame(fakeDyads, directed = TRUE, vertices = fakeNodes)
		###################################        

		###################################
		# check nodes
    nNames = igraph::V(g)$name ; npNames = igraph::V(prepped_g)$name
    nv1 = igraph::V(g)$var1 ; names(nv1) = nNames
    nv2 = igraph::V(g)$var2 ; names(nv2) = nNames
    npv1 = igraph::V(prepped_g)$var1 ; names(npv1) = npNames
    npv2 = igraph::V(prepped_g)$var2 ; names(npv2) = npNames
    expect_identical(npv1[nNames], nv1)
    expect_identical(npv2[nNames], nv2)

    # check edges
    # extract edge names
    gvecs = attributes(igraph::E(g ))$vnames
    pgvecs = attributes(igraph::E(prepped_g))$vnames
    
    # pull out edge vars
    pvar2 <- igraph::E(prepped_g)$var2 ; names(pvar2) <- pgvecs
    pvar3 <- igraph::E(prepped_g)$var3 ; names(pvar3) <- pgvecs
    pvar4 <- igraph::E(prepped_g)$var4 ; names(pvar4) <- pgvecs
    var2 <- igraph::E(g)$var2 ; names(var2) <- gvecs
    var3 <- igraph::E(g)$var3 ; names(var3) <- gvecs
    var4 <- igraph::E(g)$var4 ; names(var4) <- gvecs

    # check identical
    expect_identical(pvar2[names(var2)], var2)
    expect_identical(pvar3[names(var3)], var3)
    expect_identical(pvar4[names(var4)], var4)
		###################################
})

test_that(
	'prep_for_igraph: unweighted cross-sec, dyad and nodal attribs', {

		###################################
		# create fake dyad data for cross-sectional case
		fakeDyads <- expand.grid( actor1 = letters[1:3], actor2 = letters[1:3] )
		fakeDyads$var1 <- rnorm(nrow(fakeDyads))
		fakeDyads$var2 <- rnorm(nrow(fakeDyads))
		fakeDyads$var3 <- rnorm(nrow(fakeDyads))
		fakeDyads$var4 <- rnorm(nrow(fakeDyads))
		fakeDyads$year <- 2312
		fakeDyads$actor1 = as.character(fakeDyads$actor1)
		fakeDyads$actor2 = as.character(fakeDyads$actor2)
		fakeDyads <- fakeDyads[fakeDyads$actor1!=fakeDyads$actor2,]
        fakeDyads$dv = rbinom(nrow(fakeDyads), 1, 0.6)
        fakeDyads = fakeDyads[fakeDyads$dv==1,]

		# create fake node data for cross-sectional case
		fakeNodes <- data.frame( actor1 = letters[1:3], var1 = rnorm(3), var2 = rnorm(3) )

		# convert to netify object
		a_matrix <- netify(
		  dyad_data=fakeDyads,
		  actor1='actor1', actor2='actor2', symmetric=FALSE,
		  weight=NULL,
		  diag_to_NA=FALSE )

		# add dyad variables in fake data as a dyadic attribute
		a_matrix = add_dyad(
		  a_matrix, fakeDyads,
		  'actor1', 'actor2', NULL,
		  c('var1','var2', 'var3', 'var4'),
		  c(FALSE, FALSE, FALSE, FALSE))
		# add node variables in fake data
		a_matrix = add_nodal(
			a_matrix, fakeNodes,
			'actor1', NULL, NULL)
		###################################

		###################################
		# gen igraph versions
		prepped_g <- prep_for_igraph(a_matrix)
		g <- igraph::graph_from_data_frame(fakeDyads, directed = TRUE, vertices = fakeNodes)
		###################################        

		###################################
		# check nodes
    nNames = igraph::V(g)$name ; npNames = igraph::V(prepped_g)$name
    nv1 = igraph::V(g)$var1 ; names(nv1) = nNames
    nv2 = igraph::V(g)$var2 ; names(nv2) = nNames
    npv1 = igraph::V(prepped_g)$var1 ; names(npv1) = npNames
    npv2 = igraph::V(prepped_g)$var2 ; names(npv2) = npNames
    expect_identical(npv1[nNames], nv1)
    expect_identical(npv2[nNames], nv2)

    # check edges
    # extract edge names
    gvecs = attributes(igraph::E(g ))$vnames
    pgvecs = attributes(igraph::E(prepped_g))$vnames
    
    # pull out edge vars
    pvar1 <- igraph::E(prepped_g)$var1 ; names(pvar1) <- pgvecs
    pvar2 <- igraph::E(prepped_g)$var2 ; names(pvar2) <- pgvecs
    pvar3 <- igraph::E(prepped_g)$var3 ; names(pvar3) <- pgvecs
    pvar4 <- igraph::E(prepped_g)$var4 ; names(pvar4) <- pgvecs
    var1 <- igraph::E(g)$var1 ; names(var1) <- gvecs
    var2 <- igraph::E(g)$var2 ; names(var2) <- gvecs
    var3 <- igraph::E(g)$var3 ; names(var3) <- gvecs
    var4 <- igraph::E(g)$var4 ; names(var4) <- gvecs

    # check identical
    expect_identical(pvar1[names(var1)], var1)
    expect_identical(pvar2[names(var2)], var2)
    expect_identical(pvar3[names(var3)], var3)
    expect_identical(pvar4[names(var4)], var4)
		###################################
})


test_that(
	'prep_for_igraph, bipartite: weighted cross-sec, dyad and nodal attribs', {

		###################################
		# create fake dyad data for cross-sectional case
		ar = letters[1:3] ; nr = length(ar)
		ac = letters[22:26] ; nc = length(ac)		
		fakeDyads <- expand.grid( actor1 = ar, actor2 = ac )
		fakeDyads$weight <- rnorm(nrow(fakeDyads))
		fakeDyads$var2 <- rnorm(nrow(fakeDyads))
		fakeDyads$var3 <- rnorm(nrow(fakeDyads))
		fakeDyads$var4 <- rnorm(nrow(fakeDyads))
		fakeDyads$year <- 2312
		fakeDyads$actor1 = as.character(fakeDyads$actor1)
		fakeDyads$actor2 = as.character(fakeDyads$actor2)
		fakeDyads <- fakeDyads[fakeDyads$actor1!=fakeDyads$actor2,]

		# create fake node data for cross-sectional case
		actors <- c(ar, ac) ; nactors <- nr + nc
		fakeNodes <- data.frame( 
            actor1 = actors, var1 = rnorm(nactors), var2 = rnorm(nactors) )

		# convert to netify object
		a_matrix <- netify(
		  dyad_data=fakeDyads,
		  actor1='actor1', actor2='actor2', 
		  weight='weight',
		  mode='bipartite' )

		# add dyad variables in fake data as a dyadic attribute
		a_matrix = add_dyad(
		  a_matrix, fakeDyads,
		  'actor1', 'actor2', NULL,
		  c('var2', 'var3', 'var4'),
		  c(FALSE, FALSE, FALSE))
        
		# add node variables in fake data
		a_matrix = add_nodal(
			a_matrix, fakeNodes,
			'actor1', NULL, NULL)
		###################################

		###################################
		# gen igraph versions
		prepped_g <- prep_for_igraph(a_matrix)

        # from raw data
		g <- igraph::graph_from_data_frame(fakeDyads, directed = TRUE, vertices = fakeNodes)
		###################################        

		###################################
		# check nodes
    nNames = igraph::V(g)$name ; npNames = igraph::V(prepped_g)$name
    nv1 = igraph::V(g)$var1 ; names(nv1) = nNames
    nv2 = igraph::V(g)$var2 ; names(nv2) = nNames
    npv1 = igraph::V(prepped_g)$var1 ; names(npv1) = npNames
    npv2 = igraph::V(prepped_g)$var2 ; names(npv2) = npNames
    expect_identical(npv1[nNames], nv1)
    expect_identical(npv2[nNames], nv2)

    # check edges
    # extract edge names
    gvecs = attributes(igraph::E(g ))$vnames
    pgvecs = attributes(igraph::E(prepped_g))$vnames
    
    # pull out edge vars
    pvar2 <- igraph::E(prepped_g)$var2 ; names(pvar2) <- pgvecs
    pvar3 <- igraph::E(prepped_g)$var3 ; names(pvar3) <- pgvecs
    pvar4 <- igraph::E(prepped_g)$var4 ; names(pvar4) <- pgvecs
    var2 <- igraph::E(g)$var2 ; names(var2) <- gvecs
    var3 <- igraph::E(g)$var3 ; names(var3) <- gvecs
    var4 <- igraph::E(g)$var4 ; names(var4) <- gvecs

    # check identical
    expect_identical(pvar2[names(var2)], var2)
    expect_identical(pvar3[names(var3)], var3)
    expect_identical(pvar4[names(var4)], var4)
		###################################
})

