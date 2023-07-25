set.seed(6886)

test_that(
	'add_dyad: no time ID, asymmetric dyad vars', {

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

	# convert to conflictNet object
	a_matrix <- get_adjacency(
	  dyad_data=fakeDyads,
	  actor1='actor1', actor2='actor2', symmetric=TRUE,
	  weight=NULL,
	  diag_to_NA=FALSE )

	# add dyad variables in fake data as a dyadic attribute
	a_matrix = add_dyad(
	  a_matrix, fakeDyads,
	  'actor1', 'actor2', NULL,
	  c('var1', 'var2', 'var3', 'var4'),
	  c(FALSE, FALSE, FALSE, FALSE))

	# manually convert dyadic variables into list of array format
	manualArray <- array(NA, dim=c(3, 3, 4),
	  dimnames=list(
	    letters[1:3],
	    letters[1:3],
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
	'add_dyad: no time ID, symmetric dyad vars', {

	# function to generate symmetric variable
	genSymmVar = function(...){
		mat=matrix(rnorm(9), nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
		diag(mat) = NA
		mat[upper.tri(mat)] = mat[lower.tri(mat)]
		vDat = cbind(expand.grid(actor1=rownames(mat), actor2=colnames(mat)), var=c(mat))
		vDat$actor1=as.character(vDat$actor1)
		vDat$actor2=as.character(vDat$actor2)
		return(vDat$var)
	}

	# create fake dyad data for cross-sectional case
	fakeDyads <- expand.grid( actor1 = letters[1:3], actor2 = letters[1:3] )
	fakeDyads$actor1 = as.character(fakeDyads$actor1)
	fakeDyads$actor2 = as.character(fakeDyads$actor2)
	fakeDyads$var1 = genSymmVar()
	fakeDyads$var2 = genSymmVar()
	fakeDyads$var3 = genSymmVar()
	fakeDyads$var4 = genSymmVar()
	fakeDyads <- fakeDyads[fakeDyads$actor1!=fakeDyads$actor2,]
	fakeDyads$year <- 2312

	# convert to conflictNet object
	a_matrix <- get_adjacency(
	  dyad_data=fakeDyads,
	  actor1='actor1', actor2='actor2', symmetric=TRUE,
	  weight=NULL,
	  diag_to_NA=FALSE )

	# add dyad variables in fake data as a dyadic attribute
	a_matrix = add_dyad(
	  a_matrix, fakeDyads,
	  'actor1', 'actor2', NULL,
	  c('var1', 'var2', 'var3', 'var4'),
	  c(TRUE, TRUE, TRUE, TRUE))

	# manually convert dyadic variables into list of array format
	manualArray <- array(NA, dim=c(3, 3, 4),
	  dimnames=list(
	    letters[1:3],
	    letters[1:3],
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
  'add_dyad: supplied time ID, asymmetric dyad vars', {

	# create fake dyad data for longitudinal case
	fakeDyads <- expand.grid(
	  actor1 = letters[1:3], actor2 = letters[1:3] )
	timePds = 1:2
	fakeDyads = lapply(1:2, function(t){
	  fakeDyads$time = t
		fakeDyads$var1 <- rnorm(nrow(fakeDyads))
		fakeDyads$var2 <- rnorm(nrow(fakeDyads))
		fakeDyads$var3 <- rnorm(nrow(fakeDyads))
		fakeDyads$var4 <- rnorm(nrow(fakeDyads))
	  return(fakeDyads) })
	fakeDyads = do.call('rbind', fakeDyads)
	fakeDyads$actor1 = as.character(fakeDyads$actor1)
	fakeDyads$actor2 = as.character(fakeDyads$actor2)
	fakeDyads <- fakeDyads[fakeDyads$actor1!=fakeDyads$actor2,]

	# convert to conflictNet object
	a_matrix <- get_adjacency_list(
	  dyad_data=fakeDyads,
	  actor1='actor1', actor2='actor2', time='time',
	  symmetric=TRUE,
	  weight=NULL,
	  diag_to_NA=FALSE )

	# add dyad variables in fake data as a dyadic attribute
	a_matrix = add_dyad(
	  a_matrix, fakeDyads,
	  'actor1', 'actor2', 'time',
		c('var1', 'var2', 'var3', 'var4'),
		c(FALSE, FALSE, FALSE, FALSE))

	# manually convert dyadic variables into list of array format
	manualList <- lapply( unique(fakeDyads$time), function(timePd){

	  # construct manual array
	  manualArray <- array(NA, dim=c(3, 3, 4),
	    dimnames=list(
	      letters[1:3],
	      letters[1:3],
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

test_that(
  'add_dyad: supplied time ID, symmetric dyad vars', {

	# function to generate symmetric variable
	genSymmVar = function(...){
		mat=matrix(rnorm(9), nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
		diag(mat) = NA
		mat[upper.tri(mat)] = mat[lower.tri(mat)]
		vDat = cbind(expand.grid(actor1=rownames(mat), actor2=colnames(mat)), var=c(mat))
		vDat$actor1=as.character(vDat$actor1)
		vDat$actor2=as.character(vDat$actor2)
		return(vDat$var)
	}

	# create fake dyad data for longitudinal case
	fakeDyads <- expand.grid(
	  actor1 = letters[1:3], actor2 = letters[1:3] )
	timePds = 1:2
	fakeDyads = lapply(1:2, function(t){
	  fakeDyads$time = t
	  fakeDyads$var1 <- genSymmVar()
	  fakeDyads$var2 <- genSymmVar()
	  fakeDyads$var3 <- genSymmVar()
	  fakeDyads$var4 <- genSymmVar()
	  return(fakeDyads) })
	fakeDyads = do.call('rbind', fakeDyads)
	fakeDyads$actor1 = as.character(fakeDyads$actor1)
	fakeDyads$actor2 = as.character(fakeDyads$actor2)
	fakeDyads <- fakeDyads[fakeDyads$actor1!=fakeDyads$actor2,]

	# convert to conflictNet object
	a_matrix <- get_adjacency_list(
	  dyad_data=fakeDyads,
	  actor1='actor1', actor2='actor2', time='time',
	  symmetric=TRUE,
	  weight=NULL,
	  diag_to_NA=FALSE )

	# add dyad variables in fake data as a dyadic attribute
	a_matrix = add_dyad(
	  a_matrix, fakeDyads,
	  'actor1', 'actor2', 'time',
		c('var1', 'var2', 'var3', 'var4'),
		c(TRUE, TRUE, TRUE, TRUE))

	# manually convert dyadic variables into list of array format
	manualList <- lapply( unique(fakeDyads$time), function(timePd){

	  # construct manual array
	  manualArray <- array(NA, dim=c(3, 3, 4),
	    dimnames=list(
	      letters[1:3],
	      letters[1:3],
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
