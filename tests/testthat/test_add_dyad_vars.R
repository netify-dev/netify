# library(testthat)
# library(netify)
# devtools::load_all('~/Research/netify_dev/netify')

set.seed(6886)

# cross sec and asymm
test_that(
	'add_dyad_vars: no time ID, asymmetric dyad vars', {

	# create fake dyad data for cross-sectional case
	fake_dyads <- expand.grid( actor1 = letters[1:3], actor2 = letters[1:3] )
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
	  fake_dyads,
	  actor1='actor1', actor2='actor2', symmetric=TRUE,
	  weight=NULL,
	  diag_to_NA=FALSE )

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
	    var1 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3])),
	    var2 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3])),
	    var3 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3])),
	    var4 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
	  )
	)
	
	# fill in matrices
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

# cross sec and symm
test_that(
	'add_dyad_vars: no time ID, symmetric dyad vars', {

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
	fake_dyads <- expand.grid( actor1 = letters[1:3], actor2 = letters[1:3] )
	fake_dyads$actor1 = as.character(fake_dyads$actor1)
	fake_dyads$actor2 = as.character(fake_dyads$actor2)
	fake_dyads$var1 = genSymmVar()
	fake_dyads$var2 = genSymmVar()
	fake_dyads$var3 = genSymmVar()
	fake_dyads$var4 = genSymmVar()
	fake_dyads <- fake_dyads[fake_dyads$actor1!=fake_dyads$actor2,]
	fake_dyads$year <- 2312

	# convert to conflictNet object
	a_matrix <- get_adjacency(
	  fake_dyads,
	  actor1='actor1', actor2='actor2', symmetric=TRUE,
	  weight=NULL,
	  diag_to_NA=FALSE )

	# add dyad variables in fake data as a dyadic attribute
	a_matrix = add_dyad_vars(
	  a_matrix, fake_dyads,
	  'actor1', 'actor2', NULL,
	  c('var1', 'var2', 'var3', 'var4'),
	  c(TRUE, TRUE, TRUE, TRUE))

	# manually convert dyadic variables into NEW list of matrices format
	# Initialize with 0s to match get_matrix behavior when missing_to_zero=TRUE
	manualList <- list(
	  "1" = list(
	    var1 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3])),
	    var2 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3])),
	    var3 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3])),
	    var4 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
	  )
	)
	
	# fill in matrices (symmetric case)
	for( v in paste0('var', 1:4) ){
	  for( ii in 1:nrow(fake_dyads) ){
	    a1 <- fake_dyads$actor1[ii]
	    a2 <- fake_dyads$actor2[ii]
	    val <- fake_dyads[ii,v]
	    manualList[["1"]][[v]][a1, a2] <- val
	    # For symmetric variables, also fill the transpose
	    manualList[["1"]][[v]][a2, a1] <- val
	  }
	}

  # test if identical
	expect_equal( attributes(a_matrix)$dyad_data , manualList )
})

# longit and asymm
test_that(
  'add_dyad_vars: supplied time ID, asymmetric dyad vars', {

	# create fake dyad data for longitudinal case
	fake_dyads <- expand.grid(
	  actor1 = letters[1:3], actor2 = letters[1:3] )
	timePds = 1:2
	fake_dyads = lapply(1:2, function(t){
	  fake_dyads$time = t
		fake_dyads$var1 <- rnorm(nrow(fake_dyads))
		fake_dyads$var2 <- rnorm(nrow(fake_dyads))
		fake_dyads$var3 <- rnorm(nrow(fake_dyads))
		fake_dyads$var4 <- rnorm(nrow(fake_dyads))
	  return(fake_dyads) })
	fake_dyads = do.call('rbind', fake_dyads)
	fake_dyads$actor1 = as.character(fake_dyads$actor1)
	fake_dyads$actor2 = as.character(fake_dyads$actor2)
	fake_dyads <- fake_dyads[fake_dyads$actor1!=fake_dyads$actor2,]

	# convert to conflictNet object
	a_matrix <- get_adjacency_list(
	  fake_dyads,
	  actor1='actor1', actor2='actor2', time='time',
	  symmetric=TRUE,
	  weight=NULL,
	  diag_to_NA=FALSE )

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
	    var1 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3])),
	    var2 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3])),
	    var3 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3])),
	    var4 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
	  )

	  # subset inputted data by timepd
	  slice <- fake_dyads[fake_dyads$time==timePd,]

	  # fill in via for loop (asymmetric case)
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

# longit and symm
test_that(
  'add_dyad_vars: supplied time ID, symmetric dyad vars', {

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
	fake_dyads <- expand.grid(
	  actor1 = letters[1:3], actor2 = letters[1:3] )
	timePds = 1:2
	fake_dyads = lapply(1:2, function(t){
	  fake_dyads$time = t
	  fake_dyads$var1 <- genSymmVar()
	  fake_dyads$var2 <- genSymmVar()
	  fake_dyads$var3 <- genSymmVar()
	  fake_dyads$var4 <- genSymmVar()
	  return(fake_dyads) })
	fake_dyads = do.call('rbind', fake_dyads)
	fake_dyads$actor1 = as.character(fake_dyads$actor1)
	fake_dyads$actor2 = as.character(fake_dyads$actor2)
	fake_dyads <- fake_dyads[fake_dyads$actor1!=fake_dyads$actor2,]

	# convert to conflictNet object
	a_matrix <- get_adjacency_list(
	  fake_dyads,
	  actor1='actor1', actor2='actor2', time='time',
	  symmetric=TRUE,
	  weight=NULL,
	  diag_to_NA=FALSE )

	# add dyad variables in fake data as a dyadic attribute
	a_matrix = add_dyad_vars(
	  a_matrix, fake_dyads,
	  'actor1', 'actor2', 'time',
		c('var1', 'var2', 'var3', 'var4'),
		c(TRUE, TRUE, TRUE, TRUE))

	# manually convert dyadic variables into NEW list of matrices format
	manualList <- lapply( unique(fake_dyads$time), function(timePd){

	  # create list of individual matrices for this time period
	  # Initialize with 0s to match get_matrix behavior when missing_to_zero=TRUE
	  time_matrices <- list(
	    var1 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3])),
	    var2 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3])),
	    var3 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3])),
	    var4 = matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3], letters[1:3]))
	  )

	  # subset inputted data by timepd
	  slice <- fake_dyads[fake_dyads$time==timePd,]

	  # fill in via for loop (symmetric case)
	  for( v in paste0('var', 1:4) ){
	    for( ii in 1:nrow(slice) ){
	      a1 <- slice$actor1[ii]
	      a2 <- slice$actor2[ii]
	      val <- slice[ii,v]
	      time_matrices[[v]][a1, a2] <- val
	      # For symmetric variables, also fill the transpose
	      time_matrices[[v]][a2, a1] <- val
	    }
	  }

	  return(time_matrices)
	})
	names(manualList) <- as.character(unique(fake_dyads$time))

  # test if identical
	expect_equal( manualList, attributes(a_matrix)$dyad_data )

})

# mixed vars
test_that('add_dyad_vars: mixed variable types', {
  
  # create fake dyad data with different types
  fake_dyads <- expand.grid(actor1 = letters[1:3], actor2 = letters[1:3])
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads <- fake_dyads[fake_dyads$actor1 != fake_dyads$actor2,]
  
  fake_dyads$numeric_var <- rnorm(nrow(fake_dyads))
  fake_dyads$integer_var <- as.integer(round(runif(nrow(fake_dyads), 1, 10)))
  fake_dyads$logical_var <- sample(c(TRUE, FALSE), nrow(fake_dyads), replace = TRUE)
  fake_dyads$character_var <- sample(c("high", "low", "medium"), nrow(fake_dyads), replace = TRUE)
  
  # create netify object
  a_matrix <- get_adjacency(
    dyad_data = fake_dyads,
    actor1 = 'actor1', actor2 = 'actor2', 
    symmetric = FALSE, weight = NULL, diag_to_NA = FALSE
  )
  
  # add mixed type variables
  a_matrix <- add_dyad_vars(
    a_matrix, fake_dyads,
    'actor1', 'actor2', NULL,
    c('numeric_var', 'integer_var', 'logical_var', 'character_var'),
    c(FALSE, FALSE, FALSE, FALSE)
  )
  
  # test storage modes
  dyad_data <- attr(a_matrix, 'dyad_data')
  expect_equal(storage.mode(dyad_data[["1"]][["numeric_var"]]), "double")
  expect_equal(storage.mode(dyad_data[["1"]][["integer_var"]]), "integer") 
  expect_equal(storage.mode(dyad_data[["1"]][["logical_var"]]), "logical")
  expect_equal(storage.mode(dyad_data[["1"]][["character_var"]]), "character")
})

# rplace existing variables
test_that('add_dyad_vars: replace existing variables', {
  
  fake_dyads <- expand.grid(actor1 = letters[1:3], actor2 = letters[1:3])
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads <- fake_dyads[fake_dyads$actor1 != fake_dyads$actor2,]
  fake_dyads$var1 <- 1:nrow(fake_dyads)
  
  # create netify object and add variable
  a_matrix <- get_adjacency(dyad_data = fake_dyads, actor1 = 'actor1', actor2 = 'actor2', 
                           symmetric = FALSE, weight = NULL, diag_to_NA = FALSE)
  a_matrix <- add_dyad_vars(a_matrix, fake_dyads, 'actor1', 'actor2', NULL, 'var1', FALSE)
  
  # create new data with different values
  fake_dyads$var1 <- (1:nrow(fake_dyads)) * 100
  
  # add again with replace_existing = TRUE
  a_matrix <- add_dyad_vars(a_matrix, fake_dyads, 'actor1', 'actor2', NULL, 'var1', FALSE, 
                      replace_existing = TRUE)
  
  # check that values were replaced
  dyad_data <- attr(a_matrix, 'dyad_data')
  expect_true(any(dyad_data[["1"]][["var1"]] >= 100))
})

# merge with existing dyad data
test_that('add_dyad_vars: merge with existing dyad data', {
  
  fake_dyads <- expand.grid(actor1 = letters[1:3], actor2 = letters[1:3])
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads <- fake_dyads[fake_dyads$actor1 != fake_dyads$actor2,]
  fake_dyads$var1 <- rnorm(nrow(fake_dyads))
  fake_dyads$var2 <- rnorm(nrow(fake_dyads))
  
  # create netify object and add first variable
  a_matrix <- get_adjacency(dyad_data = fake_dyads, actor1 = 'actor1', actor2 = 'actor2', 
                           symmetric = FALSE, weight = NULL, diag_to_NA = FALSE)
  a_matrix <- add_dyad_vars(a_matrix, fake_dyads, 'actor1', 'actor2', NULL, 'var1', FALSE)
  
  # add second variable
  a_matrix <- add_dyad_vars(a_matrix, fake_dyads, 'actor1', 'actor2', NULL, 'var2', FALSE)
  
  # check both variables exist
  dyad_data <- attr(a_matrix, 'dyad_data')
  expect_true("var1" %in% names(dyad_data[["1"]]))
  expect_true("var2" %in% names(dyad_data[["1"]]))
  expect_equal(length(dyad_data[["1"]]), 2)
})

# empty data handling
test_that('add_dyad_vars: empty data slices', {
  
  # create longitudinal data where some time periods have no data
  fake_dyads <- expand.grid(actor1 = letters[1:3], actor2 = letters[1:3], time = c(1, 3))
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads <- fake_dyads[fake_dyads$actor1 != fake_dyads$actor2,]
  fake_dyads$var1 <- rnorm(nrow(fake_dyads))
  
  # create longitudinal netify object with times 1, 2, 3 (time 2 will be empty)
  full_data <- rbind(fake_dyads, 
                     data.frame(actor1 = "a", actor2 = "b", time = 2, var1 = 0))
  full_data <- full_data[full_data$actor1 != full_data$actor2,]
  
  a_matrix <- get_adjacency_list(
    dyad_data = full_data[, c("actor1", "actor2", "time")],
    actor1 = 'actor1', actor2 = 'actor2', time = 'time',
    symmetric = FALSE, weight = NULL, diag_to_NA = FALSE
  )
  
  # add dyad data (time 2 will have no entries)
  a_matrix <- add_dyad_vars(a_matrix, fake_dyads, 'actor1', 'actor2', 'time', 'var1', FALSE)
  
  dyad_data <- attr(a_matrix, 'dyad_data')
  
  # check that all time periods exist
  expect_true("1" %in% names(dyad_data))
  expect_true("2" %in% names(dyad_data))  
  expect_true("3" %in% names(dyad_data))
  
  # check that empty time period has zero matrix
  expect_true(all(dyad_data[["2"]][["var1"]] == 0))
})

# bipartite networks
test_that('add_dyad_vars: bipartite networks', {
  
  # create bipartite data (actors in actor1 different from actor2)
  fake_dyads <- expand.grid(actor1 = paste0("A", 1:2), actor2 = paste0("B", 1:3))
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads$var1 <- rnorm(nrow(fake_dyads))
  
  # create bipartite netify object
  a_matrix <- get_adjacency(
    dyad_data = fake_dyads,
    actor1 = 'actor1', actor2 = 'actor2', 
    symmetric = FALSE, mode = 'bipartite', weight = NULL, diag_to_NA = FALSE
  )
  
  # add dyad variables
  a_matrix <- add_dyad_vars(a_matrix, fake_dyads, 'actor1', 'actor2', NULL, 'var1', FALSE)
  
  dyad_data <- attr(a_matrix, 'dyad_data')
  
  # check dimensions are correct for bipartite (2x3 matrix)
  expect_equal(dim(dyad_data[["1"]][["var1"]]), c(2, 3))
  expect_equal(rownames(dyad_data[["1"]][["var1"]]), paste0("A", 1:2))
  expect_equal(colnames(dyad_data[["1"]][["var1"]]), paste0("B", 1:3))
})

# large sparse network performance
test_that('add_dyad_vars: sparse network efficiency', {
  
  # create sparse network data (only few edges in large network)
  actors <- paste0("actor", 1:100)
  fake_dyads <- expand.grid(actor1 = actors, actor2 = actors)
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads <- fake_dyads[fake_dyads$actor1 != fake_dyads$actor2,]
  fake_dyads$var1 <- rnorm(nrow(fake_dyads))
  
  # this should run efficiently
  start_time <- Sys.time()
  
  a_matrix <- get_adjacency(
    dyad_data = fake_dyads,
    actor1 = 'actor1', actor2 = 'actor2', 
    symmetric = FALSE, weight = NULL, diag_to_NA = FALSE
  )
  
  a_matrix <- add_dyad_vars(a_matrix, fake_dyads, 'actor1', 'actor2', NULL, 'var1', FALSE)
  
  end_time <- Sys.time()
  
  # should complete quickly and have correct structure
  expect_lt(as.numeric(end_time - start_time), 5)  # less than 5 seconds
  expect_equal(dim(attr(a_matrix, 'dyad_data')[["1"]][["var1"]]), c(100, 100))
})

# automatic variable detection
test_that('add_dyad_vars: automatic variable detection', {
  
  fake_dyads <- expand.grid(actor1 = letters[1:3], actor2 = letters[1:3])
  fake_dyads$actor1 <- as.character(fake_dyads$actor1)
  fake_dyads$actor2 <- as.character(fake_dyads$actor2)
  fake_dyads <- fake_dyads[fake_dyads$actor1 != fake_dyads$actor2,]
  fake_dyads$var1 <- rnorm(nrow(fake_dyads))
  fake_dyads$var2 <- rnorm(nrow(fake_dyads))
  fake_dyads$extra_col <- "ignore"
  
  a_matrix <- get_adjacency(dyad_data = fake_dyads, actor1 = 'actor1', actor2 = 'actor2', 
                           symmetric = FALSE, weight = NULL, diag_to_NA = FALSE)
  
  # don't specify dyad_vars - should auto-detect
  a_matrix <- add_dyad_vars(a_matrix, fake_dyads, 'actor1', 'actor2', NULL, 
                      dyad_vars = NULL, dyad_vars_symmetric = NULL)
  
  dyad_data <- attr(a_matrix, 'dyad_data')
  
  # should include var1, var2, extra_col but not actor1, actor2
  expect_true("var1" %in% names(dyad_data[["1"]]))
  expect_true("var2" %in% names(dyad_data[["1"]]))
  expect_true("extra_col" %in% names(dyad_data[["1"]]))
  expect_false("actor1" %in% names(dyad_data[["1"]]))
  expect_false("actor2" %in% names(dyad_data[["1"]]))
})