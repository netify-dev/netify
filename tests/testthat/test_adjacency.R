set.seed(6886)

test_that(
	'get_adjacency: cross-sectional, asymmetric, weighted network', {

  # create data
  asym_weight_df <- expand.grid(
		actor1=letters[1:3],actor2=letters[1:3],stringsAsFactors=FALSE)
  asym_weight_df$value <- rnorm(nrow(asym_weight_df))
  asym_weight_df <- asym_weight_df[asym_weight_df$actor1!=asym_weight_df$actor2,]

  # create matrix
  asym_weight_matrix <- matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3],letters[1:3]))
  for(ii in 1:nrow(asym_weight_df)){
    asym_weight_matrix[asym_weight_df$actor1[ii],asym_weight_df$actor2[ii]] = asym_weight_df$value[ii] }

  # get adjacency
  a_matrix <- get_adjacency(
    dyad_data=asym_weight_df,
    actor1='actor1', actor2='actor2',
    weight='value', symmetric=FALSE,
    diag_to_NA=FALSE )

  # the test
  expect_identical(get_raw(a_matrix), asym_weight_matrix)
  })

test_that(
  'get_adjacency: cross-sectional, asymmetric, weighted network, sum dyads', {

  # create data
  asym_weight_df <- expand.grid(
    actor1=letters[1:3],actor2=letters[1:3],stringsAsFactors=FALSE)
  asym_weight_df$value <- rnorm(nrow(asym_weight_df))
  asym_weight_df <- asym_weight_df[asym_weight_df$actor1!=asym_weight_df$actor2,]
  asym_weight_agg_df <- aggregate(
    value ~ actor1 + actor2, data = asym_weight_df, sum)

  # create matrix
  asym_weight_matrix <- matrix(
    0, nrow=3, ncol=3, 
    dimnames=list(letters[1:3],letters[1:3]))
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
    diag_to_NA=FALSE )

  # the test
  expect_identical(get_raw(a_matrix), asym_weight_matrix)
  })

test_that(
	'get_adjacency: cross-sectional, asymmetric, non-weighted network', {

  # create data that is cross-sectional, asymmetric, and non-weighted
  asym_non_weight_df <- expand.grid(actor1=letters[1:3],actor2=letters[1:3],stringsAsFactors=FALSE)
  asym_non_weight_df$value <- rbinom(nrow(asym_non_weight_df), 1, .5)
  asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$actor1!=asym_non_weight_df$actor2,]

  # filter out zero responses, equivalent to an event dataset
  asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$value>0,]

  # create matrix
  asym_non_weight_matrix <- matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3],letters[1:3]))
  for(ii in 1:nrow(asym_non_weight_df)){
    asym_non_weight_matrix[asym_non_weight_df$actor1[ii],asym_non_weight_df$actor2[ii]] = asym_non_weight_df$value[ii] }

  # get adjacency
  a_matrix <- get_adjacency(
    dyad_data=asym_non_weight_df,
    actor1='actor1', actor2='actor2',
    weight=NULL, symmetric=FALSE,
    diag_to_NA=FALSE )

  # the test
  expect_identical(get_raw(a_matrix), asym_non_weight_matrix)
  })

test_that(
	'get_adjacency: cross-sectional, asymmetric, non-weighted network, sum dyads', {

  # create data that is cross-sectional, asymmetric, and non-weighted
  asym_non_weight_df <- expand.grid(
    actor1=letters[1:3],actor2=letters[1:3],stringsAsFactors=FALSE)
  asym_non_weight_df$value <- rbinom(nrow(asym_non_weight_df), 1, .7)
  asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$actor1!=asym_non_weight_df$actor2,]
  asym_non_weight_df <- rbind(
    asym_non_weight_df, 
    data.frame('actor1'='a','actor2'='b','value'=1))

  # filter out zero responses, equivalent to an event dataset
  asym_non_weight_df <- asym_non_weight_df[asym_non_weight_df$value>0,]

  # create matrix
  asym_non_weight_matrix <- matrix(
    0, nrow=3, ncol=3, dimnames=list(letters[1:3],letters[1:3]))
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
    diag_to_NA=FALSE )

  # the test
  expect_identical(get_raw(a_matrix), asym_non_weight_matrix)
})

# cross-sectional, symmetric weighted network
test_that(
	'get_adjacency: cross-sectional, symmetric, weighted network', {

  # create symmetric dat
  actor1 <- c('a','a','b', 'b', 'c','c')
  actor2 <- c('b', 'c','a','c','b','a')
  value <- c(2,4,2,7,7,4)
  sym_weight_df <- data.frame(actor1=actor1,actor2=actor2, value = value)
  sym_weight_df <- sym_weight_df[sym_weight_df$actor1!=sym_weight_df$actor2,]

  # create matrix
  sym_weight_matrix <- matrix(
    0, nrow=3, ncol=3, dimnames=list(letters[1:3],letters[1:3]))
  for(ii in 1:nrow(sym_weight_df)){
    old_edge_val = sym_weight_matrix[sym_weight_df$actor1[ii],sym_weight_df$actor2[ii]]
    new_edge_val = sym_weight_df$value[ii] 
    sym_weight_matrix[sym_weight_df$actor1[ii],sym_weight_df$actor2[ii]] = old_edge_val + new_edge_val }
  sym_weight_matrix <- sym_weight_matrix + t(sym_weight_matrix)

  # get adjacency
  a_matrix <- get_adjacency(
    dyad_data=sym_weight_df,
    actor1='actor1', actor2='actor2',
    weight='value', symmetric=TRUE,
    sum_dyads = TRUE,
    diag_to_NA=FALSE )

  # the test
  expect_identical(get_raw(a_matrix), sym_weight_matrix)
})

## cross-sectional, symmetric weighted network + sum_dyads
test_that(
    'get_adjacency: cross-sectional, symmetric, weighted network, sum dyads', {

  ## create symmetric dat
  actor1 <- c('a','a','b', 'b', 'c','c','a')
  actor2 <- c('b', 'c','a','c','b','a','b')
  value <- c(2,4,2,7,7,4,3)
  sym_weight_df <- data.frame(
    actor1=actor1,actor2=actor2, value = value)
  sym_weight_df <- sym_weight_df[sym_weight_df$actor1!=sym_weight_df$actor2,]
  
  # gen symmetric aggregated weighted dyadic data
  sym_weight_agg_df <- aggregate(
    sym_weight_df$value, 
    by=list(
      apply(
        sym_weight_df[,1:2],1,
        function(x) {paste(sort(x),collapse='_')})),
    FUN=sum, simplify=TRUE )
  sym_weight_agg_df$actor1 <- unlist(
    lapply(
      strsplit(
        sym_weight_agg_df$Group.1,'_'),function(x) {x[1]}))
  sym_weight_agg_df$actor2 <- unlist(
    lapply(
      strsplit(
        sym_weight_agg_df$Group.1,'_'),function(x) {x[2]}))

  ## create matrix
  sym_weight_matrix <- matrix(
    0, nrow=3, ncol=3, 
    dimnames=list(letters[1:3],letters[1:3]))
  for(ii in 1:nrow(sym_weight_agg_df)){
      sym_weight_matrix[sym_weight_agg_df$actor1[ii],sym_weight_agg_df$actor2[ii]] = sym_weight_agg_df$x[ii]
      sym_weight_matrix[sym_weight_agg_df$actor2[ii],sym_weight_agg_df$actor1[ii]] = sym_weight_agg_df$x[ii] }

  ## get adjacency
  a_matrix <- get_adjacency(
      dyad_data=sym_weight_df,
      actor1='actor1', actor2='actor2',
      sum_dyads = TRUE,
      weight='value', symmetric=TRUE,
      diag_to_NA=FALSE )

  ## the test
  expect_identical(get_raw(a_matrix), sym_weight_matrix)
  })

# cross-sectional, symmetric non-weighted network
test_that(
	'get_adjacency: cross-sectional, symmetric, non-weighted network', {

  # create symmetric dat
  actor1 <- c('a','a','b', 'b', 'c','c')
  actor2 <- c('b', 'c','a','c','b','a')
  value <- c(1,0,1,1,1,0)
  sym_non_weight_df <- data.frame(actor1=actor1,actor2=actor2, value = value)
  sym_non_weight_df <- sym_non_weight_df[sym_non_weight_df$actor1!=sym_non_weight_df$actor2,]

  # filter out zero responses, equivalent to an event dataset
  sym_non_weight_df <- sym_non_weight_df[sym_non_weight_df$value>0,]

  # create matrix
  sym_non_weight_matrix <- matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3],letters[1:3]))
  for(ii in 1:nrow(sym_non_weight_df)){
    sym_non_weight_matrix[sym_non_weight_df$actor1[ii],sym_non_weight_df$actor2[ii]] = sym_non_weight_df$value[ii] }

  # get adjacency
  a_matrix <- get_adjacency(
		dyad_data=sym_non_weight_df,
    actor1='actor1', actor2='actor2',
    weight=NULL, symmetric=TRUE,
    diag_to_NA=FALSE
  )

  # the test
  expect_identical(get_raw(a_matrix), sym_non_weight_matrix)
})

# cross-sectional, symmetric non-weighted network sum dyads
# note that this is a strange case because the user is noting
# the data is symmetric but the data.frame itself has 
# repeating dyads like a-b and b-a, which we end up
# counting up as separate interactions. in the case
# of event data this might be reasonable but not 
# for every other case
test_that(
	'get_adjacency: cross-sectional, symmetric, non-weighted network, sum dyads', {

  # create symmetric dat
  actor1 <- c('a','a','b', 'b', 'c','c', 'a', 'b')
  actor2 <- c('b', 'c','a','c','b','a', 'b', 'a')
  value <- c(1,0,1,1,1,0,1, 1)
  sym_non_weight_df <- data.frame(actor1=actor1,actor2=actor2, value = value)
  sym_non_weight_df <- sym_non_weight_df[sym_non_weight_df$actor1!=sym_non_weight_df$actor2,]

  # filter out zero responses, equivalent to an event dataset
  sym_non_weight_df <- sym_non_weight_df[sym_non_weight_df$value>0,]

  # create matrix
  sym_non_weight_matrix <- matrix(0, nrow=3, ncol=3, dimnames=list(letters[1:3],letters[1:3]))
  for(ii in 1:nrow(sym_non_weight_df)){
    old_val <- sym_non_weight_matrix[sym_non_weight_df$actor1[ii],sym_non_weight_df$actor2[ii]]
    new_val <- old_val + sym_non_weight_df$value[ii]
    sym_non_weight_matrix[sym_non_weight_df$actor1[ii],sym_non_weight_df$actor2[ii]] = new_val }

  # get adjacency
  a_matrix <- get_adjacency(
		dyad_data=sym_non_weight_df,
      actor1='actor1', actor2='actor2',
      sum_dyads=TRUE,
    weight=NULL, symmetric=TRUE,
    diag_to_NA=FALSE
  )

  # the test
  expect_identical(get_raw(a_matrix), sym_non_weight_matrix*2)
})
