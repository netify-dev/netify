# library(testthat)
# library(netify)
# devtools::load_all('~/Research/netify_dev/netify/')

set.seed(6886)

test_that(
    "new_netify works with a cross-sectional matrix (unipartite, weighted)", {

    # create a 10 x 10 matrix with integer values
    m <- matrix(sample(1:200, 100, replace = TRUE), nrow = 10, ncol = 10)
    net_obj <- new_netify(m)

    # check the basics
    expect_s3_class(net_obj, "netify")
    expect_equal(attr(net_obj, "netify_type"), "cross_sec")

    # 10x10 with same row/col => unipartite
    expect_equal(attr(net_obj, "mode"), "unipartite")

    # auto-detect diagonal usage
    # with random data, we likely won't have all NAs on diag => diag_to_NA=FALSE
    # also typically missing_to_zero=TRUE if we find NAs off diagonal, else FALSE
    # but we can just check that the attributes exist:
    expect_false(attr(net_obj, "diag_to_NA"))
    # depends on if there's no missing => so presumably missing_to_zero=FALSE
    expect_true(attr(net_obj, "missing_to_zero"))

    # random data => not symmetric => expect FALSE
    expect_false(attr(net_obj, "symmetric"))

    # random data => not strictly binary => weight_binary=FALSE
    expect_false(attr(net_obj, "weight_binary"))

    # check row/col names assigned if none existed
    # auto-labeled them "a1","a2",..., if none were present
    msrs <- netify_measurements(net_obj)
    expect_equal(msrs$row_actors, paste0("a", 1:10))
    expect_equal(msrs$col_actors, paste0("a", 1:10))

    # test forced diag_to_NA
    m_diag_na <- m
    diag(m_diag_na) <- NA
    net_obj_diag_na <- new_netify(m_diag_na, diag_to_NA = TRUE)
    expect_true(attr(net_obj_diag_na, "diag_to_NA"))

    # test forced symmetric
    m_sym <- m + t(m)  # make it symmetric
    diag(m_sym) <- NA  # also set diag NA
    net_obj_sym <- new_netify(m_sym, symmetric = TRUE)
    expect_true(attr(net_obj_sym, "symmetric"))
    expect_true(attr(net_obj_sym, "diag_to_NA"))

    # test binary
    m_bin <- m_sym
    # threshold around mean => 1 or 0
    m_bin[m_sym > mean(m_sym, na.rm = TRUE)] <- 1
    m_bin[m_sym <= mean(m_sym, na.rm = TRUE)] <- 0
    net_obj_bin <- new_netify(m_bin)
    expect_true(attr(net_obj_bin, "weight_binary"))

    # test bipartite => e.g., 7 x 10 matrix
    m_bi <- matrix(sample(1:200, 70, replace = TRUE), nrow = 7, ncol = 10)
    net_obj_bi <- new_netify(m_bi)
    expect_equal(attr(net_obj_bi, "mode"), "bipartite")
})


# longit array input: Use a larger 3D array with at least 10 rows/cols per slice
test_that("new_netify works with large array input", {
  
    # Create a 3D array with dimensions 10 (rows) x 10 (cols) x 3 (time slices)
    a <- array(sample(1:1000, 10 * 10 * 3, replace = TRUE), dim = c(10, 10, 3))
    net_obj <- new_netify(a)

    # Check that attributes are set as expected
    expect_s3_class(net_obj, "netify")
    expect_equal(attr(net_obj, "netify_type"), "longit_array")  
    expect_true(attr(net_obj, "actor_time_uniform"))
    expect_equal(attr(net_obj, "mode"), "unipartite")
    expect_false(attr(net_obj, 'diag_to_NA'))
    expect_true(attr(net_obj, 'missing_to_zero'))
    expect_false(attr(net_obj, 'symmetric'))
    expect_false(attr(net_obj, 'weight_binary'))

    # check internal measurements
    msrs = netify_measurements(net_obj)
    expect_equal(
        msrs$row_actors, paste0('a', 1:10) )
    expect_equal(
        msrs$col_actors, paste0('a', 1:10) )
    expect_equal(
        msrs$time, as.character(1:3) )

    # check for NAs in off-diagonal elements
    a_w_miss <- a
    a_w_miss[1, 2, 1] <- NA
    net_obj_miss <- new_netify(a_w_miss)
    expect_false(attr(net_obj_miss, "diag_to_NA"))
    expect_false(attr(net_obj_miss, "missing_to_zero"))

    # make diags NA in a
    a_diag_NA <- a
    for(ii in 1:dim(a)[3]){ diag(a_diag_NA[,,ii]) <- NA }
    net_obj_diag_NA <- new_netify(a_diag_NA, diag_to_NA = TRUE)
    expect_true(attr(net_obj_diag_NA, "diag_to_NA"))
    expect_true(attr(net_obj_diag_NA, "missing_to_zero"))

    # make a symmetric and diags NA
    a_sym <- a
    for(i in 1:dim(a)[3]){
      a_sym[,,i] <- a[,,i] + t(a[,,i])
      diag(a_sym[,,i]) <- NA }
    net_obj_sym <- new_netify(a_sym, symmetric = TRUE)
    expect_true(attr(net_obj_sym, "symmetric"))
    expect_true(attr(net_obj_sym, "diag_to_NA"))

    # make a_sym binary
    a_sym_binary <- a_sym
    a_sym_binary[a_sym > mean(c(a_sym), na.rm=TRUE)] <- 1
    a_sym_binary[a_sym <= mean(c(a_sym), na.rm=TRUE)] <- 0
    net_obj_sym_binary <- new_netify(a_sym_binary)
    expect_true(attr(net_obj_sym_binary, "weight_binary"))

    # make a bipartite, uneven dims
    a_bipartite <- a[1:5, 6:9, ]
    net_obj_bipartite <- new_netify(a_bipartite)
    expect_equal(attr(net_obj_bipartite, "mode"), "bipartite")

    # make bipartite, different row/col labels
    a_bipartite <- a[1:5, 6:10, ]
    rownames(a_bipartite) <- paste0("r", 1:5)
    colnames(a_bipartite) <- paste0("c", 1:5)
    net_obj_bipartite_labels <- new_netify(a_bipartite)
    expect_equal(attr(net_obj_bipartite_labels, "mode"), "bipartite")    
})

test_that(
    "new_netify works with a list of matrices (longit_list)", {
  
    # create a list of 3 cross-sectional matrices, each 10x10
    mat_list <- lapply(1:3, function(x) {
        matrix(sample(1:300, 100, replace = TRUE), nrow = 10, ncol = 10) })

    # no names => new_netify should label them t1, t2, t3
    net_obj <- new_netify(mat_list)

    # check that attributes are set as expected
    expect_s3_class(net_obj, "netify")
    expect_equal(attr(net_obj, "netify_type"), "longit_list")

    # if row/col dims are all 10x10 with the same row/col sets => actor_time_uniform=TRUE
    # but let's see if new_netify auto-detects
    # by default, each matrix is random => not symmetric => so we expect net_obj is unsymmetric
    expect_false(attr(net_obj, "symmetric"))

    # check the naming of each element => t1, t2, t3
    expect_equal(names(net_obj), as.character(1:3))

    # check row/col naming => by default unipartite => "a1"... "a10"
    # check the first slice
    msrs <- netify_measurements(net_obj)
    expect_equal(length(msrs$row_actors[[1]]), 10)
    expect_equal(length(msrs$col_actors[[1]]), 10)

    # if the row/col sets are identical across all slices, actor_time_uniform => TRUE
    # but let's see
    # we can test forced: net_obj2 <- new_netify(mat_list, actor_time_uniform=FALSE)

    # test forcibly bipartite => e.g. first slice is 6x10, second is 6x10, third is 6x10
    mat_list_bi <- lapply(1:3, function(i) {
        matrix(sample(1:300, 60, replace = TRUE), nrow = 6, ncol = 10) })
    net_obj_bi <- new_netify(mat_list_bi)
    expect_equal(attr(net_obj_bi, "mode"), "bipartite")

    # test symmetry => combine each matrix with its transpose
    mat_list_sym <- lapply(mat_list, function(m) {
        m_sym <- m + t(m)
        diag(m_sym) <- NA
        m_sym })
    net_obj_sym <- new_netify(mat_list_sym, symmetric = TRUE)
    expect_true(attr(net_obj_sym, "symmetric"))

    # test binary => threshold each slice
    mat_list_binary <- lapply(mat_list_sym, function(m) {
        thresh <- mean(m, na.rm = TRUE)
        m[m > thresh] <- 1
        m[m <= thresh] <- 0
        m })
    net_obj_bin <- new_netify(mat_list_binary)
    expect_true(attr(net_obj_bin, "weight_binary"))

    # test missing => set random NA off diag
    mat_list_miss <- mat_list
    mat_list_miss[[1]][1,5] <- NA
    net_obj_miss <- new_netify(mat_list_miss)
    # by default, a single NA might not force missing_to_zero unless user sets it
    expect_false(attr(net_obj_miss, "missing_to_zero"))

    # forcibly diag_to_NA => check first slice
    mat_list_diag_na <- mat_list
    diag(mat_list_diag_na[[1]]) <- NA
    net_obj_diag_na <- new_netify(mat_list_diag_na, diag_to_NA = TRUE)
    expect_true(attr(net_obj_diag_na, "diag_to_NA"))
})

# 
test_that(
    "new_netify handles partial dimnames in array", {

    arr <- array(sample(1:50, 5 * 5 * 2, replace=TRUE), dim=c(5,5,2))

    # give row names, but not col names
    dimnames(arr)[[1]] <- paste0("row", 1:5)

    net_obj <- new_netify(arr)
    expect_s3_class(net_obj, "netify")
    # check that colnames are auto-labeled while rownames remain as "row1...row5"
    expect_equal(dimnames(net_obj)[[1]], paste0("row", 1:5))
    # e.g. colnames should have default "a1...a5" if unipartite
    expect_equal(dimnames(net_obj)[[2]], paste0("a", 1:5))
})

test_that(
    "new_netify respects user overrides over auto-detection", {

    m_sym <- matrix(sample(1:50, 25, replace=TRUE), 5, 5)
    # forcibly make it symmetrical
    m_sym <- m_sym + t(m_sym)
    diag(m_sym) <- NA

    # auto-detect => symmetric=TRUE, but we override
    net_obj_override <- new_netify(m_sym, symmetric=FALSE)
    # confirm that we see the user override, not auto-detection
    expect_false(attr(net_obj_override, "symmetric"))
})

# check for screwy dimensions :)
test_that(
    "new_netify handles single-row or single-column matrices", {

    m_single_row <- matrix(1:5, nrow=1, ncol=5)
    net_obj_row <- new_netify(m_single_row)
    expect_s3_class(net_obj_row, "netify")
    # likely mode => bipartite (since nrow != ncol)
    expect_equal(attr(net_obj_row, "mode"), "bipartite")
    # check dimnames => row= "r1", col= "c1..c5" if bipartite
    expect_equal(rownames(net_obj_row), "r1")
    expect_equal(colnames(net_obj_row), paste0("c", 1:5))
})

#
test_that(
    "new_netify respects user-supplied extra attributes", {

    m <- matrix(1:9, 3, 3)
    net_obj <- new_netify(m, my_custom_attr="hello", another_attr=123)
    expect_equal(attr(net_obj, "my_custom_attr"), "hello")
    expect_equal(attr(net_obj, "another_attr"), 123)
})


#
test_that(
    "new_netify can store nodal_data/dyad_data attributes", {

    m <- matrix(1:9, 3, 3)
    sample_nodal <- data.frame(id=1:3, some_node_attr=letters[1:3])

    net_obj <- new_netify(m, nodal_data=sample_nodal)
    expect_equal(attr(net_obj, "nodal_data"), sample_nodal)
})

test_that("new_netify handles longit_list with varying actor sets (dimensions) across slices", {
  
  # create a list with different sized matrices in each slice
  # first slice 5x5, second 7x7, third 5x5 again
  set.seed(6886)
  mat_list_vary <- list(
    matrix(sample(1:300, 25, replace=TRUE), nrow=5, ncol=5),
    matrix(sample(1:300, 49, replace=TRUE), nrow=7, ncol=7),
    matrix(sample(1:300, 25, replace=TRUE), nrow=5, ncol=5)
  )

  # pass it to new_netify
  net_obj_vary <- new_netify(mat_list_vary)

  # basic checks
  expect_s3_class(net_obj_vary, "netify")
  expect_equal(attr(net_obj_vary, "netify_type"), "longit_list")

  # since slice sizes differ, we expect actor_time_uniform = FALSE
  expect_false(attr(net_obj_vary, "actor_time_uniform"))

  # check that the slices got named t1, t2, t3
  expect_equal(names(net_obj_vary), as.character(1:3))

  # ensure each slice is class netify with cross_sec type
  # and has assigned row/col names
  for (i in seq_along(net_obj_vary)) {
    slice_i <- net_obj_vary[[i]]
    expect_s3_class(slice_i, "netify")
    expect_equal(attr(slice_i, "netify_type"), "cross_sec")
    # row/col names should exist
    expect_false(is.null(rownames(slice_i)))
    expect_false(is.null(colnames(slice_i)))
  }
})

