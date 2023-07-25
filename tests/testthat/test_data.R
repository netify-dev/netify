# example data loading checks


test_that('valid data.frame loaded for icews', {

  # load data
  data(icews)

  # check class
  expect_equal(class(icews), 'data.frame')
})
