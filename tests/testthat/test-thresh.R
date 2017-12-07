test_that("Check for threshold not > max of data", {

  counts = matrix(rpois(1440*5, lambda = 5), ncol = 1440)
  max_counts = max(counts)
  expect_error(frag(counts, thresh.lower = max_counts + 1))

})
