testthat::test_that("Testing colorAR", {

  library(ColorAR)

  set.seed(123)
  tree <- ape::rtree(26, tip.label = letters[1:26])
  nodes <- getAncestors(tree, 5)

  testthat::expect_equal(class(nodes), "integer")
  testthat::expect_equal(length(nodes), 5)

})
