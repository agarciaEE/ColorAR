testthat::test_that("Testing imagePCA function", {

  library(ColorAR)

  ## Perform PCA on transformed images
  imgPCA12 <- imagePCA(imgTransList, PCx = 1, PCy = 2, scale = F, plot.eigen = F, plot.PCA = F,
                            interpolate = 5, plot.names = T, plot.images = T, plot.tree = NULL, type = "RGB" , as.RGB = F)


  testthat::expect_equal(class(imgPCA12), "list")
  testthat::expect_equal(length(imgPCA12), 7)

})
