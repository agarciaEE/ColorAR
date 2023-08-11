testthat::test_that("Testing some ColorAR functions", {

  library(ColorAR)

  Orange_proportion <- extractColor(imgTransList[[1]], c(255, 165, 0))
  testthat::expect_equal(class(Orange_proportion), "list")
  testthat::expect_equal(as.vector(sapply(Orange_proportion, class)), c("numeric", "RasterLayer"))

  Orange_intensity <- extractIntensity(imgTransList[[1]], c(255, 165, 0))
  testthat::expect_equal(class(Orange_intensity), "list")
  testthat::expect_equal(as.vector(sapply(Orange_intensity, class)), c("numeric", "RasterLayer"))

  imgL <- imageLightness(imgTransList[[1]])
  testthat::expect_equal(class(imgL), "numeric")
  testthat::expect_equal(round(imgL,5), 42.88915)

  imgD <- imageDarkness(imgTransList[[1]])
  testthat::expect_equal(class(imgD), "numeric")
  testthat::expect_equal(round(imgD,7), 0.6196719)

  imgC <- imageContrast(imgTransList[[1]])
  testthat::expect_equal(class(imgC), "numeric")
  testthat::expect_equal(round(imgC,7), 0.6765744)

  RGB = data.frame(red = c(255, 255, 0),
                   green = c(255, 165, 0),
                   blue = c(255, 0, 0),
                   row.names = c("white", "orange", "black"))
  imgClass <- classifyColor(imgTransList[[1]], RGB = RGB, allow.admixture = T, output = "both")

  testthat::expect_equal(class(imgClass), "list")
  testthat::expect_equal(class(imgClass$codes), c("matrix", "array"))
  testthat::expect_equal(nrow(imgClass$codes), 7)
  testthat::expect_equal(raster::nlayers(imgClass$RGB), 3)
  testthat::expect_equal(raster::maxValue(imgClass$class), 7)

  CA_df <- col.adjacency(imgClass$class, bckgr = 0)
  testthat::expect_equal(class(CA_df), "list")
  testthat::expect_equal(class(CA_df$values), "numeric")
  testthat::expect_equal(as.numeric(CA_df$values),  c(0.170317496, 0.298579590, 0.513611267, 0.007910464, 0.009581183,  0.000000000,  0.000000000, 2.617056148,  0.373865164, 12.525295380,  0.596442637))

})

