testthat::test_that("Testing imageTransformation function", {

  library(ColorAR)

  ## Load images
  filenames <- sub(".png", "", list.files("inst/extdata/pictures/", pattern = "\\.png$"))

  prepath <- 'inst/extdata/pictures'
  extension <- '.png'
  imageList <- lapply(file.path(prepath, paste0(filenames, extension)), raster::stack)

  prepath <- 'inst/extdata/landmarks'
  extension <- '.txt'
  landmarkList <- lapply(file.path(prepath, paste0(filenames, extension)), utils::read.table)

  names(imageList) <- names(landmarkList) <- filenames

  lndmks_2drop  = c(2, 7, 28, 29)

  imgTransList = imageTransformation(imageList[1:5], landmarkList[1:5], adjustCoords = T, transformRef = "meanshape", drop = lndmks_2drop,
                                      crop = FALSE, cropOffset = c(0, 0, 0, 0), res = 300, keep.ASP  = T,
                                      removebg.by = "landmarks", smooth = 1, rescale = T,
                                      transformType = "tps", focal = F, sigma = 3, interpolate =  5,
                                      plot = "compare")

  testthat::expect_equal(class(imgTransList), "list")
  testthat::expect_equal(length(imgTransList), length(imageList))
  testthat::expect_equal(as.numeric(sapply(imgTransList, raster::nlayers)), rep(3, length(imageList)))
  testthat::expect_equal(names(imgTransList), names(landmarkList))

})

