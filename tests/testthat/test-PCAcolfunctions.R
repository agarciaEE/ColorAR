testthat::test_that("Testing imageTransformation function", {

  library(ColorAR)

  imgPCA12 <-  imagePCA(imgTransList, PCx = 1, PCy = 2, scale = F, plot.eigen = F, plot.PCA = F,
           interpolate = 5, plot.names = F, plot.images = F, plot.tree = NULL, type = "RGB" , as.RGB = F)
  testthat::expect_equal(class(imgPCA12), "list")
  testthat::expect_equal(names(imgPCA12), c("images", "df", "pca",  "cellIDs", "ras", "type", "components"))

  regcols <- setNames(rep(viridis::inferno(5))[as.factor(dataset$region)], dataset$sample)

  imagePCA.plot(imgPCA12, tree = tree, plot.tree = "integrated", plot.images = F)

  imagePCA.plot.decomposed(imgPCA12, tree, plot.images = T, PCA.cols = regcols, tree.cols = regcols)

  sideplot <- imagePCA.RGBsideplot(imgPCA12)
  testthat::expect_equal(class(sideplot), "list")
  testthat::expect_equal(names(sideplot), c("center", "minPC1", "maxPC1", "minPC2", "maxPC2"))
  testthat::expect_equal(as.vector(sapply(sideplot, class)), rep("RasterStack", 5))
  testthat::expect_equal(as.vector(sapply(sideplot, raster::nlayers)), rep(3, 5))

  spca.coords <- data.frame(imgPCA12$pca$x[1:5,])
  rPCAphen <- reconstruct.PCAphenotype(spca.coords, imgPCA12)

  testthat::expect_equal(class(rPCAphen), "list")
  testthat::expect_equal(as.vector(sapply(rPCAphen, class)), rep("RasterBrick", 5))


})
