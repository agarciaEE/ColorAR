## code to prepare `DATASET` dataset goes here
## Load images
filenames <- sub(".png", "", list.files("inst/extdata/pictures/", pattern = "\\.png$"))

prepath <- 'inst/extdata/pictures'
extension <- '.png'
imageList <- lapply(file.path(prepath, paste0(filenames, extension)), raster::stack)

prepath <- 'inst/extdata/landmarks'
extension <- '.txt'
landmarkList <- lapply(file.path(prepath, paste0(filenames, extension)), utils::read.table)

names(imageList) <- names(landmarkList) <- filenames

dataset <- data.frame(sample = filenames, species = "Amphiprion clarkii", region = sub("[A-Z][a-z]*[0-9]*", "", filenames))
dataset$region <- factor(dataset$region, levels = c("WI", "NWI", "CIP", "CP", "SWP"))

set.seed(12345)
tree <- ape::rtree(length(imageList))
tree$tip.label <- rev(dataset$sample[order(dataset$region)])

imgTransList = imageTransformation(imageList, landmarkList, adjustCoords = T, transformRef = "meanshape",
                                    crop = FALSE, cropOffset = c(0, 0, 0, 0), res = 300, keep.ASP  = T,
                                    removebg.by = "color", bgcol =  c(0,174,255), smooth = 0.8, rescale = T,
                                    transformType = "tps", focal = F, sigma = 3, interpolate =  5,
                                    plot = "compare")

imgPCA12 <-  imagePCA(imgTransList, PCx = 1, PCy = 2, scale = F, plot.eigen = F, plot.PCA = F,
                      interpolate = 5, plot.names = F, plot.images = F, plot.tree = NULL, type = "RGB" , as.RGB = F)


usethis::use_data(dataset, overwrite = TRUE)
usethis::use_data(landmarkList, overwrite = TRUE)
usethis::use_data(imageList, overwrite = TRUE)
usethis::use_data(imgTransList, overwrite = TRUE)
usethis::use_data(tree, overwrite = TRUE)
usethis::use_data(imgPCA12, overwrite = TRUE)
