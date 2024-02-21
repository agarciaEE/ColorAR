#' Image Landmark Transformation
#'
#' @description Process images to align using procrustes analyses based on landmarks and remove background
#'
#' @param sampleList list of images
#' @param landList list of landmarks
#' @param adjustCoords Adjust landmark coordinates ib nase to pixel coordinates. Default = TRUE
#' @param transformRef Shape reference for the procrustes  analyses. Default = "meanshape". Can also be a matrix of coordinates or a landmark list reference.
#' @param crop Logical whether to crop image
#' @param cropOffset vector of proportion of left, right, bottom and top margins to crop image
#' @param res resample image with a different resolution. Default = 300. To simplify images into lower resolutions and speeding up the transformation. It may be useful in color adjacency analyses and PCA to reduce biases made by isolated pixel changes
#' @param keep.ASP  keep aspect ratio image. Default = TRUE. If FALSE, resampling will transform the image to res by res matrix
#' @param drop vector of indexes to drop when masking the outline if removebg.by is set to "landmarks".
#' @param removebg.by Removes background based on target color in argument 'bgcol' ("color") or landmark refshape ("landmarks"). Default = NULL
#' @param smooth Numeric value, When removebg.by = "landmarks", Performs a smooth kernel tansformation of the refshape. Default NULL
#' @param rescale If output image resolution is to be as input image resolution. Default = FALSE,
#' @param resampleFactor Integer for downsampling used by redRes. Default = NULL
#' @param transformType Transformation type as used by computeTransform. Default ='tps'
#' @param focal Whether to perform Gaussian blurring. Default = FALSE.
#' @param sigma Size of sigma for Gaussian blurring. Default = 3.
#' @param interpolate Integer. Image interpolation to reduce NA values created by image transformation. Default = NULL.
#' @param bgcol RGB code for detecting and remoovig background color. Default = NULL
#' @param bg.offset numeric value between 0 and 1. If NULL, bg.offset will be estimated. color offset threshold to remove background color
#' @param plot If "compare", plots original image vs transformed. If "result", plot restuling transformed image. Default = FALSE
#' @param save Logical, whether to save transformend raster images. Default = FALSE
#' @param dir  If save is TRUE, directory name to save the transformed raster images in. if non-existent, it will create the directory. Default = current directory.
#' @param overwrite Logical. Whether overwrite the existent transformed images found in 'dir'. If TRUE, it will overwrite along the way. If FALSE, it will transform only those images that are not present in the given directory. Default is FALSE.
#'
#' @return The output from \code{\link{imageTransformation}}
#' @export
#' @importFrom smoothr smooth
#' @importFrom sp SpatialPolygons Polygons Polygon disaggregate
#' @importFrom raster extent crop raster resample focal focalWeight stack as.data.frame plotRGB mask flip crs rasterize
#' @importFrom Morpho procSym computeTransform applyTransform
#' @importFrom patternize lanArray redRes
#' @importFrom utils capture.output
#' @importFrom graphics par
#'
#' @examples
#' library(ColorAR)
#'
#' samples <- sub(".png", "", list.files(system.file("extdata", "pictures",
#'                                         package="ColorAR"), pattern = "\\.png$"))[1:5]
#'
#' prepath <- system.file("extdata", "pictures", package="ColorAR")
#' imgList <- lapply(file.path(prepath, paste0(samples, ".png")), raster::stack)
#'
#' prepath <- 'system.file("extdata", "landmarks", package="ColorAR")
#' landList <- lapply(file.path(prepath, paste0(samples, ".txt")), utils::read.table)
#'
#' names(imgList) <- names(landList) <- samples
#'
#' imgTransList = imageTransformation(imgList, landList,
#'                                    adjustCoords = TRUE, transformRef = "meanshape",
#'                                    drop = lndmks_2drop, crop = FALSE,
#'                                    cropOffset = c(0, 0, 0, 0), res = 300,
#'                                    keep.ASP  = TRUE, removebg.by = "color", bgcol = c(0,165,255),
#'                                    smooth = 1, rescale = TRUE, transformType = "tps",
#'                                    focal = FALSE, sigma = 3, interpolate =  5,
#'                                    plot = "compare")
#'
imageTransformation <- function(sampleList, landList, adjustCoords = F, transformRef = "meanshape",
                                crop = FALSE, cropOffset = c(0, 0, 0, 0), res = 300, keep.ASP = T, drop = NULL,
                                removebg.by = c(FALSE, "color", "landmarks"), smooth = FALSE, rescale = F, resampleFactor = NULL,
                                transformType = "tps", focal = F, sigma = 3, interpolate = NULL,
                                bgcol = NULL, bg.offset = NULL, plot = FALSE, save = FALSE, dir = "./", overwrite = FALSE) {

  removebg.by = removebg.by[1]
  rasterList <- list()
  if (length(sampleList) != length(landList)) {
    stop("sampleList is not of the same length as lanArray")
  }
  for (n in 1:length(sampleList)) {
    if (names(sampleList)[n] != names(landList)[n]) {
      stop("samples are not in the same order in sampleList and lanArray")
    }
  }
  lanArray <- patternize::lanArray(landList, adjustCoords = adjustCoords, sampleList)
  if (is.matrix(transformRef)) {
    refShape <- transformRef
  }
  if (!is.matrix(transformRef)) {
    if (transformRef == "meanshape") {
      invisible(utils::capture.output(transformed <- Morpho::procSym(lanArray)))
      refShape <- transformed$mshape
    }
    if (transformRef %in% names(landList)) {
      e <- which(names(landList) == transformRef)
      refShape <- lanArray[, , e]
    }
  }
  if (removebg.by == "landmarks"){
    if(is.null(drop)){
      drop = 1:nrow(refShape)
    }
    sp.ref = sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(refShape[-drop,])), 1)))
    if (is.numeric(smooth)){
      sp.ref <- smoothr::smooth(sp.ref, method = "ksmooth", smooth = smooth)
    }
  }
  if (save & overwrite) {
    files <- gsub("\\.tif", "", list.files(dir))
    idx <- which(names(sampleList) %in% files)
    message(length(idx), "images already present in the directory as transformed and thus, removed from the task. Modify parameters if is not the case.")
    sampleList <- sampleList[-idx]
    landList <- landList[-idx]
  }
  for (n in 1:length(sampleList)) {
    image <- sampleList[[n]]
    extRasterOr <- raster::extent(image)
    if (!is.null(resampleFactor)) {
      image <- patternize::redRes(image, resampleFactor)
    }
    if (crop) {
      landm <- lanArray[, , n]
      extRaster <- raster::extent(min(landm[, 1]) - min(landm[,1]) * cropOffset[1]/100,
                                  max(landm[, 1]) + max(landm[,1]) * cropOffset[2]/100,
                                  min(landm[, 2]) - min(landm[,2]) * cropOffset[3]/100,
                                  max(landm[, 2]) + max(landm[,2]) * cropOffset[4]/100)
      imageC <- raster::crop(image, extRaster)
      y <- raster::raster(ncol = dim(image)[2], nrow = dim(image)[1])
      raster::extent(y) <- extRasterOr
      image <- raster::resample(imageC, y)
    }
    if (focal) {
      gf <- raster::focalWeight(image, sigma, "Gauss")
      rrr1 <- raster::focal(image[[1]], gf)
      rrr2 <- raster::focal(image[[2]], gf)
      rrr3 <- raster::focal(image[[3]], gf)
      image <- raster::stack(rrr1, rrr2, rrr3)
    }
    mapDF <- raster::as.data.frame(image, xy = TRUE)
    invisible(utils::capture.output(transMatrix <- Morpho::computeTransform(refShape,as.matrix(lanArray[,,n]), type = "tps")))
    invisible(utils::capture.output(mapTransformed <- Morpho::applyTransform(as.matrix(mapDF[,1:2]),transMatrix)))
    ASP = 1
    if (keep.ASP == T){
      ext = as.vector(raster::extent(image))
      ASP = (ext[2]-ext[1])/(ext[4]-ext[3])
    }
    rRe <- raster::raster(ncol = res*ASP, nrow = res)
    e = raster::extent(min(refShape[,1]) - 3 * max(refShape[,1]) * cropOffset[3]/100,
                       max(refShape[,1]) + 3 * max(refShape[,1]) * cropOffset[4]/100,
                       min(refShape[,2]) - 3 * max(refShape[, 2]) * cropOffset[1]/100,
                       max(refShape[,2]) + 3 * max(refShape[, 2]) * cropOffset[2]/100)
    margin = c((e[2] - e[1]) * 0.05, (e[4] - e[3]) * 0.05)
    raster::extent(rRe) = raster::extent(e[1]-margin[1],
                                         e[2]+margin[1],
                                         e[3]-margin[2],
                                         e[4]+margin[2])

    imgTransformed <- raster::stack(sapply(names(image), function(x) raster::rasterize(mapTransformed,
                                                                               field = mapDF[,x], rRe)))
    if (removebg.by == "landmarks"){
      message("Removing background based on landmarks reference...")
      imgTransformed <- raster::mask(imgTransformed, sp.ref)
    }
    if (removebg.by == "color"){
      message("Removing background based on color...")
      imgTransformed <- removebg(imgTransformed, bgcol = bgcol, bg.offset = bg.offset, plot = F)
    }
    if (transformRef == "meanshape") {
      imgTransformed = raster::flip(raster::flip(imgTransformed, "x"), "y")
    }
    if (rescale){
      rRe <- raster::raster(nrow=dim(image)[1],ncol=dim(image)[2])
      raster::crs(rRe) = raster::crs(image)
      raster::extent(rRe) <-  raster::extent(image)
      raster::extent(imgTransformed) = raster::extent(image)
      imgTransformed = raster::stack(raster::resample(imgTransformed, rRe, method = "ngb"))
    }
    if (is.numeric(interpolate)){
      imgTransformed = sp::disaggregate(imgTransformed, fact = interpolate, method = "bilinear")
      imgTransformed = raster::resample(imgTransformed, rRe, method = "ngb")
    }
    if (plot == "result") {
      if(nlayers(imgTransformed) == 3){raster::plotRGB(imgTransformed)}
      if(nlayers(imgTransformed) == 1){plot(imgTransformed)}
    }
    if (plot == "compare") {
      graphics::par(mfrow = c(1, 2))
      if(nlayers(imgTransformed) == 3){
        raster::plotRGB(image)
        text(raster::extent(image)[2], raster::extent(image)[3], "original", adj = c(1,0))
        raster::plotRGB(imgTransformed, asp = 1)
        text(raster::extent(imgTransformed)[2], raster::extent(imgTransformed)[3], "transformed", adj = c(1,0))
      }
      if(nlayers(imgTransformed) == 1){
        plot(image)
        text(raster::extent(image)[2], raster::extent(image)[3], "original", adj = c(1,0))
        plot(imgTransformed)
        text(raster::extent(imgTransformed)[2], raster::extent(imgTransformed)[3], "transformed", adj = c(1,0))
      }
    }
    if (save) {
      if (!dir.exists(dir)) { dir.create(dir) }
      raster::writeRaster(imgTransformed, filename = file.path(dir, paste0(names(landList)[n], ".tif")))
    }
    rasterList[[names(landList)[n]]] <- imgTransformed
    print(paste("sample", names(landList)[n], "transformation done and added to rasterList",
                sep = " "))
  }
  return(rasterList)
}
