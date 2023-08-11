#' Color proportions
#'
#' @description Extract color and compute color proportion
#'
#' @param image RGB raster image
#' @param RGB n rows by 3 columns data.frame with target colors to classify. each column must be R, G and B color values, whereas numer of rows will be number of target colors. Default is NULL.
#' @param colOffset  n target colors vector of numeric values between 0 and 1. Color offset threshold to assign the target color. Default = NULL. If NULL, bg.offset will be estimated
#' @param resampleFactor Integer for downsampling used by redRes. Default = NULL
#' @param crop Logical whether to crop image
#' @param cropOffset Desired image extent
#' @param focal Whether to perform Gaussian blurring. Default = FALSE.
#' @param sigma Size of sigma for Gaussian blurring. Default = 3.
#' @param colOffset Numeric value between 0 and 1. If NULL, colOffset will be estimated. color offset threshold to classify as target color. Default is NULL.
#' @param plot Wheather plot classyfied image or not. Default is FALSE.
#' @param interpolate Integer. Image interpolation to reduce NA values created by image transformation. Default = NULL.
#' @param output type of output desired. If 'value', gives the proportion of the target color relative to the rest of color of the image. If 'image' gives a raster with values from 0 or 1 whether the target color is present in the image.
#'
#' @return The output from \code{\link{extractColor}}
#' @export
#' @importFrom raster extent crop raster focal stack crs resample focalWeight as.array values
#' @importFrom patternize redRes
#' @importFrom sp disaggregate
#' @importFrom graphics par
#' @examples
#' img <- imgTransList[[1]]
#' targetColor <- c(255, 165, 0)
#' Orange_proportion <- extractColor(img, targetColor)
#' \dontrun{
#' img <- imgTransList[[2]]
#' targetColor <- c(255, 255, 255)
#' White_proportion <- extractColor(img, targetColor)
#' }
extractColor <-  function(image, RGB,  resampleFactor = NULL, crop = F,  cropOffset = c(0, 0, 0, 0),
                          focal = F, sigma = 3, colOffset =NULL, plot = F, interpolate = NULL, output = c("both", "value", "image")){

  output = output[1]
  out = list()
  extRasterOr <- raster::extent(image)
  if(is.null(colOffset)){ colOffset <- colOffset(image, RGB)}
  if (!is.null(resampleFactor)) {
    image <- patternize::redRes(image, resampleFactor)
  }
  if (crop) {
    imageC <- raster::crop(image, cropOffset)
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
  if (is.numeric(interpolate)){
    rRe <- raster::raster(nrow=dim(image)[1],ncol=dim(image)[2])
    raster::crs(rRe) = raster::crs(image)
    raster::extent(rRe) <-  extent(image)
    image = sp::disaggregate(image, fact = interpolate, method = "bilinear")
    image = raster::resample(image, rRe, method = "ngb")
  }
  map <- apply(raster::as.array(image), 1:2, function(x) all(abs(x - RGB) < colOffset * 255))
  mapR <- raster::raster(map)
  raster::extent(mapR) = raster::extent(image)
  if (plot == "result") {
    plot(mapR)
  }
  if (plot == "compare") {
    graphics::par(mfrow = c(1, 2))
    raster::plotRGB(image)
    plot(mapR, box = F, axes = F)
  }
  if(output %in% c("value", "both")) {
    out$P <- sum(raster::values(mapR) != 0, na.rm = T)/sum(!is.na(raster::values(mapR)))
  }
  if(output %in% c("image", "both")) {
    out$ras <- mapR
  }
  return(out)
}
