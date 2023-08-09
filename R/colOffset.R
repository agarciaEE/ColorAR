#' Color Offset
#'
#' @description estimate best color offset for classifying colors into categories
#'
#' @param image RGB raster stack or list of RGB raster stacks
#' @param RGB Vector of RGB target color (numeric)
#' @param min Minimum offset
#'
#' @return The output from \code{\link{colOffset}}
#' @export
#' @importFrom raster stack raster nlayers as.data.frame unstack
#' @importFrom stats na.exclude quantile density
#' @importFrom jpeg readJPEG
#' @importFrom scales rescale
#' @examples
#' img <- jpeg::readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))
#' img <- raster::stack(sapply(1:3, function(i) raster::raster(scales::rescale(img[,,1], to = c(0,255)))))
#' colOffset(img, c(0, 255, 255))
#' \dontrun{
#' img <- jpeg::readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))
#' img <- raster::stack(sapply(1:3, function(i) raster::raster(scales::rescale(img[,,1], to = c(0,255)))))
#' imgList <- list(img, img, img)
#' colOffset(imgList, c(0, 255, 255))
#' }
colOffset <- function(image, RGB, min = 0.1){

  if(!is.numeric(RGB)){
    stop("RGB is not a numeric vector.")
  }
  if (class(image) %in% c("RasterStack", "RasterBrick") & raster::nlayers(image) == 3) {
    df = as.matrix(stats::na.exclude(raster::as.data.frame(image)))
  }
  if (is.list(image)) {
    df <- lapply(image, function(x) raster::unstack(x))
    df <- lapply(df, function(x)  setNames(x, c("R", "G", "B")))
    df = reverse.list(df)
    df <- as.matrix(sapply(df, function(x) rowMeans(raster::as.data.frame(stack(x)), na.rm = T)))
  }
  dec <- df[,1]*256^2 + df[,2]*256 + df[,3]
  decRGB <- RGB[1]*256^2 + RGB[2]*256 + RGB[3]
  tm <- table(dec)
  d <- as.numeric(names(tm)[tm > (mean(tm) + sd(tm)) ])
  if(any(d == decRGB)){
    message("Target colour within colour candidates")
    d <- abs(d-decRGB)/256^3
    dens <- stats::density(d, bw = 0.05)
    dens$y[dens$y < stats::quantile(dens$y, 0.25)] = stats::quantile(dens$y, 0.25)
    dens$y <- dens$y[dens$x >= 0]
    dens$x <- dens$x[dens$x >= 0]
    Offset <- dens$x[which.min(dens$y)]
  }
  else {
    warning("Target colour outside colour candidates", immediate. = T)
    peaks <- color.candidates(df)
    max.dist <- min(apply(peaks, 1, function(i) mean(sapply(1:3, function(j) abs(i[j]-RGB[j])/255))))
    df <- do.call(rbind, lapply(d, function(x) dec2rgb(x)))
    candidate <- df[which.min(apply(df, 1, function(i) mean(sapply(1:3, function(j) abs(i[j]-RGB[j]))))),]
    dist <- mean(sapply(1:3, function(i) abs(candidate[i]-RGB[i])/255))
    message("Most proximal candidate colour is: ")
    cat(candidate, "\n")
    if(dist > max.dist){
      warning("Most proximal candidate color is out of target color range. Setting target color offset to minimum")
      Offset = min
    }
    else{
      d <- abs(d-decRGB)/256^3
      dens <- stats::density(d, bw = 0.05)
      dens$y[dens$y < stats::quantile(dens$y, 0.25)] = stats::quantile(dens$y, 0.25)
      dens$y <- dens$y[dens$x >= 0]
      dens$x <- dens$x[dens$x >= 0]
      Offset <- dens$x[which.min(dens$y)]
    }
  }
  return(Offset)
}
