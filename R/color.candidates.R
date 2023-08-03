#' Color candidates
#'
#' @param x Image in format 'RasterStack', 'RasterBrick' or data frame with 3 columns represnting the RGB channels
#' @param n Optional. Number of candidates to search for.
#'
#' @description Explore main colour candidates on an image.
#' @return The output from \code{\link{print}}
#' @export
#' @import raster stats jpeg
#' @examples
#' img <- jpeg::readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))
#' img <- sapply(1:3, function(i) scales::rescale(img[,,1], to = c(0,255)))
#' color.candidates(img)
#' \dontrun{
#' img <- jpeg::readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))
#' img <- raster::stack(sapply(1:3, function(i) raster::raster(scales::rescale(img[,,1], to = c(0,255)))))
#' color.candidates(img, 3)
#' }
color.candidates <- function(x, n){

  if(class(x)[1] %in% c("RasterStack", "RasterBrick")){
    df = stats::na.exclude(raster::as.data.frame(x))
  }
  else if(is.matrix(x) & ncol(x) == 3){
    df <- x
  }
  else {
    stop("x is not a RGB image nor RGB color data frame")
  }
  dec <- df[,1]*256^2 + df[,2]*256 + df[,3]
  peaks <- as.numeric(names(sort(table(dec)[table(dec) > (mean(table(dec))+sd(table(dec)))], decreasing = T)))
  if(missing(n)){
    dens <- stats::density(dec)
    d.peaks <- dens$x[which(diff(sign(diff(dens$y)))<0)+1]
    n <- length(d.peaks)
    message(n, " color candidates...")
    peaks <- peaks[sapply(d.peaks, function(i) which.min(abs(i-peaks)))]
  }
  else {
    peaks <- peaks[1:n]
  }
  cols <- t(as.data.frame(sapply(peaks, function(i) dec2rgb(i)), row.names = c("R", "G", "B")))
  return(cols)
}

