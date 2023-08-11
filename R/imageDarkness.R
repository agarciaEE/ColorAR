#' Image darkness
#'
#' @description Extract image darkness
#'
#' @param image RGB raster image
#'
#' @return The output from \code{\link{imageDarkness}}
#' @export
#' @importFrom raster as.data.frame
#' @importFrom stats na.exclude
#' @examples
#' library(ColorAR)
#' data(imgTransList)
#' img <- imgTransList[[1]]
#' imgDarkness <- imageDarkness(img)
#' \dontrun{
#' library(ColorAR)
#' data(imgTransList)
#' img <- imgTransList[[2]]
#' imgDarkness<- imageDarkness(img)
#' }
imageDarkness <- function(image){

  df <- na.exclude(raster::as.data.frame(image)) # get the RGB color codes
  d <- lapply(1:3, function(i) density(df[,i])) # compute density on distances
  idx = lapply(1:3, function(i) which(d[[i]]$x >= 0 & d[[i]]$x <= 255))
  y = lapply(1:3, function(i) d[[i]]$y[idx[[i]]]/sum(d[[i]]$y[idx[[i]]]))
  x = lapply(1:3, function(i) 1-(d[[i]]$x[idx[[i]]]/255))
  darkness = mean(sapply(1:3, function(i) sum(y[[i]]*x[[i]])))

  return(darkness)
}
