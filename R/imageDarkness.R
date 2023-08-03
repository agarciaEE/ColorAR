#' Image darkness
#'
#' @description Extract image darkness
#'
#' @param image RGB raster image
#'
#' @return The output from \code{\link{print}}
#' @export
#' @import smoothr sp raster Morpho patternize
#' @examples
#' tree <- ape::rtree(26, tip.label = letters[1:26])
#' X <- data.frame(trait1 = runif(26, -10, 10), trait2 = runif(26, -25, 25))
#' plotPhylomorphospace(tree, X)
#' \dontrun{
#' plotPhylomorphospace(tree, X, palette = rainbow(6), col.branches = T)
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
