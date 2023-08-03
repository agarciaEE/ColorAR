#' Image contrast
#'
#' @description Extract image contrast
#'
#' @param image RGB raster image
#'
#' @return The output from \code{\link{print}}
#' @export
#' @import stats raster
#' @examples
#' tree <- ape::rtree(26, tip.label = letters[1:26])
#' X <- data.frame(trait1 = runif(26, -10, 10), trait2 = runif(26, -25, 25))
#' plotPhylomorphospace(tree, X)
#' \dontrun{
#' plotPhylomorphospace(tree, X, palette = rainbow(6), col.branches = T)
#' }
imageContrast <- function(image){

  df <- stats::na.exclude(raster::as.data.frame(image)) # get the RGB color codes
  Y = apply(df, 1, function(i) 0.2126 * i[1] + 0.7152 * i[2] + 0.0722 * i[3])
  contrast = sqrt(sum(((Y- mean(Y))/mean(Y))^2)/(raster::ncol(image)*raster::nrow(image)))

  return(contrast)
}
