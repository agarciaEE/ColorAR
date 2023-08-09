#' Image contrast
#'
#' @description Extract image contrast
#'
#' @param image RGB raster image
#'
#' @return The output from \code{\link{imageContrast}}
#' @export
#' @importFrom stats na.exclude
#' @importFrom ape rtree
#' @importFrom raster ncol nrow as.data.frame
#' @examples
#' img <- imgTransList[[1]]
#' imgContrast <- imageContrast(img)
#' \dontrun{
#' img <- imgTransList[[2]]
#' imgContrast<- imageContrast(img)
#' }
imageContrast <- function(image){

  df <- stats::na.exclude(raster::as.data.frame(image)) # get the RGB color codes
  Y = apply(df, 1, function(i) 0.2126 * i[1] + 0.7152 * i[2] + 0.0722 * i[3])
  contrast = sqrt(sum(((Y- mean(Y))/mean(Y))^2)/(raster::ncol(image)*raster::nrow(image)))

  return(contrast)
}
