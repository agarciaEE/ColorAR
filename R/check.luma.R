#' @title Check color luminosity
#'
#' @param col Target color
#'
#' @return The output from \code{\link{print}}
#' @export
#'
#' @examples
#' check.luma("red")
#' \dontrun{
#' check.luma("blue")
#' }
#'
check.luma <- function(col){
  col = as.numeric(grDevices::col2rgb(col, alpha = FALSE))
  luma = 0.2126 * col[1] + 0.7152 * col[2] + 0.0722 * col[3]
  return(luma)
}
