#' Decimal to rgb
#'
#' @description Convert decimal colour code to rgb colour code
#' @param c Decimal color code
#'
#' @return The output from \code{\link{dec2rgb}}
#' @export
#'
#' @examples
#' dec2rgb(16753920)
#' \dontrun{
#' dec2rgb("16777215")
#' }
#'
dec2rgb <- function(c){

  b = floor(c %% 256)
  g_0 = (c %% 65536 - b)
  r_0 = c - g_0 - b
  g = floor(g_0 / 256)
  r = floor(r_0 / 65536)
  return(abs(c(r, g, b)))
}

