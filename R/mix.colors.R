#' Mix colors
#'
#' @description Mix multiple colours into one.
#' @param cols Vector of colors.
#'
#' @return The output from \code{\link{mix.colors}}
#' @export
#' @importFrom DescTools MixColor
#' @importFrom viridis viridis
#'
#' @examples
#' mix.colors(rainbow(9))
#' \dontrun{
#' mix.colors(viridis::viridis(3))
#' }
mix.colors <- function(cols){
  if (length(cols) == 1){
    return(cols)
  }
  else{
    x <- NULL
    for ( i in 2:length(cols)){
      x <- c(x, DescTools::MixColor(cols[i], cols[i-1]))
      if (length(x) == 2){
        x <- DescTools::MixColor(x[1],x[2])
      }
    }
    return(x)
  }
}
