#' Image perceived lightness
#'
#' @description Extract image perceived lightness
#'
#' @param image RGB raster image
#' @param output type of output desired. If 'value', gives the average perceived lightness of the image. If 'raster' gives a raster with values from 0 to 1 with the perceived lightness value.
#'
#' @return The output from \code{\link{imageLightness}}
#' @export
#' @importFrom raster as.data.frame
#' @importFrom stats na.exclude
#' @examples
#' img <- imgTransList[[1]]
#' imgLightness <- imageLightness(img)
#' \dontrun{
#' img <- imgTransList[[2]]
#' imgLightness <- imageLightness(img)
#' }
imageLightness <-  function(image, output = c("value", "raster", "both")) {

  output <- output[1]
  if (output != "raster"){
    df <- stats::na.exclude(raster::as.data.frame(image/255))
    ldf = t(apply(df, 1, function(i) sapply(1:3, function(j) if (i[j] <= 0.04045) { i[j]/12.92} else { ((i[j] + 0.055)/1.055)^2.4})))
    Y <-  (0.2126 * ldf[,1] + 0.7152 * ldf[,2] + 0.0722 * ldf[,3])
    Lstar <- sapply(Y, function(i) if (i <= 216/243389) {i * 24389/27} else { i^(1/3)*116-16})
    Lstar.m <-  mean(Lstar)
  }
  if (output != "value"){
    r <- image
    for (i in 1:3){
      r[[i]][image[[i]] <= 0.04045] = image[[i]][image[[i]] <= 0.04045]/12.92
      r[[i]][image[[i]] > 0.04045] = ((image[[i]][image[[i]] > 0.04045] + 0.055)/1.055)^2.4
    }
    Y <- (0.2126 * r[[1]] + 0.7152 * r[[2]] + 0.0722 * r[[3]])
    Lstar <-  Y
    Lstar[Y <= 216/243389] <- Y[Y <= 216/243389] * 24389/27
    Lstar[Y > 216/243389] <- Y[Y > 216/243389] ^(1/3)*116-1
    names(Lstar) = "Lightness"
  }
  if (output == "value"){ return(Lstar.m) }
  if (output == "raster"){ return(Lstar) }
  if (output == "both"){ return(list(Lstar = Lstar.m, ras = Lstar)) }
}
