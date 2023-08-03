#' Image saturation and intensity
#'
#' @description Extract image saturation and intensity
#'
#' @param image RGB raster image
#' @param output type of output desired. If 'value', gives the average color saturation and intensity  of the image. If 'raster' gives a raster with values from 0 to 1 with the saturation and intensity value.
#'
#' @return The output from \code{\link{print}}
#' @export
#' @import raster stats
#' @examples
#' tree <- ape::rtree(26, tip.label = letters[1:26])
#' X <- data.frame(trait1 = runif(26, -10, 10), trait2 = runif(26, -25, 25))
#' plotPhylomorphospace(tree, X)
#' \dontrun{
#' plotPhylomorphospace(tree, X, palette = rainbow(6), col.branches = T)
#' }
imageSI <- function(image, output = c("value", "raster", "both")){

  if (output != "raster"){
    df <- stats::na.exclude(raster::as.data.frame(image))
    for (i in 1:3) if(df[which(df[,i] == max(df[,i])),i] > 255){df[which(df[,i] == max(df[,i])),i] <-  255}
    hsi.df <- t(rgb2hsv(t(df)))
    S.mean <- mean(hsi.df[,2])
    I.mean <- mean(hsi.df[,3])
    values <- c(Saturation = S.mean, Intensity =  I.mean)
  }
  if (output != "value"){
    df <- raster::as.data.frame(image, xy = T)
    r <-  cbind(df[,1:2], NA, NA, NA)
    for (i in 3:5) if(df[which(df[,i] == max(df[,i], na.rm = T)),i] > 255){ df[which(df[,i] == max(df[,i], na.rm = T)),i] <-  255 }
    r[!is.na(rowSums(df[,3:5])),3:5] <-  t(apply(df[!is.na(rowSums(df[,3:5])),3:5], 1, function(i) rgb2hsv(as.numeric(i))))
    S <- raster::rasterFromXYZ(cbind(df[,1:2],r[,4]))
    I <-  raster::rasterFromXYZ(cbind(df[,1:2],r[,5]))
    ras <-  stack(S, I)
    names(ras) = c("Saturation", "Intensity")
  }
  if (output == "value"){return(values)}
  if (output == "raster"){return(ras)}
  if (output == "both"){return(list(values = values, ras = ras))}
}
