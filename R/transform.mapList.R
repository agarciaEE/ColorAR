#' Create image mapList
#'
#' @description Internal function to transform images for PCA legend.
#'
#' @param imgList a list of images.
#' @param mapList legend list.
#' @param res resolution.
#' @param type wheter RGB or raster format.
#' @param is.NA value to give to NA values.
#' @param interpolate Integer for the interpolateing factor.
#'
#' @return The output from \code{\link{print}}
#' @import raster
#'
#' @examples
#' tree <- ape::rtree(26, tip.label = letters[1:26])
#' X <- data.frame(trait1 = runif(26, -10, 10), trait2 = runif(26, -25, 25))
#' plotPhylomorphospace(tree, X)
#' \dontrun{
#' plotPhylomorphospace(tree, X, palette = rainbow(6), col.branches = T)
#' }
transform.mapList <- function(imgList, mapList, res = NULL, type  = c("RGB", "raster"), is.NA = 0, interpolate = NULL){

  ras = raster::extent(imgList[[1]])
  rRe <- raster::raster(nrow=nrow(imgList[[1]]),ncol=ncol(imgList[[1]]))
  crs(rRe) = NA
  raster::extent(rRe) <- ras
  for (i in 1:length(mapList)){
    if(!is.null(res)){
      e = as.vector(extent(mapList[[i]]))
      ratio = (e[2]-e[1])/(e[4]-e[3])
      ras = e
      trRe <- raster::raster(nrow=res,ncol=floor(res*ratio))
      crs(trRe) = NA
      raster::extent(trRe) <- ras
      mapList[[i]] = raster::resample(mapList[[i]], trRe, method = "ngb")
    }
    raster::extent(mapList[[i]]) <- raster::extent(imgList[[1]])
    if (is.numeric(interpolate)) {
      mapList[[i]] = raster::disaggregate(mapList[[i]], fact = interpolate, method = "bilinear")
    }
    mapList[[i]] = raster::resample(mapList[[i]], rRe, method = "ngb")
  }
  if(type == "RGB"){
    mean.image <- raster::stack(sapply(1:3, function(x) raster::mean(raster::stack(sapply(imgList, function(i) i[[x]])), na.rm = T)))
    for (n in 1:length(mapList)){
      if(raster::nlayers(mapList[[n]]) == 3){
        if (length(is.NA) != 3){ is.NA <- rep(is.NA[1], 3)}
        if(n == 1){
          for (i in 1:3){
            mapList[[n]][[i]][is.na(mapList[[n]][[i]]) & !is.na(mean.image[[i]])] = is.NA[i]
          }
        }
        else{
          mapList[[n]] <- raster::stack(sapply(1:3, function(i) raster::mean(mapList[[1]][[i]], mapList[[n]][[i]], mapList[[n]][[i]], na.rm = T)))
        }
      }
      else{
        mapList[[n]][is.na(mapList[[n]]) & !is.na(mean.image[[1]])] = is.NA
      }
    }
  }
  if(type == "raster"){
    mean.image = raster::mean(raster::stack(imgList))
    for (n in 1:length(mapList)){
      mapList[[n]][is.na(mapList[[n]]) & !is.na(mean.image)] = is.NA
    }
  }
  return(mapList)
}