#' Vectorize images to data frame
#'
#' @param imgList List of images
#' @param res resolution desired (nrows). Default is NULL/
#' @param fill.NAs logical. Whether to fill NAs with mean values to avoid loss of data.
#' @param type whether images are "RGB", "decimal" or "raster" images
#'
#' @return data.frame
#' @export
#' @importFrom stats na.exclude
#' @importFrom raster nrow ncol nlayers extent raster resample as.data.frame stack mean crs ncell
#'
#' @examples
#' library(ColorAR)
#' data(imgTransList)
#' imgdf <-  vectorize.images(imgTransList)
#'
vectorize.images <- function(imgList, res = NULL, fill.NAs = FALSE,
                       type = c("RGB", "decimal", "raster")){


  out = list()
  type = type[1]
  if(raster::nlayers(imgList[[1]]) != 3 & type %in% c("RGB", "decimal")){
    warning("imgList is not in RGB format, using type 'raster'", immediate. = T)
    type = "raster"
  }
  if(raster::nlayers(imgList[[1]]) == 3 & type == "raster"){
    warning("imgList is in RGB format, using type 'RGB'", immediate. = T)
    type = "RGB"
  }
  for (n in 1:length(imgList)) {
    r = imgList[[n]]
    NR <- raster::nrow(r)
    NC <- raster::ncol(r)
    if (is.integer(interpolate)) {
      ras = as.vector(raster::extent(r))
      rRe <- raster::raster(nrow=NR,ncol=NC)
      raster::extent(rRe) <- ras
      r = sp::disaggregate(r, fact = interpolate, method = "bilinear")
      r = raster::resample(r, rRe, method = "ngb")
    }
    if(!is.null(res)){
      e = as.vector(raster::extent(r))
      ratio = (e[2]-e[1])/(e[4]-e[3])
      ras = e
      rRe <- raster::raster(nrow=res,ncol=floor(res*ratio))
      raster::crs(rRe) = NA
      raster::extent(rRe) <- ras
      r = raster::resample(r, rRe, method = "ngb")
    }
    if(type == "RGB"){
      if (n == 1) {
        rasDF <- raster::as.data.frame(as.vector(r))
      }
      else {
        rasDF <- cbind(rasDF, raster::as.data.frame(as.vector(r)))
      }
    }
    else {
      if(type == "decimal"){
        r = r[[1]]*256^2 + r[[2]]*256 + r[[3]]
      }
      if (n == 1) {
        rasDF <- raster::as.data.frame(r)
      }
      else {
        rasDF <- cbind(rasDF, raster::as.data.frame(r))
      }
    }
  }
  colnames(rasDF) = names(imgList)
  if (fill.NAs){
    mean_img <- raster::stack(raster::mean(raster::stack(lapply(imgList, function(i) i[[1]])), na.rm = T),
                              raster::mean(raster::stack(lapply(imgList, function(i) i[[2]])), na.rm = T),
                              raster::mean(raster::stack(lapply(imgList, function(i) i[[3]])), na.rm = T))
    if(type == "RGB"){
      mean_img_df <- raster::as.data.frame(as.vector(r))
    } else {
      mean_img_df <- raster::as.data.frame(mean_img)
    }
    rs <- rowSums(rasDF)
    NA.cells <- which(is.na(rs) & !is.na(mean_img_df))
    rasDF[NA.cells, ] <- mean_img_df[NA.cells, ]
  }
  rasDF = stats::na.exclude(rasDF)
  rw.val = rownames(rasDF)
  df = t(rasDF)

  return(df)
}


