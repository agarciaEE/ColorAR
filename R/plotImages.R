#' Plot images
#'
#' @description plot images into specified coordinates
#'
#' @param x a vector of x coordinates.
#' @param y a vector of y coordinates.
#' @param images a list of images.
#' @param width widht of the image.
#' @param height height of the image.
#' @param interpolate Logical whether to interpolate the image. Default = FALSE.
#' @param names a vector of names to display along the image. Default is 'NULL'.
#' @param cex text size.
#' @param pos text position.
#' @param adj text adjustment.
#' @param cols vector of colors to use if images are rasterLayers.
#'
#' @return The output from \code{\link{print}}
#' @export
#' @import raster graphics grDevices
#'
#' @examples
#' img <- jpeg::readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))
#' img <- img <- raster::stack(sapply(1:3, function(i) raster::raster(scales::rescale(img[,,1], to = c(0,255)))))
#' imgList <- list(img, img, img)
#' x <- rep(mean(par()$usr[1:2]),3)
#' y <- seq(par()$usr[3], par()$usr[4], length.out = 3)
#' plot.new()
#' plotImages(x, y, imgList, names = letters[1:3], pos = 2, adj = 1)
#' \dontrun{
#' plot.new()
#' plotImages(x, y, imgList, names = letters[1:3], pos = 2, adj = 1)
#' }
plotImages <- function(x, y, images, width = 0.1, height = width, interpolate = FALSE, names = NULL, cex = 1, pos = 1, adj = 1,
                        cols  = c("red", "grey90", "blue")){

  cols = grDevices::colorRampPalette(cols)(n=100)
  stopifnot(length(x) == length(y))
  if(length(images) < length(x)){
    images <- replicate(length(x), images, simplify=FALSE)
  }
  if(!is.null(names) && length(names) < length(images)){
    names <- rep(names, length(images))
  }
  if(length(pos) < length(images)){
    pos <- rep(pos, length(images))
  }
  if(length(width) < length(x)){
    width <- rep(width, length(images))
  }
  if(length(height) < length(x)){
    height <- rep(height, length(images))
  }
  width = width+width*diff(range(x))
  height = height+height*diff(range(y))
  for (ii in seq_along(x)){
    if (class(images[[ii]]) %in% c("RasterStack", "RasterBrick") && dim(images[[ii]])[3] == 3) {
      images[[ii]] = sapply(1:3, function(i) images[[ii]][[i]]/255)
      images[[ii]] = as.array(raster::stack(append(images[[ii]], images[[ii]][[1]])))
      images[[ii]][,,4][!is.na(images[[ii]][,,4])] = 1
      for (i in 1:4) {
        images[[ii]][,,i][is.na(images[[ii]][,,i])] = 0
        images[[ii]][,,i][images[[ii]][,,i] >1] = 1
        images[[ii]][,,i][images[[ii]][,,i] <0] = 0
      }
    }
    if (is.array(images[[ii]]) && dim(images[[ii]])[3] == 4){
      graphics::rasterImage(images[[ii]], xleft=x[ii] - 0.5*width[ii],
                  ybottom= y[ii] - 0.5*height[ii],
                  xright=x[ii] + 0.5*width[ii],
                  ytop= y[ii] + 0.5*height[ii], interpolate=interpolate)
    }
    if (class(images[[ii]]) == "RasterLayer") {
      e = as.vector( raster::extent(images[[ii]]))
      ratio = (e[2]-e[1])/(e[4]-e[3])
      raster::extent(images[[ii]]) =  c(x[ii] - 0.5*width[ii], x[ii] + 0.5*width[ii], y[ii] - 0.5*height[ii]*ratio, y[ii] + 0.5*height[ii]*ratio)
      graphics::image(images[[ii]], interpolate=interpolate, add = T, legend = F, col = cols)
    }
    if(!is.null(names)){
      #mtext(names[ii],side=pos[ii],col="black", cex = cex)
      graphics::text(x[ii], y[ii], names[ii], pos = pos, adj = adj)
    }
  }
}
