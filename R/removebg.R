#' Remove background
#'
#' @param image raster image in RGB format
#' @param bgcol vector of RGB color code to remove. If 'NULL', the user has to identify the colors selecting the target colors on the image
#' @param bg.offset color offset to remove the background. If 'NULL', bg.offset is computed using colOffset function.
#' @param plot Logical.
#'
#' @description ## remove the background color of an image using color detection.
#' @return The output from \code{\link{print}}
#' @export
#' @import raster stats jpeg
#' @examples
#' img <- jpeg::readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))
#' img <- raster::stack(sapply(1:3, function(i) raster::raster(scales::rescale(img[,,1], to = c(0,255)))))
#' removebg(img, bgcol = c(255, 255, 255), bg.offset = 0.1)
#' \dontrun{
#' img <- jpeg::readJPEG(system.file("img", "Rlogo.jpg", package="jpeg"))
#' img <- raster::stack(sapply(1:3, function(i) raster::raster(scales::rescale(img[,,1], to = c(0,255)))))
#' removebg(img, c(133,133,133))
#' }
removebg <- function(image, bgcol = NULL, bg.offset = NULL, plot = F){

  if (is.null(bgcol)){
    raster::plotRGB(image)
    message("Select the target colours on the image and press [esc] to continue.")
    reference <- as.data.frame(graphics::locator(type = "p", col = "red"))
    #if(all(res(image) != 1)){
    #  e = as.vector(extent(image))
    #  dims = dim(image)
    #  reference = data.frame(x = (reference[1]-e[1])/(e[2]-e[1])*dims[2],
    #                         y = (reference[2]-e[3])/(e[4]-e[3])*dims[1])
    #}
    #imgdat <- raster::as.array(image)
    bgcol <- try(do.call(rbind, lapply(seq_len(nrow(reference)),
                                       function(x) as.data.frame(raster::extract(image, reference)))), silent = TRUE)

    #bgcol <- try(do.call(rbind, lapply(seq_len(nrow(reference)),
    #                                   function(x) as.data.frame(t(imgdat[reference$x[x],
    #                                                                      reference$y[x], 1:3])))), silent = TRUE)
    message("RGB Color background selected: ")
    cat(paste(bgcol), "\n")
  }
  if(is.numeric(bgcol)){
    bgcol = t(as.data.frame(bgcol))
  }
  nbg <- nrow(bgcol)
  if (is.null(bg.offset)){
    bg.offset <-  sapply(1:nbg, function(i) colOffset(image, as.numeric(bgcol[i,])))
    message("Color background offset: ")
    print(data.frame(bgcol, offset = bg.offset))
  }
  imgDF <- raster::as.data.frame(image, xy = TRUE)
  for (i in 1:nbg){
    imgDF[apply(imgDF[3:5], 1, function(x) all(abs(x - as.numeric(bgcol[i,])) < bg.offset *255, na.rm = T)),3:5] = NA
  }
  imgR <- raster::stack(raster::rasterFromXYZ(imgDF))
  message("Done")
  if(plot){
    raster::plotRGB(imgR)
  }
  return(imgR)
}


