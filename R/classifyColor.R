#' Image color classification
#'
#' @description Image color classification into color categories or classes.
#'
#' @param image RGB raster image
#' @param RGB n rows by 3 columns data.frame with target colors to classify. each column must be R, G and B color values, whereas number of rows will be number of target colors. Default is NULL.
#' @param colOffset  n target colors vector of numeric values between 0 and 1. Color offset threshold to assign the target color. Default = NULL. If NULL, bg.offset will be estimated
#' @param resampleFactor Integer for downsampling used by redRes. Default = NULL
#' @param crop Logical whether to crop image
#' @param cropOffset Desired image extent
#' @param focal Whether to perform Gaussian blurring. Default = FALSE.
#' @param sigma Size of sigma for Gaussian blurring. Default = 3.
#' @param allow.admixture if TRUE allows target color admixture classification. If FALSE, forces their classification into one of the target colors.
#' @param output type of output desired. If 'class', gives a raster image of classified color code. If 'RGB' give a RGB image with classified RGB color codes. If 'both' give a list with 'class' and 'RGB' outputs. Default = 'class'
#' @param removebg Whether remove background based on target color in argument 'bgcol'. Default = FALSE
#' @param bgcol RGB color code to remove from image. if NULL but removebg = TRUE, it will be asked to select on the image.
#' @param bg.offset Numeric value between 0 and 1. If NULL, bg.offset will be estimated. color offset threshold to remove background color. Default NULL
#' @param plot Wheather plot classyfied image or not. Default FALSE
#'
#' @return The output from \code{\link{classifyColor}}
#' @export
#' @importFrom raster plotRGB as.array raster extent stack
#' @importFrom grDevices rgb
#' @examples
#' RGB = data.frame(red = c(255, 255, 0), green = c(255, 165, 0), blue = c(255, 0, 0), row.names = c("white", "orange", "black"))
#' imgClass <- classifyColor(imgTransList[[1]], RGB = RGB, allow.admixture = TRUE, output = "both")
#' plot(imgClass$class)
#' \dontrun{
#' RGB = data.frame(red = c(255, 255, 0), green = c(255, 165, 0), blue = c(255, 0, 0), row.names = c("white", "orange", "black"))
#' imgClass <- classifyColor(imgTransList[[2]], RGB = RGB, allow.admixture = TRUE, output = "both")
#' raster::plotRGB(imgClass$RGB)
#' }
classifyColor <-  function(image, RGB = NULL, colOffset = NULL,  resampleFactor = NULL,
                           crop = F,  cropOffset = c(0, 0, 0, 0),  focal = F, sigma = 3,
                           allow.admixture = F, output = c("class", "RGB", "both"),
                           removebg = F, bgcol = NULL,
                           bg.offset = NULL, plot = F){
  output = output[1]
  out <- list()
  name = names(image)[1]
  name = substr(name, 0, nchar(name)-2)
  if (!class(image) %in% c("RasterStack", "RasterBrick")) {
    stop("object provided is not an image")
  }
  if (removebg == T){
    message("Removing background...")
    image <- removebg(image, bgcol = bgcol, bg.offset = bg.offset, plot = plot)
  }
  if (is.null(RGB)){
    raster::plotRGB(image)
    message("Select the focal colours in image ", name, ", and press [esc] to continue.")
    reference <- as.data.frame(locator(type = "p", col = "blue"))
    kcols <- nrow(reference)
    imgdat <- raster::as.array(image)
    RGB <- try(do.call(rbind, lapply(seq_len(nrow(reference)),
                                     function(x) as.data.frame(t(imgdat[reference$x[x],
                                                                        reference$y[x], 1:3])))), silent = TRUE)
    cols = apply(as.data.frame(RGB), 1, function(x) rgb(x[1],x[2],x[3], maxColorValue = 255))
    plot(image, zlim = c(0,1), breaks = seq(0,1, 1/kcols), col=cols, legend.only = TRUE, legend.width = 5, horizontal = TRUE,
         smallplot = c(0.25, 0.75, 0.5, 0.6),
         axis.args=list(at=seq(0,1, 1/kcols)[-1]-1/kcols*0.5,
                        labels=seq(1,kcols,1),
                        cex.axis=1.2, font.axis = 2),
         legend.args=list(text='kcols', side=2, font=2, line=1, cex=1.5))
  }
  if (is.null(colOffset)){
    message("Computing colors' offsets...")
    colOffset <- apply(RGB, 1, function(x) colOffset(image, x))
    message("Selected offsets: ")
    print(colOffset)
  }
  if (is.data.frame(RGB) || is.matrix(RGB)){
    message("Classifying image...")
    kcols = nrow(RGB)
    imgDF = sapply(1:kcols, function(j) apply(raster::as.array(image), 1:2,
                                              function(x) sum(abs(x - as.numeric(RGB[j,])) < colOffset[j]*255, na.rm = T)))
    vals = rep(NA,nrow(imgDF))
    NAs = which(apply(raster::as.array(image), 1:2,
                      function(x) all(is.na(x))))
    if(length(NAs) > 0){imgDF = imgDF[-NAs,]}
    v = apply(as.data.frame(imgDF), 1, function(i) which(i == max(i)))
    vd <- which(sapply(v, function(x) length(x) > 1))
    if(length(vd) > 0){
      message("Admixture of target colors present...")
      if (allow.admixture){
        vdf = t(sapply(v, function(x) colMeans(RGB[x,])))
      }
      else {
        dist = sapply(1:kcols, function(j) apply(raster::as.array(image), 1:2,
                                                 function(x) mean(abs(x - as.numeric(RGB[j,])))))
        if(length(NAs) > 0){ dist <- dist[-NAs,]}
        v[vd] <- sapply(vd, function(x) v[[x]][which.min(dist[x,v[[x]]])])
        vdf = t(sapply(v, function(x) as.numeric(RGB[x,])))
      }
    }
    RGBr = rbind(as.matrix(RGB), unique(vdf))
    RGBr = RGBr[!duplicated(RGBr),]
    kr = nrow(RGBr)
    rownames(RGBr) = 1:kr
    if (kcols != kr){
      message("There are ", kr-kcols, " colors not fitting into the selected color categories...")
      print(RGBr)
      out$codes <- RGBr
    }
    if (output != "class"){
      imdf = matrix(NA, nrow = length(vals), ncol = 3)
      imdf[-NAs,] = vdf
      imgR = raster::stack(apply(imdf, 2, function(x) raster::raster(matrix(x, ncol =  raster::ncol(image), nrow = raster::nrow(image)))))
      raster::extent(imgR) = raster::extent(image)
      if (plot) {
        raster::plotRGB(imgR)
      }
      out$RGB = imgR
    }
    if (output != "RGB"){
      vals[-NAs] = apply(vdf, 1,
                         function(x) which(sapply(1:kr,
                                                  function(j) all(abs(x - as.numeric(RGBr[j,])) == 0))))
      imgC = matrix(vals, ncol =  raster::ncol(image), nrow = raster::nrow(image))
      imgC = raster(imgC)
      #imgR[NAs] = NA
      raster::extent(imgC) = raster::extent(image)
      if (plot) {
        cols = apply(raster::as.data.frame(RGBr), 1, function(x) grDevices::rgb(x[1],x[2],x[3], maxColorValue = 255))
        plot(imgC, col = cols, legend = F)
        plot(imgC, zlim = c(0,1), breaks = seq(0,1, 1/kr), col=cols, legend.only = TRUE,
             axis.args=list(at=seq(0,1, 1/kr)[-1]-1/kr*0.5,
                            labels=seq(1,kr,1),
                            cex.axis=1, font.axis = 1),
             legend.args=list(text='color codes', side=4, font=2, line=2, cex=1))
      }
      imgC[is.na(imgC)] = 0
      out$class = imgC
    }
    message("Done")
  }
  return(out)
}
