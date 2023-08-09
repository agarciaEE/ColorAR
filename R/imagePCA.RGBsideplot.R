#' PCA side plot
#'
#' @description makes the RGB reconstruction side plot of imagePCA.plot function
#' @param x imagePCA object
#' @param q quantile value to subset images based on their PC distribution
#' @param plot Logical whether to plot the side plot.
#'
#' @return The output from \code{\link{print}}
#' @export
#' @importFrom stats quantile dnorm
#' @importFrom raster mean stack
#' @importFrom shape Arrows
#' @importFrom graphics text
#'
#' @examples
#' library(ColorAR)
#' data(imgPCA12)
#' sideplot <- imagePCA.RGBsideplot(imgPCA12)
#'
imagePCA.RGBsideplot <- function(x, q = 0.75, plot = T){

  PCx <- x$components[1]
  PCy <- x$components[2]

  # get mean image (center)
  mean.image <- raster::stack(raster::mean(raster::stack(lapply(x$images, function(i) i[[1]])), na.rm = T),
                              raster::mean(raster::stack(lapply(x$images, function(i) i[[2]])), na.rm = T),
                              raster::mean(raster::stack(lapply(x$images, function(i) i[[3]])), na.rm = T))

  # get PC threshold to find representatives
  th.minPCx <- stats::quantile(x$df[x$df[,PCx] < 0,1], q)
  th.maxPCx <- stats::quantile(x$df[x$df[,PCx] > 0,1], q)
  th.minPCy <- stats::quantile(x$df[x$df[,PCy] < 0,1], q)
  th.maxPCy <- stats::quantile(x$df[x$df[,PCy] > 0,1], q)
  # get representatives names
  minPCx.names <- rownames(x$df[x$df[,PCx] < th.minPCx,])
  maxPCx.names <- rownames(x$df[x$df[,PCx] > th.maxPCx,])
  minPCy.names <- rownames(x$df[x$df[,PCy] < th.minPCy,])
  maxPCy.names <- rownames(x$df[x$df[,PCy] > th.maxPCy,])

  # replicate representatives in base to their proximity to the center of coordinates of the other axis
  PCx_range <- range(x$df[,PCx])
  PCy_range <- range(x$df[,PCy])

  PCx_cutoff <- seq(PCx_range[1]*1.01, PCx_range[2]*1.01, length.out = 10)
  PCy_cutoff <- seq(PCy_range[1]*1.01, PCy_range[2]*1.01, length.out = 10)

  PCx_norm <-  stats::dnorm(PCx_cutoff, 0, stats::sd(x$df[,PCx]))
  PCx_cutoff <- list(cutoff = PCx_cutoff, reps = round(PCx_norm/max(PCx_norm) * 10))

  PCy_norm <-  stats::dnorm(PCy_cutoff, 0, sd(x$df[,PCy]))
  PCy_cutoff <- list(cutoff = PCy_cutoff, reps = round(PCy_norm/max(PCy_norm) * 10))

  ####  ####  ####  ####  ####
  minPCx_reps <- rep(PCy_cutoff[[2]])[cut(x$df[minPCx.names,PCy], PCy_cutoff[[1]])]
  minPCx.names <- rep(minPCx.names, times = minPCx_reps)

  maxPCx_reps <- rep(PCy_cutoff[[2]])[cut(x$df[maxPCx.names,PCy], PCy_cutoff[[1]])]
  maxPCx.names <- rep(maxPCx.names, times = maxPCx_reps)

  minPCy_reps <- rep(PCx_cutoff[[2]])[cut(x$df[minPCy.names,PCx], PCx_cutoff[[1]])]
  minPCy.names <- rep(minPCy.names, times = minPCy_reps)

  maxPCy_reps <- rep(PCx_cutoff[[2]])[cut(x$df[maxPCy.names,PCx], PCx_cutoff[[1]])]
  maxPCy.names <- rep(maxPCy.names, times = maxPCy_reps)

  # get mean image of representatives
  mean.minPCx <- raster::stack(raster::mean(raster::stack(lapply(x$images[minPCx.names], function(i) i[[1]])), na.rm = T),
                               raster::mean(raster::stack(lapply(x$images[minPCx.names], function(i) i[[2]])), na.rm = T),
                               raster::mean(raster::stack(lapply(x$images[minPCx.names], function(i) i[[3]])), na.rm = T))

  mean.maxPCx <- raster::stack(raster::mean(raster::stack(lapply(x$images[maxPCx.names], function(i) i[[1]])), na.rm = T),
                               raster::mean(raster::stack(lapply(x$images[maxPCx.names], function(i) i[[2]])), na.rm = T),
                               raster::mean(raster::stack(lapply(x$images[maxPCx.names], function(i) i[[3]])), na.rm = T))

  mean.minPCy <- raster::stack(raster::mean(raster::stack(lapply(x$images[minPCy.names], function(i) i[[1]])), na.rm = T),
                               raster::mean(raster::stack(lapply(x$images[minPCy.names], function(i) i[[2]])), na.rm = T),
                               raster::mean(raster::stack(lapply(x$images[minPCy.names], function(i) i[[3]])), na.rm = T))

  mean.maxPCy <- raster::stack(raster::mean(raster::stack(lapply(x$images[maxPCy.names], function(i) i[[1]])), na.rm = T),
                               raster::mean(raster::stack(lapply(x$images[maxPCy.names], function(i) i[[2]])), na.rm = T),
                               raster::mean(raster::stack(lapply(x$images[maxPCy.names], function(i) i[[3]])), na.rm = T))

  # make mapList object
  mapList <- c(mean.image, mean.minPCx, mean.maxPCx, mean.minPCy, mean.maxPCy)
  names(mapList) <- c("center", paste0("min", PCx), paste0("max", PCx), paste0("min", PCy), paste0("max", PCy))

  #plot
  if (plot){
    plot(NULL, xlim=c(-2,2), ylim=c(-2,2), type="n", axes = FALSE, xlab = '', ylab='')
    shape::Arrows(0,-1, 0, 1,  code = 3, lwd =2, arr.type = "triangle")
    shape::Arrows(-1, 0, 1, 0, code = 3, lwd =2, arr.type = "triangle")
    plotImages(c(0,-1.5, 1.5, 0, 0), c(0,0, 0, -1.5, 1.5), mapList,
                width = c(0.1*4, rep(0.1*2,4)),  interpolate = T,
                names = NULL,
                pos = c(1, 2,  4,  1,  3))
    graphics::text(c(0,-1.3, 1.3, 0, 0), c(0, -0.5, -0.5, -1.9, 1.9),
                   c("", paste0("min", PCx), paste0("max", PCx), paste0("min", PCy), paste0("max", PCy)),
                   pos = c(1, 2,  4,  1,  3))
  }
  return(mapList)
}
