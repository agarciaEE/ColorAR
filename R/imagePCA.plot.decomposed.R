#' Plot decomposed imagePCA
#'
#' @description plot imagePCA object with decomposed PCA axis
#'
#' @param imagePCA imagePCA object
#' @param tree Optional, if class 'phylo' tree is to be integrated in the output.
#' @param cols vector of colors used for visualize color changes. Default = NULL.
#' @param tree.cols vector of k colors with length of n tips of the given tree. If NULL, coltree will match colPCA. Default is NULL.
#' @param images vector of images to be displayed. Defaul it NULL
#' @param ras ras
#' @param PCA.legend Logical whether to plot PCA legend. Default TRUE.
#' @param tree.legend Logical whether to plot tree legend. Default TRUE.
#'
#' @return The output from \code{\link{print}}
#' @export
#' @import graphics raster stats
#' @examples
#' tree <- ape::rtree(26, tip.label = letters[1:26])
#' X <- data.frame(trait1 = runif(26, -10, 10), trait2 = runif(26, -25, 25))
#' plotPhylomorphospace(tree, X)
#' \dontrun{
#' plotPhylomorphospace(tree, X, palette = rainbow(6), col.branches = T)
#' }
imagePCA.plot.decomposed <- function(imagePCA, tree, cols = NULL, tree.cols = NULL,
                                     images = NULL, ras = NULL,
                                     PCA.legend = T, tree.legend = T){

  mat <- matrix(c(7, 7, 5, 6,
                  1, 2, 3, 4), 2, 4,
                byrow = TRUE)
  layout(mat, widths = c(2,0.25,1,1), heights = c(0.5,5))

  df <-  imagePCA$df
  PCx <- colnames(df)[1]; PCy <- colnames(df)[2]
  ev <- imagePCA$pca$sdev^2
  ev <- ev/sum(ev)*100
  ### plot tree left side
  par(mar = c(4,1,0,0))
  opar = par()
  par(fg="transparent")
  plot(tree)
  pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
  par(fg="black")
  ### Add color boxes on tree tips
  for(i in 1:Ntip(tree)) boxlabel(pp$xx[i],pp$yy[i],length = 4, tree$tip.label[i],bg= tree.cols[tree$tip.label[i]], cex = 1.5, alpha = 0.8)
  par(mar = c(4,0,0,0))
  ### plot species images
  plot(NULL, xlim=c(-1,1), ylim=c(1,nrow(df)), type="n", axes = FALSE, xlab = '', ylab='')
  if(!is.null(images)){
    plotImages(rep(0,nrow(df)), df[,5], images[rownames(df)], interpolate = T, width = 1.5, height = 0.05)
  }
  ### plot PCx
  par(mar = c(4,1,0,1))
  plot(df[,c(1,5)], col = cols[rownames(df)],
       pch = 19, yaxt = 'n',
       cex = 2, xlim = range(df[,PCx]), ylim = range(df[,PCy]), ylab = "",
       xlab = paste(PCx, " (", round(ev[PCx], 1), " %)"))
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "grey90")
  abline(v = 0, lty = 2, lwd = 2, col = "white")
  points(df[,1], df[,5], col = cols[rownames(df)], pch = 19, cex = 2)
  sapply(1:nrow(df), function(i) graphics::segments(df[i,1]-df[i,3], df[i,5], df[i,1]+df[i,3], df[i,5], col = alpha(cols[rownames(df)[i]], 0.8) , lwd = 4))
  if(!is.null(df[,6])){
    sapply(unique(df[,6]), function(i) abline(h = max(df[df[,6] == i,"order"])+0.5, lty = 2, col = "grey50"))
    sapply(unique(df[,6]), function(i) abline(h = min(df[df[,6] == i,"order"])-0.5, lty = 2, col = "grey50"))
  }
  ### plot PCy
  par(mar = c(4,1,0,1))
  plot(df[,c(2,5)], col = cols[rownames(df)],
       pch = 19, yaxt = 'n',
       cex = 2, xlim = range(df[,PCx]), ylim = range(df[,PCy]), ylab = "",
       xlab = paste(PCy, " (", round(ev[PCy], 1), " %)"))
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "grey90")
  abline(v = 0, lty = 2, lwd = 2, col = "white")
  points(df[,2], df[,5], col = cols[rownames(df)], pch = 19, cex = 2)
  sapply(1:nrow(df), function(i) graphics::segments(df[i,2]-df[i,4], df[i,5], df[i,2]+df[i,4], df[i,5], col = alpha(cols[rownames(df)[i]], 0.8) , lwd = 4))
  if(!is.null(df[,6])){
    sapply(unique(df[,6]), function(i) abline(h = max(df[df[,6] == i,"order"])+0.5, lty = 2, col = "grey50"))
    sapply(unique(df[,6]), function(i) abline(h = min(df[df[,6] == i,"order"])-0.5, lty = 2, col = "grey50"))
  }

  ### plot PC1 changes
  par(mar = c(0,2,1,2))
  plot(NULL, xlim=c(-2,2), ylim=c(-2,2), type="n", axes = FALSE, xlab = '', ylab='')
  shape::Arrows(-1, 0, 1, 0, code = 3, lwd =2, arr.type = "triangle")
  plotImages(c(-1.5, 1.5), c(0, 0), ras[2:3], cols = c("red", "white", "blue"),
              width = rep(0.1*2,2), height = rep(2,2),  interpolate = T)
  ### plot PC2 changes
  par(mar = c(0,2,1,2))
  plot(NULL, xlim=c(-2,2), ylim=c(-2,2), type="n", axes = FALSE, xlab = '', ylab='')
  shape::Arrows(-1, 0, 1, 0, code = 3, lwd =2, arr.type = "triangle")
  plotImages(c(-1.5, 1.5), c(0, 0), ras[4:5], cols = c("red", "white", "blue"),
              width = rep(0.1*2,2), height = rep(2,2),  interpolate = T)
  ###plot legends
  plot(NULL, xlim=c(-1,1), ylim=c(1,28), type="n", axes = FALSE, xlab = '', ylab='')
  if(PCA.legend){
    legend("left", legend = names(legend), bg = "transparent",  cex=3, box.lty = 0, bty = "n", horiz = T)
  }
  if (tree.legend){
    legend("right", legend = names(tree.legend), bg = "transparent",
           fill= tree.legend,  cex=1.5, box.lty = 0, horiz = T)
  }
  par(mfrow = c(1,1))
}
