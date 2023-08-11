#' Plot decomposed imagePCA
#'
#' @description plot imagePCA object with decomposed PCA axis
#'
#' @param imagePCA imagePCA object
#' @param tree Optional, if class 'phylo' tree is to be integrated in the output.
#' @param groups vector of length equal to the number of images indicating the grouping.
#' @param scale logical whether to scale the PCA scores. Default is FALSE.
#' @param cols vector of colors used for visualize color changes. Default = NULL.
#' @param PCA.cols vector of colors to plot pca points with length of number images given to the imagePCA. If NULL, it will be set to black.
#' @param tree.cols vector of colors with length of n tips of the given tree. If NULL, coltree will match colPCA. Default is NULL.
#' @param plot.images logical whether to plot images. Default is FALSE.
#' @param PCA.legend Logical whether to plot PCA legend. Default TRUE.
#' @param tree.legend Logical whether to plot tree legend. Default TRUE.
#' @param pch symbol type. Default is 19
#' @param ... extra arguments passed to graphical functions (e.g. cex = 1)
#'
#' @return The output from \code{\link{imagePCA.plot.decomposed}}
#' @export
#' @importFrom graphics segments layout points abline legend
#' @importFrom raster mean stack
#' @importFrom shape Arrows
#' @importFrom stats setNames
#' @importFrom ape .PlotPhyloEnv
#' @examples
#' library(ColorAR)
#' data(tree)
#' data(imgPCA12)
#' imagePCA.plot.decomposed(imgPCA12, tree)
#' \dontrun{
#' library(ColorAR)
#' data(imgPCA12)
#' gtree <- ape::rtree(3, tip.label = letters[1:3])
#' groups <- sample(letters[1:3], length(imgPCA12$images), replace = TRUE)
#' imagePCA.plot.decomposed(imgPCA12, gtree, groups = groups)
#' }
imagePCA.plot.decomposed <- function(imagePCA, tree, groups = NULL, scale = F, PCA.cols = NULL, tree.cols = NULL,
                                     plot.images = F, PCA.legend = NULL, tree.legend = NULL, pch = 19, ...){

  df <-  imagePCA$df
  ras <- imagePCA$ras
  imgList <- imagePCA$images
  PCx <- colnames(df)[1]; PCy <- colnames(df)[2]
  if (ncol(df) == 2) {
    if (!is.null(groups)){
      if (length(unique(groups)) == length(tree$tip.label)){
        df <- aggregateGroups(imagePCA$pca, groups, PCx = PCx, PCy = PCy, scale = scale)
        imgList <-  sapply(unique(groups), function(g) raster::stack(raster::mean(raster::stack(lapply(imgList[which(groups == g)], function(i) i[[1]])), na.rm = T),
                                                                     raster::mean(raster::stack(lapply(imgList[which(groups == g)], function(i) i[[2]])), na.rm = T),
                                                                     raster::mean(raster::stack(lapply(imgList[which(groups == g)], function(i) i[[3]])), na.rm = T)))
      }
    } else {
      df <- cbind(df, 0, 0)
      colnames(df)[3:4] <- paste0(c(PCx, PCy), ".sd")
      imgList <- imagePCA$images
    }
  }
  mat <- matrix(c(7, 7, 5, 6,
                  1, 2, 3, 4), 2, 4,
                byrow = TRUE)
  graphics::layout(mat, widths = c(2,0.25,1,1), heights = c(0.5,5))

  ev <- imagePCA$pca$sdev^2
  ev <- ev/sum(ev)*100
  ### plot tree left side
  graphics::par(mar = c(4,1,0,0))
  plot(tree)
  pp<-get("last_plot.phylo",envir=ape::.PlotPhyloEnv)
  ### Add color boxes on tree tips
  ntips <- length(tree$tip.label)
  df$vpos <- sapply(rownames(df), function(i) which(tree$tip.label %in% i))
  if (is.null(tree.cols)) { tree.cols <- stats::setNames(rep("transparent", ntips), tree$tip.label) }
  if (is.null(PCA.cols)) { PCA.cols <- stats::setNames(rep("black", nrow(df)), rownames(df)) }
  for(i in 1:ntips) boxlabel(pp$xx[i],pp$yy[i], tree$tip.label[i], bg = tree.cols[tree$tip.label[i]])
  par(mar = c(4,0,0,0))
  ### plot species images
  plot(NULL, xlim=c(-1,1), ylim=c(1,nrow(df)), type="n", axes = FALSE, xlab = '', ylab='')
  if(plot.images){
    plotImages(rep(0,nrow(df)), df$vpos, imgList[rownames(df)], interpolate = T, width = 1.5, height = 0.05)
  }
  ### plot PCx
  par(mar = c(4,1,0,1))
  plot(NULL, xlim = range(c(df[,PCx]-df[,3], df[,PCx]+df[,3]), na.rm = TRUE),
       ylim = range(df$vpos, na.rm = TRUE), ylab = "", yaxt = "n",
       xlab = paste(PCx, " (", round(ev[as.numeric(sub("PC", "", PCx))], 1), " %)"))
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col = "grey90")
  graphics::abline(v = 0, lty = 2, lwd = 2, col = "white")
  graphics::points(df[,PCx], df$vpos, col = PCA.cols[rownames(df)], ...)
  sapply(1:nrow(df), function(i) graphics::segments(df[i,PCx]-df[i,3], df[i,"vpos"],
                                                    df[i,PCx]+df[i,3], df[i,"vpos"],
                                                    col = scales::alpha(PCA.cols[rownames(df)[i]], 0.8) ,
                                                    lwd = 4))

  ### plot PCy
  par(mar = c(4,1,0,1))
  plot(NULL, xlim = range(c(df[,PCy]-df[,4], df[,PCy]+df[,4]), na.rm = TRUE),
       ylim = range(df$vpos, na.rm = TRUE), ylab = "", yaxt = "n",
       xlab = paste(PCy, " (", round(ev[as.numeric(sub("PC", "", PCy))], 1), " %)"))
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "grey90")
  graphics::abline(v = 0, lty = 2, lwd = 2, col = "white")
  graphics::points(df[,PCy], df$vpos, col = PCA.cols[rownames(df)], ...)
  sapply(1:nrow(df), function(i) graphics::segments(df[i,PCy]-df[i,4], df[i,"vpos"],
                                                    df[i,PCy]+df[i,4], df[i,"vpos"],
                                                    col = scales::alpha(PCA.cols[rownames(df)[i]], 0.8) ,
                                                    lwd = 4))

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
  if(!is.null(PCA.legend)) graphics::legend("left", legend = names(PCA.legend), fill= PCA.legend, bg = "transparent",  cex=3, box.lty = 0, bty = "n", horiz = T)
  if(!is.null(tree.legend)) graphics::legend("right", legend = names(tree.legend), fill= tree.legend, bg = "transparent",
             cex=1.5, box.lty = 0, horiz = T)
  par(mfrow = c(1,1))
}
