#' Plot imagePCA
#'
#' @description plot imagePCA object with standard 2-D PCA axis
#'
#' @param imgPCA imagePCA object
#' @param tree Optional, if class 'phylo' tree is to be integrated in the output.
#' @param plot.tree if 'side', tree will be shown next to the PCA analyses. If 'integrated', Phytool::fastAnc will be computed and tree will be integrated into PCA plot. Default 'integated'.
#' @param onTop Logical whether to plot data points on top of images.
#' @param SD Logical wheter to plot standard deviation. SD must be given by imagePCA object df slot as 3rd and 4th columns
#' @param col.branches Logical whether to color tree branches.
#' @param node.width size of the tree nodes.
#' @param node.pch type of symbol to display at nodes of the tree if plot.tree is "integrated". Default is 18.
#' @param tree.cex tree tips text size. Default 1
#' @param names.cex image names text size. Default 1
#' @param legend Logical whether to plot a legend on the main PCA plot
#' @param pos Position of the legend. Default is 'bottomleft'.
#' @param xlim PC x axis range. Default is NULL.
#' @param ylim PC y axis range. Default is NULL.
#' @param colPCA vector of k colors with lenght of n images. names of each color must match the name of image to represent. If NULL it will be computed based on disntances of PCA results.
#' @param coltree vector of k colors with lenght of n tips of the given tree. If NULL, coltree will match colPCA. Default is NULL.
#' @param palette palette of brewer.pal colors to use. If NULL and colPCA = NULL, viridis palette will be used. Default NULL.
#' @param plot.names Whether plot image names on PCA plot, Default TRUE
#' @param plot.images Whether plot images on PCA plot, Default TRUE
#' @param cex text size. Default 1
#' @param pch type of symbol to display on PCA data points. Default is 19.
#' @param size size of the images to display on the PCA plot
#' @param cols vector of colors used for visualize color changes. Default = c("red", "white", "blue")
#'
#' @return The output from \code{\link{imagePCA.plot}}
#' @export
#' @importFrom raster nlayers plot
#' @importFrom viridis viridis
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics legend text
#' @importFrom scales alpha
#' @importFrom shape Arrows
#'
#' @examples
#' color.list <- viridis::viridis(5)
#' col_region <- setNames(rep(color.list)[as.factor(dataset$region)], dataset$sample)
#' col_region_legend <- setNames(rep(color.list)[as.factor(levels(as.factor(dataset$region)))], levels(as.factor(dataset$region)))
#' imagePCA.plot(imgPCA12,  tree = tree, plot.tree = "side",
#'               plot.images = F, cex = 1, plot.names = T, colPCA = col_region, legend = col_region_legend)
#' \dontrun{
#' imagePCA.plot(imgPCA12, tree = tree, plot.tree = "integrated", plot.images = F)
#' }
imagePCA.plot <-function(imgPCA, tree = NULL, plot.tree = c("side", "integrated"), onTop = F, SD = F,
                         col.branches  = F, node.width = 0.5, node.pch = 18, tree.cex = 1, names.cex = 1,
                         legend = NULL, pos = "bottomleft", xlim = NULL, ylim =  NULL,
                         colPCA = NULL, coltree = colPCA, palette = NULL, plot.names = TRUE, plot.images = TRUE,
                         cex = 1, pch = 19, size = 0.1, cols = c("red", "white", "blue")){

  if(!is.null(plot.tree)){
    if(is.null(tree) & !is.null(imgPCA$tree)) {
      tree = imgPCA$tree
      plot.tree = plot.tree[1]
    }
    if(is.null(tree) & is.null(imgPCA$tree)) {
      warning("tree not provided.")
      plot.tree = NULL
    }
  }
  images = imgPCA$images
  type = imgPCA$type
  PCx = imgPCA$components[1]
  PCy = imgPCA$components[2]
  mapList = imgPCA$ras
  as.RGB = F
  if(type == "RGB" & raster::nlayers(mapList[[2]]) == 3){ as.RGB = T }
  pcdata = imgPCA$df
  comp <-  imgPCA$pca
  if (is.null(xlim)){
    xlim <- range(pcdata[, 1])
  }
  if (is.null(ylim)){
    ylim <- range(pcdata[, 2])
  }
  xmin <- xlim[1]
  xmax <- xlim[2]
  ymin <- ylim[1]
  ymax <- ylim[2]
  rotation <- comp$rotation
  summ <- summary(comp)
  if(is.null(palette)){
    colRamp <- grDevices::colorRampPalette(viridis::viridis(9))(n=100)
  }
  else {
    colRamp <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = palette))(n=100)
  }
  if(!is.null(tree) & is.null(coltree)){
    if (all(tree$tip.label %in% rownames(pcdata))){
      nodes <- unique(tree$edge[tree$edge > tree$Nnode +1])
      nodeList <- sapply(as.character(tree$tip.label), function(i) sort(getAncestors(tree, i), decreasing = F))
      coltree <- sapply(nodeList, function(i) colRamp[ceiling((i - min(nodes) + 1)/(diff(range(nodes)) + 1) * 100)])
      coltree <- sapply(coltree, function(i) mix.colors(unlist(i)))
      colPCA = coltree
    }
    else{
      coltree <- rep("white", length(tree$tip.label))
      names(coltree) <- tree$tip.label
    }
  }
  if (is.null(colPCA)){
    colPCA <- lapply(1:2, function(j) sapply(rownames(pcdata),
                                             function(i) colRamp[ceiling((pcdata[i,j] +
                                                                            abs(min(pcdata[,j]))+1) * 100 / (diff(range(pcdata[,j]))+1))]))
    colPCA <- reverse.list(colPCA)
    colPCA <- sapply(colPCA, function(i) mix.colors(unlist(i)))
  }
  mat <- matrix(c(1, 1, 1, 2, 2,
                  1, 1, 1, 2, 2,
                  1, 1, 1, 3, 3), 3, 5,
                byrow = TRUE)
  if (as.RGB){
    mat <- matrix(c(1, 1, 1, 1, 3, 3,
                    1, 1, 1, 1, 2, 2,
                    1, 1, 1, 1, 2, 2,
                    1, 1, 1, 1, 3, 3), 4, 6,
                  byrow = TRUE)
  }
  if (!is.null(tree)){
    if(plot.tree == "integrated"){
      if (!all(tree$tip.label %in% rownames(pcdata))){
        warning("tree provided and PCA data don't macth. tree cannot be integrated, siwtching to 'side' type... ", immediate. = T)
        plot.tree = "side"
      }
      else{
        layout(mat)
        par(mar = c(5, 5, 3, 4))
        plotPhylomorphospace(tree, pcdata[,1:2], cols = colPCA, cex = cex, node.width = node.width,
                         col.branches = col.branches, node.pch = node.pch, xlim = xlim, ylim = ylim,
                         xlab = paste(PCx, " (", round(summ$importance[2,PCx] * 100, 1), " %)"),
                         ylab = paste(PCy, " (", round(summ$importance[2,PCy] * 100, 1), " %)"))
        if(plot.images) {
          plotImages(pcdata[,1], pcdata[,2], images, interpolate = T, width = size)
        }
        if (plot.names){
          text(pcdata[, 1], pcdata[, 2], cex = cex, pos = 1 , offset = 1,
               as.character(rownames(pcdata)))
        }
        if(!is.null(legend)){
          legend(pos, legend=names(legend), bg = "transparent",
                 fill=legend,  cex=cex*0.8, box.lty = 0)
        }
        par(mar = c(4, 4, 5, 4))
        plot(NULL, xlim=c(-2,2), ylim=c(-2,2), type="n", axes = FALSE, xlab = '', ylab='')
        shape::Arrows(0,-1, 0, 1,  code = 3, lwd =2, arr.type = "triangle")
        shape::Arrows(-1, 0, 1, 0, code = 3, lwd =2, arr.type = "triangle")
        plotImages(c(0,-1.5, 1.5, 0, 0), c(0,0, 0, -1.5, 1.5), mapList, cols = cols,
                    width = c(0.4, rep(0.2,4)),  interpolate = T, names = c("", paste0("min", PCx), paste0("max", PCx)
                                                                            , paste0("min", PCy), paste0("max", PCy)),
                    pos = c(1, 2,  4,  1,  3))
        if (!as.RGB){
          par(mar = c(5, 5, 5, 5))
          plot(NULL, xlim=c(0,1), ylim=c(0,1), type="n", axes = FALSE, xlab = '', ylab='')
          raster::plot(mapList[[1]], zlim = c(-1,1), col= grDevices::colorRampPalette(cols)(n=100),  legend.only = TRUE, legend.width = 2, horizontal = TRUE,
               smallplot = c(0.3, 0.9, 0.3, 0.5), legend.args = list(text="Standardized\ndifferences", side = 3, font = 2, line = 1, cex = 1))
        }
      }
    }
    if(plot.tree == "side"){
      mat <- matrix(c(1, 1, 2, 2, 2, 3, 3,
                      1, 1, 2, 2, 2, 3, 3,
                      1, 1, 2, 2, 2, 4, 4), 3, 7,
                    byrow = TRUE)
      if (as.RGB){
        mat <- matrix(c(1, 1, 2, 2, 2, 4, 4,
                        1, 1, 2, 2, 2, 3, 3,
                        1, 1, 2, 2, 2, 3, 3,
                        1, 1, 2, 2, 2, 4, 4), 4, 7,
                      byrow = TRUE)
      }
      layout(mat)
      par(mar = c(3,3,3,3), oma = c(0,0,0,0))
      opar = par()
      plot(tree)
      pp<-get("last_plot.phylo",envir=.PlotPhyloEnv)
      par(fg="black")
      for(i in 1:Ntip(tree)) boxlabel(pp$xx[i], pp$yy[i], tree$tip.label[i], bg=coltree[tree$tip.label[i]], cex = pp$cex, alpha = 0.8)
      par(mar = c(5, 4, 3, 4))
      plot(pcdata[,1:2], col = colPCA[rownames(pcdata)],
           pch = 1,
           cex = 0, xlim = c(xmin, xmax), ylim = c(ymin,ymax),
           xlab = paste(PCx, " (", round(summ$importance[2,PCx] * 100, 1), " %)"),
           ylab = paste(PCy, " (", round(summ$importance[2,PCy] * 100, 1), " %)"))
      if (!onTop){
        if (SD){
          sapply(1:nrow(pcdata), function(i) segments(pcdata[i,1]-pcdata[i,3], pcdata[i,2], pcdata[i,1]+pcdata[i,3], pcdata[i,2], col = scales::alpha(colPCA[rownames(pcdata)[i]], 0.8) , lwd = 4))
          sapply(1:nrow(pcdata), function(i) segments(pcdata[i,1], pcdata[i,2]-pcdata[i,4], pcdata[i,1], pcdata[i,2]+ pcdata[i,4], col = scales::alpha(colPCA[rownames(pcdata)[i]], 0.8) , lwd = 4))
        } else{
          points(pcdata[,1], pcdata[,2], col = colPCA[rownames(pcdata)], pch = pch, cex = cex)
        }
      }
      if(plot.images) {
        plotImages(pcdata[,1], pcdata[,2], images, interpolate = T, width = size)
      }
      if (plot.names){
        text(pcdata[, 1], pcdata[, 2], cex = names.cex, pos = 1 , offset = 1,
             as.character(rownames(pcdata)))
      }
      if (onTop){
        if (SD){
          sapply(1:nrow(pcdata), function(i) segments(pcdata[i,1]-pcdata[i,3], pcdata[i,2], pcdata[i,1]+pcdata[i,3], pcdata[i,2], col = scales::alpha(colPCA[rownames(pcdata)[i]], 0.8) , lwd = 4))
          sapply(1:nrow(pcdata), function(i) segments(pcdata[i,1], pcdata[i,2]-pcdata[i,4], pcdata[i,1], pcdata[i,2]+ pcdata[i,4], col = scales::alpha(colPCA[rownames(pcdata)[i]], 0.8) , lwd = 4))
        } else{
          points(pcdata[,1], pcdata[,2], col = colPCA[rownames(pcdata)], pch = pch, cex = cex)
        }
      }
      if(!is.null(legend)){
        legend(pos, legend=names(legend), bg = "transparent",
               fill=legend,  cex=cex*0.8, box.lty = 0)
      }
      plot(NULL, xlim=c(-2,2), ylim=c(-2,2), type="n", axes = FALSE, xlab = '', ylab='')
      shape::Arrows(0,-1, 0, 1,  code = 3, lwd =2, arr.type = "triangle")
      shape::Arrows(-1, 0, 1, 0, code = 3, lwd =2, arr.type = "triangle")
      plotImages(c(0,-1.5, 1.5, 0, 0), c(0,0, 0, -1.5, 1.5), mapList, cols = cols,
                  width = c(0.4, rep(0.2,4)),  interpolate = T, names = c("", paste0("min", PCx), paste0("max", PCx)
                                                                          , paste0("min", PCy), paste0("max", PCy)),
                  pos = c(1, 2,  4,  1,  3))
      if (!as.RGB){
        par(mar = c(5, 5, 5, 5))
        plot(NULL, xlim=c(0,1), ylim=c(0,1), type="n", axes = FALSE, xlab = '', ylab='')
        raster::plot(mapList[[1]], zlim = c(-1,1), col= grDevices::colorRampPalette(cols)(n=100),  legend.only = TRUE, legend.width = 2, horizontal = TRUE,
             smallplot = c(0.4, 0.9, 0.3, 0.5), legend.args = list(text="Standardized\ndifferences", side = 3, font = 2, line = 1, cex = 1))
      }
    }
  }
  else{
    layout(mat)
    par(mar = c(5, 5, 3, 4))
    plot(pcdata[,1:2], col = colPCA[rownames(pcdata)],
         pch = pch,
         cex = cex, xlim = c(xmin, xmax), ylim = c(ymin,ymax),
         xlab = paste(PCx, " (", round(summ$importance[2,PCx] * 100, 1), " %)"),
         ylab = paste(PCy, " (", round(summ$importance[2,PCy] * 100, 1), " %)"))
    if(plot.images) {
      plotImages(pcdata[,1], pcdata[,2], images, interpolate = T, width = size)
    }
    if(plot.names){
      graphics::text(pcdata[, 1], pcdata[, 2], cex = names.cex, pos = 1 , offset = 1,
           as.character(rownames(pcdata)))
    }
    if(!is.null(legend)){
      graphics::legend(pos, legend=names(legend), bg = "transparent",
             fill=legend,  cex=cex*0.8, box.lty = 0)
    }
    plot(NULL, xlim=c(-2,2), ylim=c(-2,2), type="n", axes = FALSE, xlab = '', ylab='')
    shape::Arrows(0,-1, 0, 1,  code = 3, lwd =2, arr.type = "triangle")
    shape::Arrows(-1, 0, 1, 0, code = 3, lwd =2, arr.type = "triangle")
    plotImages(c(0,-1.5, 1.5, 0, 0), c(0,0, 0, -1.5, 1.5), mapList, cols = cols,
                width = c(0.4, rep(0.2,4)),  interpolate = T, names = c("", paste0("min", PCx), paste0("max", PCx)
                                                                        , paste0("min", PCy), paste0("max", PCy)),
                pos = c(1, 2,  4,  1,  3))
    if (!as.RGB){
      par(mar = c(5, 5, 5, 5))
      plot(NULL, xlim=c(0,1), ylim=c(0,1), type="n", axes = FALSE, xlab = '', ylab='')
      raster::plot(mapList[[1]], zlim = c(-1,1), col= colorRampPalette(cols)(n=100),  legend.only = TRUE, legend.width = 2, horizontal = TRUE,
           smallplot = c(0.3, 0.9, 0.3, 0.5), legend.args = list(text="Standardized\ndifferences", side = 3, font = 2, line = 1, cex = 1))
    }
  }
  par(mfrow = c(1,1))
}
