#' Plot imageUMAP
#'
#' @description plot imageUMAP object with standard 2-D UMAP axis
#'
#' @param imgUMAP imageUMAP object
#' @param tree Optional, if class 'phylo' tree is to be integrated in the output.
#' @param plot.tree if 'side', tree will be shown next to the UMAP analyses. If 'integrated', Phytool::fastAnc will be computed and tree will be integrated into UMAP plot. Default 'integated'.
#' @param onTop Logical whether to plot data points on top of images.
#' @param SD Logical wheter to plot standard deviation. SD must be given by imageUMAP object df slot as 3rd and 4th columns
#' @param col.branches Logical whether to color tree branches.
#' @param node.width size of the tree nodes.
#' @param node.pch type of symbol to display at nodes of the tree if plot.tree is "integrated". Default is 18.
#' @param tree.cex tree tips text size. Default 1
#' @param names.cex image names text size. Default 1
#' @param legend Logical whether to plot a legend on the main UMAP plot
#' @param pos Position of the legend. Default is 'bottomleft'.
#' @param xlim PC x axis range. Default is NULL.
#' @param ylim PC y axis range. Default is NULL.
#' @param colUMAP vector of k colors with lenght of n images. names of each color must match the name of image to represent. If NULL it will be computed based on disntances of UMAP results.
#' @param coltree vector of k colors with lenght of n tips of the given tree. If NULL, coltree will match colUMAP. Default is NULL.
#' @param palette palette of brewer.pal colors to use. If NULL and colUMAP = NULL, viridis palette will be used. Default NULL.
#' @param plot.names Whether plot image names on UMAP plot, Default TRUE
#' @param plot.images Whether plot images on UMAP plot, Default TRUE
#' @param cex text size. Default 1
#' @param pch type of symbol to display on UMAP data points. Default is 19.
#' @param size size of the images to display on the UMAP plot
#' @param cols vector of colors used for visualize color changes. Default = c("red", "white", "blue")
#'
#' @return The output from \code{\link{imageUMAP.plot}}
#' @export
#' @importFrom raster nlayers plot
#' @importFrom viridis viridis
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics layout par text legend
#' @importFrom scales alpha
#' @importFrom shape Arrows
#' @importFrom ape .PlotPhyloEnv
#'
#' @examples
#' color.list <- viridis::viridis(5)
#' col_region <- stats::setNames(rep(color.list)[as.factor(dataset$region)], dataset$sample)
#' colreg_legend <- stats::setNames(rep(color.list)[as.factor(levels(as.factor(dataset$region)))],
#'                                 levels(as.factor(dataset$region)))
#' imageUMAP.plot(imgUMAP12,  tree = tree, plot.tree = "side",
#'               plot.images = FALSE, cex = 1, plot.names = TRUE,
#'               colUMAP = col_region, legend = colreg_legend)
#' \dontrun{
#' imageUMAP.plot(imgUMAP12, tree = tree, plot.tree = "integrated", plot.images = FALSE)
#' }
imageUMAP.plot <-function(imgUMAP, tree = NULL, plot.tree = c("side", "integrated"), onTop = F, SD = F,
                         col.branches  = F, node.width = 0.5, node.pch = 18, tree.cex = 1, names.cex = 1,
                         legend = NULL, legend.cex = 1, pos = "bottomleft", xlim = NULL, ylim =  NULL, alpha = 1,
                         colUMAP = NULL, coltree = colUMAP, palette = NULL, plot.names = TRUE, plot.images = TRUE,
                         cex = 1, pch = 19, size = 0.1, cols = c("red", "white", "blue"), ...){

  if(!is.null(plot.tree)){
    if(is.null(tree) & !is.null(imgUMAP$tree)) {
      tree = imgUMAP$tree
      plot.tree = plot.tree[1]
    }
    if(is.null(tree) & is.null(imgUMAP$tree)) {
      warning("tree not provided.")
      plot.tree = NULL
    }
  }
  images = imgUMAP$images
  type = imgUMAP$type

  umap.df = imgUMAP$df
  comp <-  imgUMAP$UMAP
  if (is.null(xlim)){
    xlim <- range(umap.df[, 1])
  }
  if (is.null(ylim)){
    ylim <- range(umap.df[, 2])
  }
  xmin <- xlim[1]
  xmax <- xlim[2]
  ymin <- ylim[1]
  ymax <- ylim[2]

  xaxis <- unique(c(seq(floor(xmin), 0, length.out = 3), 0, seq(0, ceiling(xmax), length.out = 3)))
  yaxis <- unique(c(seq(floor(ymin), 0, length.out = 3), 0, seq(0, ceiling(ymax), length.out = 3)))

  if(is.null(palette)){
    colRamp <- grDevices::colorRampPalette(viridis::viridis(9))(n=100)
  }
  else {
    colRamp <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = palette))(n=100)
  }
  if(!is.null(tree) & is.null(coltree)){
    if (all(tree$tip.label %in% rownames(umap.df))){
      nodes <- unique(tree$edge[tree$edge > tree$Nnode +1])
      nodeList <- sapply(as.character(tree$tip.label), function(i) sort(getAncestors(tree, i), decreasing = F))
      coltree <- sapply(nodeList, function(i) colRamp[ceiling((i - min(nodes) + 1)/(diff(range(nodes)) + 1) * 100)])
      coltree <- sapply(coltree, function(i) mix.colors(unlist(i)))
      colUMAP = coltree
    }
    else{
      coltree <- setNames(rep("white", length(tree$tip.label)), tree$tip.label)
    }
  }
  if (is.null(colUMAP)){
    colUMAP <- lapply(1:2, function(j) sapply(rownames(umap.df),
                                             function(i) colRamp[ceiling((umap.df[i,j] +
                                                                            abs(min(umap.df[,j]))+1) * 100 / (diff(range(umap.df[,j]))+1))]))
    colUMAP <- reverse.list(colUMAP)
    colUMAP <- sapply(colUMAP, function(i) mix.colors(unlist(i)))
  }
  colUMAP <- scales::alpha(colUMAP, alpha)

  if (!is.null(tree)){
    if(plot.tree == "integrated"){
      if (!all(tree$tip.label %in% rownames(umap.df))){
        warning("tree provided and UMAP data don't macth. tree cannot be integrated, siwtching to 'side' type... ", immediate. = T)
        plot.tree = "side"
      }
      else{
        graphics::par(mar = c(5, 5, 3, 4))
        plotPhylomorphospace(tree, umap.df[,1:2], cols = colUMAP, cex = tree.cex, node.width = node.width,
                             col.branches = col.branches, node.pch = node.pch, xlim = xlim, ylim = ylim,
                             xlab = "UMAP 1",
                             ylab = "UMAP 2", ...)
        if(plot.images) {
          plotImages(umap.df[,1], umap.df[,2], images, interpolate = T, width = size)
        }
        if (plot.names){
          text(umap.df[, 1], umap.df[, 2], cex = cex, pos = 1 , offset = 1,
               as.character(rownames(umap.df)))
        }
      }
    }
    if(plot.tree == "side"){
      mat <- matrix(c(1, 2, 2,
                      1, 2, 2), 2, 3,
                    byrow = TRUE)
      graphics::layout(mat)
      graphics::par(mar = c(2,2,0,0), oma = c(0,0,0,0))

      coltree <- sapply(tree$edge[,2], function(i)
        if (i %in% 1:(length(tree$tip.label))) {
          coltree[tree$tip.label[i]]
        } else { "black" })
      suppressWarnings({
        plot(tree, cex = tree.cex, edge.color = coltree, ...)
        pp<-get("last_plot.phylo",envir=ape::.PlotPhyloEnv)
        #for(i in 1:length(tree$tip.label)) boxlabel(pp$xx[i], pp$yy[i], tree$tip.label[i], bg=coltree[tree$tip.label[i]], cex = pp$cex, alpha = 0.8)
        graphics::par(mar = c(5, 7, 3, 4))
        plot(umap.df[,1:2], col = colUMAP[rownames(umap.df)],
                              pch = 1, xaxt = "n", yaxt = "n", ann=FALSE, frame.plot=TRUE,
                              cex = 0, xlim = c(xmin, xmax), ylim = c(ymin,ymax),
                              xlab = "UMAP 1",
                              ylab = "UMAP 2", axes = FALSE, ...)
        ## add the ticks
        axis(1, at = xaxis, label = xaxis, tck = -0.01, ...)
        axis(2, at = yaxis, label = yaxis, tck = -0.01, ...)
        })

      if (!onTop){
        if (SD){
          sapply(1:nrow(umap.df), function(i) segments(umap.df[i,1]-umap.df[i,3], umap.df[i,2], umap.df[i,1]+umap.df[i,3], umap.df[i,2], col = scales::alpha(colUMAP[rownames(umap.df)[i]], 0.8) , lwd = 4))
          sapply(1:nrow(umap.df), function(i) segments(umap.df[i,1], umap.df[i,2]-umap.df[i,4], umap.df[i,1], umap.df[i,2]+ umap.df[i,4], col = scales::alpha(colUMAP[rownames(umap.df)[i]], 0.8) , lwd = 4))
        } else{
          points(umap.df[,1], umap.df[,2], col = colUMAP[rownames(umap.df)], pch = pch, cex = cex)
        }
      }
      if(plot.images) {
        plotImages(umap.df[,1], umap.df[,2], images, interpolate = T, width = size)
      }
      if (plot.names){
        graphics::text(umap.df[, 1], umap.df[, 2], cex = names.cex, pos = 1 , offset = 1,
                       as.character(rownames(umap.df)))
      }
      if (onTop){
        if (SD){
          sapply(1:nrow(umap.df), function(i) segments(umap.df[i,1]-umap.df[i,3], umap.df[i,2], umap.df[i,1]+umap.df[i,3], umap.df[i,2], col = scales::alpha(colUMAP[rownames(umap.df)[i]], 0.8) , lwd = 4))
          sapply(1:nrow(umap.df), function(i) segments(umap.df[i,1], umap.df[i,2]-umap.df[i,4], umap.df[i,1], umap.df[i,2]+ umap.df[i,4], col = scales::alpha(colUMAP[rownames(umap.df)[i]], 0.8) , lwd = 4))
        } else{
          points(umap.df[,1], umap.df[,2], col = colUMAP[rownames(umap.df)], pch = pch, cex = cex)
        }
      }
    }
  }
  else{
    graphics::par(mar = c(5, 5, 3, 4))
    suppressWarnings({
      plot(umap.df[,1:2], col = colUMAP[rownames(umap.df)],
                          pch = pch, xaxt = "n", yaxt = "n", ann=FALSE, frame.plot=TRUE,
                          cex = cex, xlim = c(xmin, xmax), ylim = c(ymin,ymax),
                          xlab = "UMAP 1",
                          ylab = "UMAP 2", axes = FALSE, ...)
                     ## add the ticks
                     axis(1, at = xaxis, label = xaxis, tck = -0.01, ...)
                     axis(2, at = yaxis, label = yaxis, tck = -0.01, ...)
                    })
    if(plot.images) {
      plotImages(umap.df[,1], umap.df[,2], images, interpolate = T, width = size)
    }
    if(plot.names){
      graphics::text(umap.df[, 1], umap.df[, 2], cex = names.cex, pos = 1 , offset = 1,
                     as.character(rownames(umap.df)))
    }
  }
  if(!is.null(legend)){
    legend(pos, legend=names(legend), bg = "transparent",
           fill=legend, cex = legend.cex, box.lty = 0)
  }
  graphics::par(mfrow = c(1,1))
}

