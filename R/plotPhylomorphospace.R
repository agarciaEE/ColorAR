#' Plot phylomorphospace
#'
#' @description Modifyed phytools phylomorphospace function. Plot integrated phylogenetic tree on colour PCA.
#'
#' @param tree a phylogenetic tree in "phylo" format, or a modified "phylo" object with a mapped discrete character.
#' @param X an n x 2 matrix of tip values for two characters in n species.
#' @param cols a vector of colors.
#' @param palette a list of colors to use for the branches.
#' @param cex text font size.
#' @param node.pch node symbol.
#' @param node.width node width.
#' @param tip.pch tip symbol.
#' @param xlab x label text.
#' @param ylab y label text.
#' @param xlim x value range.
#' @param ylim y value range.
#' @param col.branches Logical whether to color the branches.
#' @param add Logical whether to add plot to the previous one.
#'
#' @return The output from \code{\link{plotPhylomorphospace}}
#' @export
#' @importFrom ape unroot rtree is.rooted
#' @importFrom grDevices colorRampPalette
#' @importFrom viridis viridis
#' @importFrom phytools fastAnc
#'
#' @examples
#' library(ColorAR)
#' tree <- ape::rtree(26, tip.label = letters[1:26])
#' X <- data.frame(trait1 = runif(26, -10, 10), trait2 = runif(26, -25, 25))
#' plotPhylomorphospace(tree, X)
#' \dontrun{
#' plotPhylomorphospace(tree, X, palette = rainbow(6), col.branches = T)
#' }
plotPhylomorphospace <- function(tree, X, cols = NULL,
                             palette = NULL, cex = 1,
                             node.pch = 18, node.width = 0.5, tip.pch = 19,
                             xlab="trait 1", ylab="trait 2",
                             xlim = range(X[, 1]), ylim = range(X[, 2]),
                             col.branches = F, add = F){

  if(ape::is.rooted(tree)){ tree <- ape::unroot(tree) }
  if (is.null(cols)){
    if(is.null(palette)){
      colRamp <- grDevices::colorRampPalette(viridis::viridis(9))(n=100)
    }
    else {
      colRamp <- grDevices::colorRampPalette(palette)(n=100)
    }
    nodes <- unique(tree$edge[tree$edge > tree$Nnode +1])
    nodeList <- sapply(as.character(tree$tip.label), function(i) sort(getAncestors(tree, i), decreasing = F))
    cols <- sapply(nodeList, function(i) colRamp[ceiling((i - min(nodes) + 1)/(diff(range(nodes)) + 1) * 100)])
    cols <- sapply(cols, function(i) mix.colors(unlist(i)))
  }
  if (col.branches){
    names(tree$tip.label) = NULL
    desc <- sapply(tree$tip.label, function(i) getAncestors(tree, i))
    nd <- sapply(unique(as.vector(tree$edge)), function(i) sapply(names(desc), function(j) if(i %in% desc[[j]]){1} else{0}))
    colnames(nd) = unique(as.vector(tree$edge))
    for (i in 1:length(tree$tip.label)) {nd[tree$tip.label[i],as.character(i)] = 1}
    nd <- sapply(colnames(nd),function(i) unique(cols[rownames(nd)[nd[,i] == 1]]))
    col.edge <- sapply(nd, function(i) mix.colors(i))
  }
  else {
    names(tree$tip.label) = NULL
    col.edge <- setNames(rep("grey20", length(unique(as.vector(tree$edge)))),  unique(as.vector(tree$edge)))
  }

  A <- apply(X, 2, phytools::fastAnc, tree = tree)

  aa <- setNames(c(X[tree$tip.label, 1], A[, 1]), c(1:length(tree$tip.label),
                                                    rownames(A)))
  bb <- setNames(c(X[tree$tip.label, 2], A[, 2]), c(1:length(tree$tip.label),
                                                    rownames(A)))
  XX <- matrix(aa[as.character(tree$edge)], nrow(tree$edge),
               2)
  YY <- matrix(bb[as.character(tree$edge)], nrow(tree$edge),
               2)
  if (!add){
    plot(x = A[1, 1], y = A[1, 2], xlim = xlim, ylim = ylim,
         xlab = xlab, ylab = ylab, pch = 16, cex = 0.1, col = "white",
         axes = T, frame.plot = TRUE)
  }
  for (i in 1:nrow(XX)) lines(XX[i, ], YY[i, ],
                              col = mix.colors(c(col.edge[as.character(tree$edge[i,1])],
                                                 col.edge[as.character(tree$edge[i,2])])), lwd = 1)

  zz <- c(tree$edge[1, 1], tree$edge[tree$edge[, 2] > length(tree$tip.label),
                                     2])
  points(XX[1,1], YY[1,1], pch = 8, cex = node.width*cex, col = col.edge[as.character(zz[1])])
  points(XX[tree$edge[, 2] > length(tree$tip.label),
            2],YY[tree$edge[, 2] > length(tree$tip.label),
                  2], pch = node.pch, cex = node.width*cex, col = col.edge[as.character(zz[-1])])
  zz <- sapply(1:length(tree$tip.label), function(x, y) which(x ==
                                                                y), y = tree$edge[, 2])
  points(XX[tree$edge[, 2] <= length(tree$tip.label), 2], YY[tree$edge[,
                                                                       2] <= length(tree$tip.label), 2], pch = tip.pch, cex = cex,
         col = cols[tree$tip.label])
}
