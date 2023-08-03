#' Get ancestors
#'
#' @param tree get ancestor nodes of a taxon in a 'phylo' class tree
#' @param x tip number
#'
#' @return The output from \code{\link{print}}
#' @export
#' @examples
#' tree <- ape::rtree(26, tip.label = letters[1:26])
#' getAncestors(tree, 5)
#' \dontrun{
#' getAncestors(tree, 10)
#' }
getAncestors <- function(tree, x) {

  if (class(tree) == "phylo"){
    LAnc <- as.integer(length(tree$tip.label)+1)
    if (is.character(x) & x %in% tree$tip.label) {
      descendant <- which(tree$tip.label == x)
    }
    else if (as.integer(x) & x %in% tree$edge) {
      descendant <- as.integer(x)
    }
    else{
      stop("x not found in tree")
    }
    edge <- as.data.frame(tree$edge)
    nodes <- NULL
    while(descendant != LAnc){
      ancestor <- edge[which(edge[,2] == descendant),1]
      nodes <- c(nodes, ancestor)
      descendant <- ancestor
    }
  }
  else{
    stop("tree is not class 'phylo'.")
  }
  return(nodes)
}
