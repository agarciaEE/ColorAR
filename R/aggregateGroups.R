#' Aggregate groups
#'
#' @description  Aggregate PCA scores by groups and compute centroids and standard deviations.
#'
#' @param pca pca obhect from prcomp function.
#' @param groups vector of length equal to number of images defining groups to compute centroids and standard deviations PCA scores.
#' @param PCx integer indicating which PC component is to be considered as  x axis. Default 1
#' @param PCy integer indicating which PC component is to be considered as  y axis. Default 2
#' @param scale Whether set scale TRUE/FALSE in the PCA prcomp function. Default FALSE
#'
#' @return The output from \code{\link{print}}
#' @export
#' @import stats
#' @examples
#' tree <- ape::rtree(26, tip.label = letters[1:26])
#' X <- data.frame(trait1 = runif(26, -10, 10), trait2 = runif(26, -25, 25))
#' plotPhylomorphospace(tree, X)
#' \dontrun{
#' plotPhylomorphospace(tree, X, palette = rainbow(6), col.branches = T)
#' }
aggregateGroups <- function(pca, groups, PCx = 1, PCy = 2, scale = F) {

  df <- pca$x[, c(PCx, PCy)]

  if (missing(groups)) {
    stop("Argument 'groups' is missing.")
  }
  if (scale) {
    df[,PCx] <- scale(df[,PCx])
    df[,PCy] <- scale(df[,PCy])
  }
  centroids <- as.data.frame(t(sapply(unique(groups), function(i) stats::cov.wt(df[which(groups == i),c(PCx, PCy)])$center)))
  sd = as.data.frame(t(sapply(unique(groups), function(i) apply(df[which(groups == i), c(PCx, PCy)], 2, function(j) stats::sd(j)))))
  colnames(sd) = paste0(colnames(sd), ".sd")
  df <- cbind(centroids, sd)

  return(df)
}
