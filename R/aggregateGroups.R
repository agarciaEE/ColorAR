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
#' @return The output from \code{\link{aggregateGroups}}
#' @export
#' @importFrom stats cov.wt sd prcomp
#' @examples
#' df <- sapply(1:10, function(i) runif(50))
#' comp <- stats::prcomp(df)
#' groups <- sample(1:3, 50, replace = T)
#' groups_df <- aggregateGroups(comp, groups, PCx = 1, PCy = 2, scale = T)
#' \dontrun{
#' df <- sapply(1:10, function(i) runif(100))
#' comp <- stats::prcomp(df)
#' groups <- sample(LETTERS[1:5], 100, replace = T)
#' groups_df <- aggregateGroups(comp, groups, PCx = 1, PCy = 2, scale = T)
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
