#' Reverse list of lists
#'
#' @description  Reverse list of lists.
#' @param ll list of lists
#'
#' @return The output from \code{\link{reverse.list}}
#' @export
#'
#' @examples
#' library(ColorAR)
#' ll <- list(a = list("A" = 1, "B" = 2, "C" = 3), b = list("D" = 1, "F" = 2, "G" = 3))
#' reverse.list(ll)
reverse.list <-  function(ll) {
  nms <- unique(unlist(lapply(ll, function(X) names(X))))
  ll <- lapply(ll, function(X) setNames(X[nms], nms))
  ll <- apply(do.call(rbind, ll), 2, as.list)
  lapply(ll, function(X) X[!sapply(X, is.null)])
}
