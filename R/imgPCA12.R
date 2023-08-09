#' @title image PCA object
#'
#' @description object list containing the output of
#' \code{imagePCA} function ran on the imageList dataset selecting
#' the first two principal components.
#'
#' @format A \code{list} with 6 elements, which are:
#' \describe{
#' \item{images}{ list of images}
#' \item{df}{soutput data frame containing the scores from the selected axes on the PCA}
#' \item{pca}{\code{prcomp} object}
#' \item{cellIDs}{ a vector of cell IDs related to the positions in which PCA is performed}
#' \item{ras}{ Raster list containing the center, minPCs and maxPCs images}
#' \item{type}{ Selected type of imagePCA}
#' \item{components}{ Principal Components selected}
#' }
"imgPCA12"
