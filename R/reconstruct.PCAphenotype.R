#' Reconstruct phenotype
#'
#' @description Reconstruct color phenotype from PCA
#' @param x matrix of number of columns equal to the number of PC used for the reconstruction and number of rows equal to the number of phenotype reconstructions to do. column names must match imagePCA object PC scores column names.
#' @param imagePCA imagePCA object
#' @param PCnames name of PCs to use in the reconstruction. If NULL all will be taken, Default is NULL.
#' @param interpolate Integer. Image interpolation to reduce NA values created by image transformation. Default = NULL.
#'
#' @return The output from \code{\link{reconstruct.PCAphenotype}}
#' @export
#' @importFrom stats na.exclude
#' @importFrom scales rescale
#' @importFrom raster nrow ncol extent raster crs xyFromCell stack disaggregate aggregate ncell
#'
#' @examples
#' library(ColorAR)
#' spca.coords <- data.frame(imgPCA12$pca$x[1:2,])
#' rPCAphen <- reconstruct.PCAphenotype(spca.coords, imgPCA12)
#' par(mfrow = c(2,2))
#' raster::plotRGB(rPCAphen[[1]])
#' raster::plotRGB(rPCAphen[[2]])
#' \dontrun{
#' library(ColorAR)
#' rpca.coords <- data.frame(apply(imgPCA12$pca$x[,1:5], 2,
#'                             function(i) sample(i))[1:5,], row.names = 1:5)
#' rPCAphen <- reconstruct.PCAphenotype(rpca.coords, imgPCA12)
#' }
reconstruct.PCAphenotype <- function(x, imagePCA, PCnames = NULL, interpolate = 5) {

  x = as.data.frame(x)
  if(is.null(PCnames)) { PCnames <- colnames(x) }
  else {
    if (!PCnames %in% colnames(x)) {
      stop("PCnames not found as column names in 'x'.")
    }
  }
  # extract mean image from PCA
  r = imagePCA$ras$center
  NR <- raster::nrow(r)
  NC <- raster::ncol(r)
  # create raster template
  ras = raster::extent(r)
  rRe <- raster::raster(nrow=NR,ncol=NC)
  raster::crs(rRe) = NA
  raster::extent(rRe) <- ras
  # extract cell IDs with values
  rasDF <- stats::na.exclude(raster::as.data.frame(r))
  # get image cell IDs used for PCA
  cellIDs = imagePCA$cellIDs

  comp <- imagePCA$pca  # get PCA data
  pcdata <- comp$x # get PCA scores
  rotation <- comp$rotation # get rotation matrix

  n <- length(comp$center)/3 # number of cells for each RGB layer
  xy <- raster::xyFromCell(r, 1:raster::ncell(r)) # coordinates of the images
  cellIDs <- cellIDs[1:n] # get only the row names of the first layer

  center <- lapply(1:3, function(i) comp$center[(n*i-n+1):(n*i)]) # split centers per layer
  rotation <- lapply(1:3, function(i) rotation[(n*i-n+1):(n*i),]) # split rotation per layer

  pc.vec <- rep(0, dim(pcdata)[1]) # create empty vector with PCA data dimensions
  xR <- list() # empty list for recontructed vectors
  mapR <-  list()

  for(i in 1:nrow(x)) {
    pc.vec[as.numeric(gsub("PC", "", PCnames))] <- as.numeric(x[i,PCnames])
    xR[[i]] <- lapply(1:3, function(j) rep(NA, NC*NR))
    for (j in 1:3){
      xR[[i]][[j]][as.numeric(cellIDs)] = as.integer(scales::rescale(as.vector(pc.vec %*% t(rotation[[j]])), to = c(0,255)))
    }
    mapR[[i]] <- raster::stack(sapply(1:3, function(j) raster::raster(t(matrix(xR[[i]][[j]], ncol = NR,
                                                                       nrow = NC)))))
    if (is.numeric(interpolate)) {
      mapR[[i]] <-  raster::disaggregate(mapR[[i]], fact = interpolate, method = "bilinear")
      mapR[[i]] <- raster::aggregate(mapR[[i]], interpolate)
      for (j in 1:3){
        mapR[[i]][[j]][] <- as.integer(mapR[[i]][[j]][])
      }
    }
    raster::extent(mapR[[i]]) <-  raster::extent(rRe)
  }
  names(mapR) <- as.character(rownames(x))
  return(mapR)
}
