#' Image PC analysis
#'
#' @description Performs PCA on a list of images
#'
#' @param imgList list of images. Can be raster images or RGB images
#' @param res number of rows to transform the images. If NULL, res will be extracted from imgList (If not all images  have the same resolution, first will be taken). Default NULL.
#' @param tree Optional, if class 'phylo' tree is to be integrated in the output.
#' @param phy.method  Method to obtain the correlation structure in phyl.pca from phytools. It can be "BM" or "lambda".
#' @param groups Optional. vector of length equal to number of images defining groups to compute centroids and standard deviations PCA scores.
#' @param plot.eigen Logical
#' @param plot.PCA Logical
#' @param plot.tree if 'side', tree will be shown next to the PCA analyses. If 'integrated', Phytools::fastAnc will be computed and tree will be integrated into PCA plot. Default 'integated'.
#' @param node.width size of the tree nodes.
#' @param col.branches Logical to whether color the branches of the tree if plot.tree is "integrated". Default is F.
#' @param node.pch type of symbol to display at nodes of the tree if plot.tree is "integrated". Default is 18.
#' @param size size of the images to display on the PCA plot
#' @param fill.NAs If TRUE, NAs on images will be filled with the average color value of all images. Default is FALSE.
#' @param PCx integer indicating which PC component is to be considered as  x axis. Default 1
#' @param PCy integer indicating which PC component is to be considered as  y axis. Default 2
#' @param plot.names Whether plot image names on PCA plot, Default TRUE
#' @param plot.images Whether plot images on PCA plot, Default TRUE
#' @param interpolate Integer. Image interpolation to reduce NA values created by image transformation. Default = NULL.
#' @param cex text size. Default 1
#' @param type Whether analysis is to be performed on RGB images or rasters.
#' @param as.RGB Logical whether to display side plot as RGB, in which case patterns of differences on the PC axes will be reconstructed as RGB images.
#' @param scale Whether set scale TRUE/FALSE in the PCA prcomp function. Default FALSE
#' @param cols vector of colors used for visualize color changes. Default = c("red", "white", "blue")
#' @param colPCA vector of k colors with length of n images. names of each color must match the name of image to represent. If NULL it will be computed based on distances of PCA results.
#' @param coltree vector of k colors with length of n tips of the given tree. If NULL, coltree will match colPCA. Default is NULL.
#' @param palette palette of brewer.pal colors to use. If NULL and colPCA = NULL, viridis palette will be used. Default NULL.
#'
#' @return The output from \code{\link{imagePCA}}
#' @export
#' @importFrom stats na.exclude
#' @importFrom raster nrow ncol extent raster resample as.data.frame stack mean crs ncell
#' @importFrom sp disaggregate
#' @importFrom graphics barplot
#'
#' @examples
#' library(ColorAR)
#' data(imgTransList)
#' imgPCA12 <-  imagePCA(imgTransList, PCx = 1, PCy = 2, scale = FALSE,
#'                      plot.eigen = FALSE, plot.PCA = FALSE,
#'                      interpolate = 5, plot.names = FALSE, plot.images = FALSE,
#'                      plot.tree = NULL, type = "RGB" , as.RGB = FALSE)
#'
imagePCA <- function(imgList, res = NULL, tree = NULL, phy.method = "BM", groups = NULL, plot.eigen = TRUE, plot.PCA = TRUE, plot.tree = "integrated",
                     node.width = 0.5, col.branches = FALSE, node.pch = 18, size = 0.1, fill.NAs = FALSE,
                     PCx = 1, PCy = 2, plot.names = T, plot.images = TRUE, interpolate = NULL,  cex = 1, type = c("RGB", "decimal", "raster"), as.RGB = FALSE,
                     scale = FALSE, cols = c("red", "grey90", "blue"), colPCA = NULL, coltree = colPCA, palette = NULL){

  out = list()
  type = type[1]
  if(raster::nlayers(imgList[[1]]) != 3 & type %in% c("RGB", "decimal")){
    warning("imgList is not in RGB format. imagePCA will use type 'raster'", immediate. = T)
    type = "raster"
  }
  if(raster::nlayers(imgList[[1]]) == 3 & type == "raster"){
    warning("imgList is in RGB format. imagePCA will use type 'RGB'", immediate. = T)
    type = "RGB"
  }
  for (n in 1:length(imgList)) {
    r = imgList[[n]]
    NR <- raster::nrow(r)
    NC <- raster::ncol(r)
    if (is.integer(interpolate)) {
      ras = as.vector(raster::extent(r))
      rRe <- raster::raster(nrow=NR,ncol=NC)
      raster::extent(rRe) <- ras
      r = sp::disaggregate(r, fact = interpolate, method = "bilinear")
      r = raster::resample(r, rRe, method = "ngb")
    }
    if(!is.null(res)){
      e = as.vector(raster::extent(r))
      ratio = (e[2]-e[1])/(e[4]-e[3])
      ras = e
      rRe <- raster::raster(nrow=res,ncol=floor(res*ratio))
      raster::crs(rRe) = NA
      raster::extent(rRe) <- ras
      r = raster::resample(r, rRe, method = "ngb")
    }
    if(type == "RGB"){
      if (n == 1) {
        rasDF <- raster::as.data.frame(as.vector(r))
      }
      else {
        rasDF <- cbind(rasDF, raster::as.data.frame(as.vector(r)))
      }
    }
    else {
      if(type == "decimal"){
        r = r[[1]]*256^2 + r[[2]]*256 + r[[3]]
      }
      if (n == 1) {
        rasDF <- raster::as.data.frame(r)
      }
      else {
        rasDF <- cbind(rasDF, raster::as.data.frame(r))
      }
    }
  }
  colnames(rasDF) = names(imgList)
  if (fill.NAs){
    mean_img <- raster::stack(raster::mean(raster::stack(lapply(imgList, function(i) i[[1]])), na.rm = T),
                      raster::mean(raster::stack(lapply(imgList, function(i) i[[2]])), na.rm = T),
                      raster::mean(raster::stack(lapply(imgList, function(i) i[[3]])), na.rm = T))
    if(type == "RGB"){
      mean_img_df <- raster::as.data.frame(as.vector(r))
    } else {
      mean_img_df <- raster::as.data.frame(mean_img)
    }
    rs <- rowSums(rasDF)
    NA.cells <- which(is.na(rs) & !is.na(mean_img_df))
    rasDF[NA.cells, ] <- mean_img_df[NA.cells, ]
  }
  rasDF = stats::na.exclude(rasDF)
  rw.val = rownames(rasDF)
  df = t(rasDF)
  if (!is.null(tree)){
    comp <- as.prcomp(phyl.pca(tree, df, method = phy.method, mode = ifelse(scale, "corr", "cov")))
  } else {
    comp <- stats::prcomp(df, scale. = scale)
  }
  pcdata <- comp$x
  rotation <- comp$rotation
  summ <- summary(comp)
  eigen = comp$sdev^2
  variance = eigen*100/sum(eigen)
  cumvar = cumsum(variance)
  if (plot.eigen){
    v = which(cumvar >= 60)[1]
    col = c(rep("blue", v), rep("lightgrey", length(cumvar)-v))
    par(mfrow = c(1,1))
    graphics::barplot(variance, col = col, main = paste(n, "color PC variance explained"))
  }
  PCxmin <- min(pcdata[, PCx])
  PCxmax <- max(pcdata[, PCx])
  PCymin <- min(pcdata[, PCy])
  PCymax <- max(pcdata[, PCy])
  pc.vecMix <- rep(0, dim(pcdata)[1])
  pc.vecMix[PCx] <- PCxmin
  pc.vecMax <- rep(0, dim(pcdata)[1])
  pc.vecMax[PCx] <- PCxmax
  pc.vecMiy <- rep(0, dim(pcdata)[1])
  pc.vecMiy[PCy] <- PCymin
  pc.vecMay <- rep(0, dim(pcdata)[1])
  pc.vecMay[PCy] <- PCymax
  if(type == "RGB"){
    n <- length(comp$center)/3
    xy <- raster::xyFromCell(r, 1:raster::ncell(r))
    rw.val <- rw.val[1:n]
    center <- lapply(1:3, function(i) comp$center[(n*i-n+1):(n*i)])
    rotation <- lapply(1:3, function(i) rotation[(n*i-n+1):(n*i),])
    xMi <- lapply(1:3, function(i) rep(NA, NC*NR))
    xMa <- lapply(1:3, function(i) rep(NA, NC*NR))
    xCt <- lapply(1:3, function(i) rep(NA, NC*NR))
    yMi <- lapply(1:3, function(i) rep(NA, NC*NR))
    yMa <- lapply(1:3, function(i) rep(NA, NC*NR))
    if(as.RGB){
      for (i in 1:3){
        xCt[[i]][as.numeric(rw.val)] = scales::rescale(center[[i]], to = c(0,255))
        xMi[[i]][as.numeric(rw.val)] = scales::rescale(as.vector(pc.vecMix %*% t(rotation[[i]])), to = c(0,255))
        xMa[[i]][as.numeric(rw.val)] = scales::rescale(as.vector(pc.vecMax %*% t(rotation[[i]])), to = c(0,255))
        yMi[[i]][as.numeric(rw.val)] = scales::rescale(as.vector(pc.vecMiy %*% t(rotation[[i]])), to = c(0,255))
        yMa[[i]][as.numeric(rw.val)] = scales::rescale(as.vector(pc.vecMay %*% t(rotation[[i]])), to = c(0,255))
      }
    }
    if(!as.RGB){
      for (i in 1:3){
        xCt[[i]][as.numeric(rw.val)] = scales::rescale(center[[i]], to = c(0,255))
        xMi[[i]][as.numeric(rw.val)] = scales::rescale(as.vector(pc.vecMix %*% t(rotation[[i]])), to = c(-1,1))
        xMa[[i]][as.numeric(rw.val)] = scales::rescale(as.vector(pc.vecMax %*% t(rotation[[i]])), to = c(-1,1))
        yMi[[i]][as.numeric(rw.val)] = scales::rescale(as.vector(pc.vecMiy %*% t(rotation[[i]])), to = c(-1,1))
        yMa[[i]][as.numeric(rw.val)] = scales::rescale(as.vector(pc.vecMay %*% t(rotation[[i]])), to = c(-1,1))
      }
    }
    mapc <- raster::stack(sapply(1:3, function(i) raster::raster(t(matrix(xCt[[i]], ncol = NR,
                                                                  nrow = NC)))))
    #mapc <- raster::stack(sapply(1:3, function(x) raster::mean(raster::stack(sapply(imgList, function(i) i[[x]])), na.rm = T)))

    mapMix <- raster::stack(sapply(1:3, function(i) raster::raster(t(matrix(xMi[[i]], ncol = NR,
                                                                    nrow = NC)))))
    mapMax <- raster::stack(sapply(1:3, function(i) raster::raster(t(matrix(xMa[[i]], ncol = NR,
                                                                    nrow = NC)))))
    mapMiy <- raster::stack(sapply(1:3, function(i) raster::raster(t(matrix(yMi[[i]], ncol = NR,
                                                                    nrow = NC)))))
    mapMay <- raster::stack(sapply(1:3, function(i) raster::raster(t(matrix(yMa[[i]], ncol = NR,
                                                                    nrow = NC)))))
    mapList <- list(mapc, mapMix, mapMax, mapMiy, mapMay)
    names(mapList) <- c("center", paste0("minPC",PCx),  paste0("maxPC",PCx),  paste0("minPC",PCy),  paste0("maxPC",PCy))
    if(!as.RGB){
      mapList[-1] <- lapply(mapList[-1], function(i) raster::mean(i, na.rm = T))
      mapList[-1] <- transform.mapList(imgList, mapList[-1],  res = res, is.NA = 0, type = "RGB", interpolate = interpolate)
      mapList[[1]] = transform.mapList(imgList, list(mapList[[1]]), res = res, is.NA = rep(204,3), type = "RGB", interpolate = interpolate)[[1]]
    }
    if(as.RGB){
      mapList <- transform.mapList(imgList, mapList, res = res, is.NA = rep(204,3), type = "RGB", interpolate = interpolate)
    }
  }
  else{
    as.RGB = F
    xMi <- rep(NA, NC*NR)
    xMa <- rep(NA, NC*NR)
    xCt = rep(NA, NC*NR)
    xCt[as.numeric(rw.val)] = comp$center
    xMi[as.numeric(rw.val)] = as.vector(pc.vecMix %*% t(rotation))
    xMa[as.numeric(rw.val)] = as.vector(pc.vecMax %*% t(rotation))
    x2c <- t(matrix(xCt, ncol = NR,
                    nrow = NC))
    x2Mi <- t(matrix(xMi, ncol = NR,
                     nrow = NC))
    x2Ma<- t(matrix(xMa, ncol = NR,
                    nrow = NC))
    yMi <- rep(NA, NC*NR)
    yMa <- rep(NA, NC*NR)
    yMi[as.numeric(rw.val)] = as.vector(pc.vecMiy %*% t(rotation))
    yMa[as.numeric(rw.val)] = as.vector(pc.vecMay %*% t(rotation))
    y2Mi <- t(matrix(yMi, ncol = NR,
                     nrow = NC))
    y2Ma <- t(matrix(yMa, ncol = NR,
                     nrow = NC))
    mapc <- raster::raster(x2c)
    mapMix <- raster::raster(x2Mi)
    mapMax <- raster::raster(x2Ma)
    mapMiy <- raster::raster(y2Mi)
    mapMay <- raster::raster(y2Ma)
    mapList <- list(mapc, mapMix, mapMax, mapMiy, mapMay)
    names(mapList) <- c("center", paste0("minPC",PCx),  paste0("maxPC",PCx),  paste0("minPC",PCy),  paste0("maxPC",PCy))
    mapList <- transform.mapList(imgList, mapList, type = "raster", interpolate = interpolate)
  }
  if (!is.null(groups)){
    df <- aggregateGroups(comp, groups, PCx = PCx, PCy = PCy, scale = scale)
    imgList <-  sapply(unique(groups), function(g) raster::stack(raster::mean(raster::stack(lapply(imgList[which(groups == g)], function(i) i[[1]])), na.rm = T),
                                             mean(raster::stack(lapply(imgList[which(groups == g)], function(i) i[[2]])), na.rm = T),
                                             mean(raster::stack(lapply(imgList[which(groups == g)], function(i) i[[3]])), na.rm = T)))
  } else {
    df <- pcdata[,c(PCx, PCy)]
  }
  out$images = imgList
  out$df = as.data.frame(df)
  out$pca = comp
  out$cellIDs = rw.val # images cell IDs used on PCA
  out$ras = mapList
  if (!is.null(tree)){out$tree = tree}
  if (!is.null(groups)){out$groups = groups}
  out$type = type
  out$components = colnames(pcdata)[c(PCx, PCy)]
  if (plot.PCA){
    imagePCA.plot(out, plot.tree = plot.tree,
                  colPCA = colPCA, coltree = coltree, node.width = node.width,
                  col.branches = col.branches, node.pch = node.pch,
                  palette = palette, plot.names = plot.names, plot.images = plot.images,
                  cex = cex, size = size, cols = cols)
  }
  return(out)
}
