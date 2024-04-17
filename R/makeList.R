# copy of Patternize function of the same name but removing the STUPID crs assignment

makeList <- function (IDlist, type, prepath = NULL, extension = NULL, format = "imageJ", 
          tpsFile = NULL, skipLandmark = NULL) 
{
  objectList <- list()
  if (!is.null(skipLandmark)) {
    skipLandmark <- -1 * skipLandmark
  }
  for (n in 1:length(IDlist)) {
    if (format == "imageJ") {
      print(paste("sample", n, IDlist[n], "added to list", 
                  sep = " "))
      if (type == "landmark") {
        if (is.null(prepath)) {
          landmarks <- read.table(paste(IDlist[n], extension, 
                                        sep = ""), header = FALSE, stringsAsFactors = FALSE, 
                                  colClasses = c("numeric", "numeric"))
        }
        else {
          landmarks <- read.table(paste(prepath, "/", 
                                        IDlist[n], extension, sep = ""), header = FALSE, 
                                  stringsAsFactors = FALSE, colClasses = c("numeric", 
                                                                           "numeric"))
        }
        landmarks <- as.matrix(landmarks)
        colnames(landmarks) <- NULL
        if (!is.null(skipLandmark)) {
          landmarks <- landmarks[skipLandmark, ]
        }
        objectList[[IDlist[n]]] <- landmarks
      }
    }
    if (type == "image") {
      if (is.null(prepath)) {
        suppressWarnings(image <- raster::stack(paste(IDlist[n], 
                                                      extension, sep = "")))
        #crs(image) <- sp::CRS("+init=EPSG:4326")
      }
      else {
        suppressWarnings(image <- raster::stack(paste(prepath, 
                                                      "/", IDlist[n], extension, sep = "")))
        #crs(image) <- sp::CRS("+init=EPSG:4326")
      }
      objectList[[IDlist[n]]] <- image
    }
  }
  if (all(c(type == "landmark", format == "tps"))) {
    objectListX <- readland.tps(tpsFile, specID = "imageID", 
                                warnmsg = FALSE)
    objectList <- lapply(1:dim(objectListX)[3], function(i) objectListX[, 
                                                                        , i])
    names(objectList) <- IDlist
  }
  return(objectList)
}
