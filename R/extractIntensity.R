#' Color intensity
#'
#' @description Extract color intensity
#'
#' @param image RGB raster image
#' @param RGB n rows by 3 columns data.frame with target colors to classify. each column must be R, G and B color values, whereas numer of rows will be number of target colors. Default is NULL.
#' @param method Wheter to use RGB or decimal color format. Default is 'RGB'.
#' @param colOffset Numeric value between 0 and 1. If NULL, colOffset will be estimated. color offset threshold to classify as target color. Default is NULL.
#' @param output type of output desired. If 'value', gives the average color intensity  of the image. If 'raster' gives a raster with values from 0 to 1 with the intensity value.
#' @param plot Wheather plot classyfied image or not. Default is FALSE
#'
#' @return The output from \code{\link{extractIntensity}}
#' @export
#' @importFrom stats na.exclude density
#' @importFrom raster as.data.frame stack maxValue
#' @examples
#' img <- imgTransList[[1]]
#' targetColor <- c(255, 165, 0)
#' Orange_intensity <- extractIntensity(img, targetColor)
#' \dontrun{
#' img <- imgTransList[[2]]
#' targetColor <- c(255, 255, 255)
#' White_intensity<- extractIntensity(img, targetColor)
#' }
extractIntensity <- function(image, RGB = c(0,0,0), method = c("RGB", "dec"),
                             colOffset = NULL, output = c("both", "value", "raster"), plot = F){

  method <-  method[1]
  output <- output[1]
  if(is.null(colOffset)){
    colOffset <- colOffset(image, RGB)
  }
  if (output != "raster"){
    df <- stats::na.exclude(raster::as.data.frame(image)) # get the RGB color codes
    if(method == "RGB"){
      range <- c(0,255*colOffset)
      check <- sum(duplicated(rbind(unique(df), RGB)))
      tab <- apply(df, 2, function(i) as.numeric(names(table(i))[table(i) > mean(table(i))]))
      tab <- lapply(1:3, function(i) abs(tab[[i]]-RGB[i]))
      if(all(sapply(tab, function(i) any(i == 0))) & check == 1){
        message("Target colour within colour candidates")
      }
      else{
        message("Target color is not a predominant color.")
      }
      df <- sapply(1:3, function(i) abs(df[,i] - RGB[i])) # transform to distance to target color
      d <- lapply(1:3, function(i) stats::density(df[,i])) # compute density on distances
      idx <- lapply(1:3, function(i) which(d[[i]]$x >= range[1] & d[[i]]$x <= range[2])) # keep only values within color range (0-255)
      y <- lapply(1:3, function(i) d[[i]]$y[idx[[i]]]/sum(d[[i]]$y)) # compute relative densities
      x <- lapply(1:3, function(i) 1-(d[[i]]$x[idx[[i]]]/range[2])) # assign weight coefficient based on distances from target color
      I <- mean(sapply(1:3, function(i) sum(y[[i]]*x[[i]]))) # compute mean of RGB color intensity estimates
    }
    if (method == "dec"){
      range <- c(0,256^3*colOffset)
      RGBdec <- RGB[1]*256^2 + RGB[2]*256 +RGB[3]
      dec <- df[,1]*256^2 + df[,2]*256 +df[,3]
      tab <- as.numeric(names(table(dec))[table(dec) > mean(table(dec))])
      if (RGBdec %in% tab){
        message("Target colour within colour candidates.")
      }
      else{
        warning("Target color is not a predominant color.", immediate. = T)
      }
      df <- abs(dec - RGBdec) # transform to distance to target color
      d <- stats::density(df) # compute density on distances
      idx <- which(d$x >= range[1] & d$x <= range[2]) # keep only values within color offset (0-255)
      y <- d$y[idx]/sum(d$y) # compute relative densities
      x <- 1-(d$x[idx]/range[2]) # assign weight coefficient based on distances from target color
      I <- sum(y*x)
    }
  }
  if (output != "value"){
    if (method == "RGB"){
      range <- c(0,255*colOffset)
      ras <- sapply(1:3, function(i) abs(image[[i]] - RGB[i]))
      for (i in 1:3){
        ras[[i]][ras[[i]] > range[2]] = NA
        ras[[i]] <- 1 - ras[[i]]^2 / range[2]^2
      }
      ras <- sum(raster::stack(ras), na.rm = T)
      ras <- ras/raster::maxValue(ras)
    }
    if (method == "dec"){
      range <- c(0,256^3*colOffset^3)
      RGBdec <- RGB[1]*256^2 + RGB[2]*256 +RGB[3]
      ras <- image[[1]]*256^2 + image[[2]]*256 + image[[3]]
      ras <- abs(ras - RGBdec)
      #ras[ras > range[2]] = NA
      ras <- 1 - ras^(1/3) / range[2]^(1/3)
    }
    if(plot){ plot(ras)}
  }
  if (output == "value"){ return(I) }
  if (output == "raster"){ return(ras) }
  if (output == "both"){ return(list(I = I, ras = ras)) }
}
