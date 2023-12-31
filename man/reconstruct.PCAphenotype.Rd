% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reconstruct.PCAphenotype.R
\name{reconstruct.PCAphenotype}
\alias{reconstruct.PCAphenotype}
\title{Reconstruct phenotype}
\usage{
reconstruct.PCAphenotype(x, imagePCA, PCnames = NULL, interpolate = 5)
}
\arguments{
\item{x}{matrix of number of columns equal to the number of PC used for the reconstruction and number of rows equal to the number of phenotype reconstructions to do. column names must match imagePCA object PC scores column names.}

\item{imagePCA}{imagePCA object}

\item{PCnames}{name of PCs to use in the reconstruction. If NULL all will be taken, Default is NULL.}

\item{interpolate}{Integer. Image interpolation to reduce NA values created by image transformation. Default = NULL.}
}
\value{
The output from \code{\link{reconstruct.PCAphenotype}}
}
\description{
Reconstruct color phenotype from PCA
}
\examples{
library(ColorAR)
spca.coords <- data.frame(imgPCA12$pca$x[1:2,])
rPCAphen <- reconstruct.PCAphenotype(spca.coords, imgPCA12)
par(mfrow = c(2,2))
raster::plotRGB(rPCAphen[[1]])
raster::plotRGB(rPCAphen[[2]])
\dontrun{
library(ColorAR)
rpca.coords <- data.frame(apply(imgPCA12$pca$x[,1:5], 2,
                            function(i) sample(i))[1:5,], row.names = 1:5)
rPCAphen <- reconstruct.PCAphenotype(rpca.coords, imgPCA12)
}
}
