% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imagePCA.RGBsideplot.R
\name{imagePCA.RGBsideplot}
\alias{imagePCA.RGBsideplot}
\title{PCA side plot}
\usage{
imagePCA.RGBsideplot(x, q = 0.75, plot = T)
}
\arguments{
\item{x}{imagePCA object}

\item{q}{quantile value to subset images based on their PC distribution}

\item{plot}{Logical whether to plot the side plot.}
}
\value{
The output from \code{\link{print}}
}
\description{
makes the RGB reconstruction side plot of imagePCA.plot function
}
\examples{
library(ColorAR)
data(imgPCA12)
sideplot <- imagePCA.RGBsideplot(imgPCA12)

}
