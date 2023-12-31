% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.mapList.R
\name{transform.mapList}
\alias{transform.mapList}
\title{Create image mapList}
\usage{
\method{transform}{mapList}(
  imgList,
  mapList,
  res = NULL,
  type = c("RGB", "raster"),
  is.NA = 0,
  interpolate = NULL
)
}
\arguments{
\item{imgList}{a list of images.}

\item{mapList}{legend list.}

\item{res}{resolution.}

\item{type}{whether RGB or raster format.}

\item{is.NA}{value to give to NA values.}

\item{interpolate}{Integer for the interpolating factor.}
}
\value{
The output from \code{\link{transform.mapList}}
}
\description{
Internal function to transform images for PCA legend.
}
