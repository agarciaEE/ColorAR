% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregateGroups.R
\name{aggregateGroups}
\alias{aggregateGroups}
\title{Aggregate groups}
\usage{
aggregateGroups(pca, groups, PCx = 1, PCy = 2, scale = F)
}
\arguments{
\item{pca}{pca obhect from prcomp function.}

\item{groups}{vector of length equal to number of images defining groups to compute centroids and standard deviations PCA scores.}

\item{PCx}{integer indicating which PC component is to be considered as  x axis. Default 1}

\item{PCy}{integer indicating which PC component is to be considered as  y axis. Default 2}

\item{scale}{Whether set scale TRUE/FALSE in the PCA prcomp function. Default FALSE}
}
\value{
The output from \code{\link{aggregateGroups}}
}
\description{
Aggregate PCA scores by groups and compute centroids and standard deviations.
}
\examples{
df <- sapply(1:10, function(i) runif(50))
comp <- stats::prcomp(df)
groups <- sample(1:3, 50, replace = TRUE)
groups_df <- aggregateGroups(comp, groups, PCx = 1, PCy = 2, scale = TRUE)
\dontrun{
df <- sapply(1:10, function(i) runif(100))
comp <- stats::prcomp(df)
groups <- sample(LETTERS[1:5], 100, replace = TRUE)
groups_df <- aggregateGroups(comp, groups, PCx = 1, PCy = 2, scale = TRUE)
}
}
