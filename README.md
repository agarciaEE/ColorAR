
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ColorAR

<!-- badges: start -->
<!-- badges: end -->

The goal of ColorAR is to …

## Installation

You can install the development version of ColorAR like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ColorAR)
#> Warning: remplacement de l'importation précédente 'ape::zoom' par
#> 'raster::zoom' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'ape::rotate' par
#> 'raster::rotate' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'DescTools::Range' par
#> 'scales::Range' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'phytools::rescale' par
#> 'scales::rescale' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'raster::density' par
#> 'stats::density' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'raster::weighted.mean' par
#> 'stats::weighted.mean' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'raster::predict' par
#> 'stats::predict' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'raster::aggregate' par
#> 'stats::aggregate' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'raster::quantile' par
#> 'stats::quantile' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'raster::update' par
#> 'stats::update' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'smoothr::smooth' par
#> 'stats::smooth' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'raster::tail' par
#> 'utils::tail' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'raster::stack' par
#> 'utils::stack' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'raster::unstack' par
#> 'utils::unstack' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'raster::head' par
#> 'utils::head' lors du chargement de 'ColorAR'
#> Warning: remplacement de l'importation précédente 'scales::viridis_pal' par
#> 'viridis::viridis_pal' lors du chargement de 'ColorAR'
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
