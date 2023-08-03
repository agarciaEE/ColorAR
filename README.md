
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ColorAR

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/agarciaEE/ColorAR.svg?branch=main)](https://travis-ci.com/agarciaEE/ColorAR)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/agarciaEE/ColorAR?branch=main&svg=true)](https://ci.appveyor.com/project/agarciaEE/ColorAR)
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
#> Warning: replacing previous import 'ape::zoom' by 'raster::zoom' when loading
#> 'ColorAR'
#> Warning: replacing previous import 'ape::rotate' by 'raster::rotate' when
#> loading 'ColorAR'
#> Warning: replacing previous import 'DescTools::Range' by 'scales::Range' when
#> loading 'ColorAR'
#> Warning: replacing previous import 'phytools::rescale' by 'scales::rescale'
#> when loading 'ColorAR'
#> Warning: replacing previous import 'raster::density' by 'stats::density' when
#> loading 'ColorAR'
#> Warning: replacing previous import 'raster::weighted.mean' by
#> 'stats::weighted.mean' when loading 'ColorAR'
#> Warning: replacing previous import 'raster::predict' by 'stats::predict' when
#> loading 'ColorAR'
#> Warning: replacing previous import 'raster::aggregate' by 'stats::aggregate'
#> when loading 'ColorAR'
#> Warning: replacing previous import 'raster::quantile' by 'stats::quantile' when
#> loading 'ColorAR'
#> Warning: replacing previous import 'raster::update' by 'stats::update' when
#> loading 'ColorAR'
#> Warning: replacing previous import 'smoothr::smooth' by 'stats::smooth' when
#> loading 'ColorAR'
#> Warning: replacing previous import 'raster::tail' by 'utils::tail' when loading
#> 'ColorAR'
#> Warning: replacing previous import 'raster::stack' by 'utils::stack' when
#> loading 'ColorAR'
#> Warning: replacing previous import 'raster::unstack' by 'utils::unstack' when
#> loading 'ColorAR'
#> Warning: replacing previous import 'raster::head' by 'utils::head' when loading
#> 'ColorAR'
#> Warning: replacing previous import 'scales::viridis_pal' by
#> 'viridis::viridis_pal' when loading 'ColorAR'
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
