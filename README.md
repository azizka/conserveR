# conserveR 1.0.4

<!-- badges: start -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check](https://github.com/azizka/conserveR/workflows/R-CMD-check/badge.svg)](https://github.com/azizka/conserveR/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/conserveR)](https://CRAN.R-project.org/package=conserveR)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/conserveR)](https://cranlogs.r-pkg.org:443/badges/grand-total/conserveR)
[![rstudio mirror downloads](https://cranlogs.r-pkg.org/badges/conserveR)](https://cranlogs.r-pkg.org:443/badges/conserveR)
<!-- badges: end -->

Helping biologists to chose the most suitable approach to link their research to conservation. After answering few questions on the type of data available, geographic and taxonomic scope, conserveR ranks existing methods for conservation prioritization and systematic conservation planning by suitability.

## Background
The methods data base of conserveR contains 134 methods for conservation prioritization based on a systematic review of > 12,000 scientific publications from the fields of spatial conservation prioritization, systematic conservation planning, biogeography and ecology.

## Installation
Either from CRAN

```r
install.packages("conserveR")
```

Or, the developemental version from GitHub/

``` r
library(devtools)

install_github(repo = "azizka/conserveR")
```

## Example
The find_methods function will prompt the conserveR input dialog and in the end return a data.frame of conservation prioritization methods ranked by suitability, as well as a visualization of those methods similar to the most suitable ones. 

``` r
library(conserveR)

out <- find_method()

out

map_selection(out)

```

## Citation

```
citation("conserveR")
```

