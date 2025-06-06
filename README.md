README
================
Chris Thaxter
21/05/2025

<!-- README.md is generated from README.Rmd. Please edit that file, but only with package maintainer's permission -->

# RakeRvis

### Current version: 0.0.0.9000

The `RakeRvis` package is a Shiny app to support `MoveRakeR`. This is an
initial release and is subject to ongoing tests and updates. More
detailed instructions will be added in due course.

## Installation

Please install `RakeRvis` by using:

``` r
devtools::install_github('BritishTrustForOrnithology/RakeRvis', build_vignettes = TRUE)
```

## Dependencies

`RakeRvis` needs `MoveRakeR` installed to function correctly. In
addition the following packages are required:

shiny, shinydashboard, shinyBS, shinyjs, shinybusy, shinyWidgets,
htmltools, leaflet, leafgl, leaflet.extras2, sfheaders, sf, data.table,
DT, dplyr, markdown, and leaflegend

## Usage

The `RakeRvis` package should be used as a visualisation tool alongside
the `MoveRakeR` package to aid in visualisation of GPS tracking data of
individual animals. Data can be provided initially to the app, or
downloaded in the app from MoveBank or UvA-BiTS data repositories.

``` r
library(RakeRvis)

data <- read.csv(data.csv,sep = ",", header=TRUE)
data <- Track(data)

RakeRvis(data)
```

The `RakeRvis` package makes use of `leaflet`, `leafgl` and `mapdeck`.
Note, for smoother running with `mapdeck` we receommned installing the
very latest releases from `SymbolixAU` and associated packages rather
than the current Cran version, e.g. :

``` r
 
devtools::install_github('SymbolixAU/colourvalues')
devtools::install_github('SymbolixAU/spatialwidget')
devtools::install_github('SymbolixAU/mapdeck')
```

More detailed notes and help files within the app will follow soon…
