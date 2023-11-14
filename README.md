---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# `envClean`: an R package to help clean large unstructured data set(s)

<!-- badges: start -->
<!-- badges: end -->

The goal of `envClean` is to help clean large, unstructured, biological (or `env`ironmental) data sets.

It assumes the desired end result is a plausible list of taxa recorded at space and time locations for use in further analysis. This is _not the same_ as an authoritative checklist of taxa for any space and time locations.

While there are many implied and explicit decisions to make (e.g. there may be a lot of work to set up for new data sets), there is no manual input required once those decisions are made - these functions have the potential to provide an automated workflow from combined data through to analysis-ready data. Some help with reporting on the cleaning process also included.

## Installation

`envClean` is not on [CRAN](https://CRAN.R-project.org).

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Acanthiza/envClean")
```

Load `envClean`


```r
library("envClean")
```

## Filtering an 'area of interest'

This example uses the `flor_all` data frame and the simple feature `aoi`. Converting `flor_all` to `sf` allows plotting them together.

Load `flor_all`


```r
flor_all <- tibble::as_tibble(flor_all)
```

Convert `flor_all` to `sf` and plot together with `aoi`.


```r

  flor_all_sf <- flor_all %>%
    sf::st_as_sf(coords = c("long", "lat")
                 , crs = 4326
                 )

  tmap::tm_shape(aoi
           , bbox = sf::st_bbox(flor_all_sf)
           ) +
    tmap::tm_polygons() +
  tmap::tm_shape(flor_all_sf) +
    tmap::tm_dots()
```

<div class="figure">
<img src="man/figures/README-aoi-1.png" alt="Records from `flor_all` plotted over the area of interest `aoi`." width="100%" />
<p class="caption">Records from `flor_all` plotted over the area of interest `aoi`.</p>
</div>

Filtering `flor_all` to `aoi` is done with `filter_geo_range`.


```r

  flor_aoi <- filter_geo_range(flor_all
                         , use_aoi = aoi
                         , crs_aoi = sf::st_crs(aoi)
                         ) %>%
    envFunc::add_time_stamp()
#> Error in filter_geo_range(flor_all, use_aoi = aoi, crs_aoi = sf::st_crs(aoi)): unused argument (crs_aoi = sf::st_crs(aoi))

  flor_aoi
#> Error in eval(expr, envir, enclos): object 'flor_aoi' not found
```

Check that spatial filter worked.


```r

  flor_aoi_sf <- flor_aoi %>%
    sf::st_as_sf(coords = c("long", "lat")
                 , crs = 4326
                 )
#> Error in eval(expr, envir, enclos): object 'flor_aoi' not found

  tmap::tm_shape(aoi
           , bbox = sf::st_bbox(flor_all_sf)
           ) +
    tmap::tm_polygons() +
  tmap::tm_shape(flor_aoi_sf) +
    tmap::tm_dots()
#> Error in eval(expr, envir, enclos): object 'flor_aoi_sf' not found
```

## What else is in `envClean`

The following functions and data sets are provided in `envClean`. See https://acanthiza.github.io/envClean/ for more examples.


```
#> Error in `dplyr::mutate()`:
#> i In argument: `class = purrr::map_chr(object, ~envFunc::vec_to_sentence(class(get(.))))`.
#> Caused by error in `purrr::map_chr()`:
#> i In index: 9.
#> Caused by error in `get()`:
#> ! lazy-load database 'C:/Users/sysnw/AppData/Local/R/win-library/4.3/envClean/R/envClean.rdb' is corrupt
#> Error in eval(expr, envir, enclos): object 'manuals' not found
```




