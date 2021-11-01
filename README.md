
<!-- README.md is generated from README.Rmd. Please edit that file -->

# envClean

<!-- badges: start -->

<!-- badges: end -->

The goal of envClean is to help clean large, unstructured, biological
(or `env`ironmental) data sets.

It assumes the desired end result is a plausible list of taxa recorded
at space and time locations for use in further analysis. This is *not
the same* as an authoritative checklist of taxa for any space and time
locations.

While there are many implied and explicit decisions to make (e.g. there
may be a lot of work to set up for new data sets), there is no manual
input required once those decisions are made - these functions have the
potential to provide an automated workflow from combined data through to
analysis-ready data.

## Installation

`envClean` is not on [CRAN](https://CRAN.R-project.org).

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Acanthiza/envClean")
```

Load `envClean`

``` r
library("envClean")
```

## Filtering an ‘area of interest’

This example uses the `flor_all` data frame and the simple feature
`aoi`. Converting `flor_all` to `sf` allows plotting them together.

Load `flor_all`

``` r
flor_all <- tibble::as_tibble(flor_all)
```

Convert `flor_all` to `sf` and plot together with `aoi`.

``` r

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

<p class="caption">

Records from `flor_all` plotted over the area of interest `aoi`.

</p>

</div>

Filtering `flor_all` to `aoi` is done with `filter_aoi`.

``` r

  flor_aoi <- filter_aoi(flor_all
                         , use_aoi = aoi
                         , crs_aoi = sf::st_crs(aoi)
                         ) %>%
    envFunc::add_time_stamp()
#> Joining, by = c("lat", "long")

  flor_aoi
#> # A tibble: 3,485 x 13
#>      lat  long data_name site   date       original_name cover cover_code quad_x
#>    <dbl> <dbl> <fct>     <chr>  <date>     <chr>         <dbl> <chr>       <dbl>
#>  1 -34.5  140. BCM       -5177~ 2006-09-10 Olearia pime~    NA <NA>           30
#>  2 -34.5  140. BCM       -5177~ 2006-09-10 Eucalyptus o~    NA <NA>           30
#>  3 -34.5  140. BCM       -5177~ 2006-09-10 Eucalyptus g~    NA <NA>           30
#>  4 -34.5  140. BCM       -5177~ 2006-09-10 Maireana tri~    NA <NA>           30
#>  5 -34.5  140. BCM       -5177~ 2006-09-10 Maireana pen~    NA <NA>           30
#>  6 -34.5  140. BCM       -5177~ 2006-09-10 Santalum mur~    NA <NA>           30
#>  7 -34.5  140. BCM       -5177~ 2006-09-10 Rhagodia cra~    NA <NA>           30
#>  8 -34.5  140. BCM       -5177~ 2006-09-10 Eremophila g~    NA <NA>           30
#>  9 -34.5  140. BCM       -5177~ 2006-09-10 Geijera line~    NA <NA>           30
#> 10 -34.5  140. BCM       -5177~ 2006-09-10 Maireana eri~    NA <NA>           30
#> # ... with 3,475 more rows, and 4 more variables: quad_y <dbl>, rel_dist <dbl>,
#> #   month <dbl>, year <dbl>
```

Check that spatial filter worked.

``` r

  flor_aoi_sf <- flor_aoi %>%
    sf::st_as_sf(coords = c("long", "lat")
                 , crs = 4326
                 )

  tmap::tm_shape(aoi
           , bbox = sf::st_bbox(flor_all_sf)
           ) +
    tmap::tm_polygons() +
  tmap::tm_shape(flor_aoi_sf) +
    tmap::tm_dots()
```

<img src="man/figures/README-flor_aoi-1.png" width="100%" />

## What else is in `envClean`

The following functions and data sets are provided in `envClean`.

| object                 | class                       | description                                                                 |
| :--------------------- | :-------------------------- | :-------------------------------------------------------------------------- |
| add\_cover             | function                    | Generate best guess of cover for each taxa\*context                         |
| add\_lifeform          | function                    | Generate best guess of lifeform for each taxa\*context                      |
| aoi                    | sf and data.frame           | Simple feature to define a geographic area of interest.                     |
| filter\_aoi            | function                    | Filter a dataframe with e/n or lat/long to an area of interest polygon (sf) |
| filter\_counts         | function                    | Filter any context with less instances than a threshold value               |
| filter\_prop           | function                    | Filter taxa recorded at less than x percent of visits                       |
| filter\_spat\_rel      | function                    | Filter data frame to specified spatial reliability                          |
| filter\_taxa           | function                    | Clean/Tidy to one row per taxa\*Visit                                       |
| filter\_text\_col      | function                    | Filter a dataframe column on character string(s)                            |
| flor\_all              | tbl\_df, tbl and data.frame | Example of data combined from several data sources.                         |
| get\_gbif\_common      | function                    | Find common name from GBIF                                                  |
| get\_gbif\_tax         | function                    | Get taxonomy from href                                                      |
| lufilter               | tbl\_df, tbl and data.frame | Dataframe of filtering steps                                                |
| lurank                 | tbl\_df, tbl and data.frame | Dataframe of taxonomic ranks                                                |
| make\_cover            | function                    | Make a single (numeric, proportion) cover column from different sorts of    |
| make\_effort\_mod\_cat | function                    | Model the effect of categorical variables on taxa richness.                 |
| make\_effort\_mod\_pca | function                    | Model the effect of principal components axes on taxa richness.             |
| make\_env\_pca         | function                    | Principal components analysis and various outputs from environmental data   |
| make\_ind\_status      | function                    | Make indigenous status lookup                                               |
| make\_lifeform         | function                    | Get unique lifeform across species, perhaps including further context       |
| make\_taxa\_taxonomy   | function                    | Make taxonomy lookups                                                       |
