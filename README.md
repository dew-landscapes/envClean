---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# `envClean`: an R package to help clean large unstructured data set(s)

<!-- badges: start -->
<!-- badges: end -->

The goal of `envClean` is to help clean large, unstructured, biological (or `env`ironmental) data sets.

It assumes the desired end result is a plausible list of taxa recorded at space and time locations for use in further analysis. This is _not the same_ as an authoritative checklist of taxa for any space and time locations.

While there are many implied and explicit decisions to make (e.g. there may be a lot of work to set up for new data sets), there is no manual input required once those decisions are made - these functions have the potential to provide an automated workflow from combined data through to analysis-ready data.

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

<img src="man/figures/README-aoi-1.png" title="Records from `flor_all` plotted over the area of interest `aoi`." alt="Records from `flor_all` plotted over the area of interest `aoi`." width="100%" />

Filtering `flor_all` to `aoi` is done with `filter_aoi`.


```r

  flor_aoi <- filter_aoi(flor_all
                         , use_aoi = aoi
                         , crs_aoi = sf::st_crs(aoi)
                         ) %>%
    envFunc::add_time_stamp()
#> Error in filter_aoi(flor_all, use_aoi = aoi, crs_aoi = sf::st_crs(aoi)): could not find function "filter_aoi"

  flor_aoi
#> Error in eval(expr, envir, enclos): object 'flor_aoi' not found
```

Check that spatial filter worked.


```r

  flor_aoi_sf <- flor_aoi %>%
    sf::st_as_sf(coords = c("long", "lat")
                 , crs = 4326
                 )
#> Error in sf::st_as_sf(., coords = c("long", "lat"), crs = 4326): object 'flor_aoi' not found

  tmap::tm_shape(aoi
           , bbox = sf::st_bbox(flor_all_sf)
           ) +
    tmap::tm_polygons() +
  tmap::tm_shape(flor_aoi_sf) +
    tmap::tm_dots()
#> Error in as.list.environment(environment()): object 'flor_aoi_sf' not found
```

## What else is in `envClean`

The following functions and data sets are provided in `envClean`. See https://acanthiza.github.io/envClean/ for more examples.


|object                         |class                      |description                                                                                                        |
|:------------------------------|:--------------------------|:------------------------------------------------------------------------------------------------------------------|
|`envFunc::add_cover`           |function                   |Generate best guess of cover for each taxa*context                                                                 |
|`envFunc::add_lifeform`        |function                   |Generate best guess of lifeform for each taxa*context                                                              |
|`envFunc::aoi`                 |sf and data.frame          |Simple feature to define a geographic area of interest.                                                            |
|`envFunc::cleaning_text`       |function                   |Write a sentence describing change in taxa, records, visits and sites between                                      |
|`envFunc::filter_counts`       |function                   |Filter any context with less instances than a threshold value                                                      |
|`envFunc::filter_geo_range`    |function                   |Filter a dataframe with e/n or lat/long to an area of interest polygon (sf)                                        |
|`envFunc::filter_geo_rel`      |function                   |Filter data frame to specified spatial reliability                                                                 |
|`envFunc::filter_prop`         |function                   |Filter taxa recorded at less than x percent of visits                                                              |
|`envFunc::filter_taxa`         |function                   |Clean/Tidy to one row per taxa*Visit                                                                               |
|`envFunc::filter_text_col`     |function                   |Filter a dataframe column on character string(s)                                                                   |
|`envFunc::find_outliers`       |function                   |Find local outliers                                                                                                |
|`envFunc::find_taxa`           |function                   |Find how taxa changed through the cleaning/filtering/tidying process                                               |
|`envFunc::flor_all`            |tbl_df, tbl and data.frame |Example of data combined from several data sources.                                                                |
|`envFunc::get_gbif_common`     |function                   |Find common name from GBIF                                                                                         |
|`envFunc::get_gbif_tax`        |function                   |Get taxonomy from href{https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c}{GBIF Backbone Taxonomy}. |
|`envFunc::luclean`             |tbl_df, tbl and data.frame |Dataframe of cleaning steps                                                                                        |
|`envFunc::lurank`              |tbl_df, tbl and data.frame |Dataframe of taxonomic ranks                                                                                       |
|`envFunc::make_aoi`            |function                   |Create an 'area of interest'                                                                                       |
|`envFunc::make_cover`          |function                   |Make a single (numeric, proportion) cover column from different sorts of                                           |
|`envFunc::make_effort_mod`     |function                   |Distribution of credible values for taxa richness.                                                                 |
|`envFunc::make_effort_mod_pca` |function                   |Model the effect of principal components axes on taxa richness.                                                    |
|`envFunc::make_env_pca`        |function                   |Principal components analysis and various outputs from environmental data                                          |
|`envFunc::make_ind_status`     |function                   |Make indigenous status lookup                                                                                      |
|`envFunc::make_lifeform`       |function                   |Get unique lifeform across taxa, perhaps including further context                                                 |
|`envFunc::make_taxa_taxonomy`  |function                   |Make taxonomy lookups                                                                                              |
|`envFunc::rec_vis_sit_tax`     |function                   |How many records, visits, sites and taxa in a dataframe                                                            |
|`envFunc::taxonomy_fixes`      |tbl_df, tbl and data.frame |Manual taxonomic fixes                                                                                             |




