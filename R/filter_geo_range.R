

#' Filter a dataframe with e/n or lat/long to an area of interest polygon (sf)
#'
#' @param df Dataframe. Needs coordinate columns
#' @param use_aoi sf. Name of sf object defining the area of interest
#' @param x Character. Name of column with x coord
#' @param y Character. Name of column with y coord
#' @param crs_df Anything that will return a legitimate crs when passed to the
#' crs attribute of st_transform or st_as_sf
#'
#' @return Dataframe filtered to area of interest
#' @export
#'
#' @examples
  filter_geo_range <- function(df
                               , use_aoi
                               , x = "long"
                               , y = "lat"
                               , crs_df = 4326
                               ) {

    df %>%
      dplyr::distinct(dplyr::across(tidyselect::any_of(c(x,y)))) %>%
      sf::st_as_sf(coords = c(x,y)
                   , crs = crs_df
                   , remove = FALSE
                   ) %>%
      sf::st_transform(crs = sf::st_crs(use_aoi)) %>%
      sf::st_filter(use_aoi) %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::inner_join(df) %>%
      tibble::as_tibble()

  }

