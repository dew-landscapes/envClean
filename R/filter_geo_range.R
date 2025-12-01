

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

  points <- df |>
    dplyr::distinct(dplyr::across(tidyselect::any_of(c(x,y))))

  crs_aoi <- sf::st_crs(use_aoi)

  if(is.numeric(crs_df)) crs_df <- paste0("epsg:", crs_df)

  points |>
    dplyr::bind_cols(sf::sf_project(from = crs_df
                                    , to = crs_aoi
                                    , pts = points
                                    , keep = TRUE
                                    ) |>
                       tibble::as_tibble(.name_repair = "unique_quiet") |>
                       dplyr::rename(x_new = 1, y_new = 2)
                     ) |>
    sf::st_as_sf(coords = c("x_new", "y_new")
                 , crs = crs_aoi
                 ) |>
    sf::st_join(use_aoi
                , left = FALSE
                ) |>
    sf::st_set_geometry(NULL) |>
    dplyr::inner_join(df)

}

