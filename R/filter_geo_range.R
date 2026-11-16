

#' Filter a dataframe with e/n or lat/long to an area of interest polygon (sf)
#'
#' @details
#'
#' An `ArrowObject` can be passed as `df`. In that case the
#' filter will be bounding box only and the returned object will also be an
#' `ArrowObject.`
#'
#' @param df Dataframe (or ArrowObject). Needs coordinate columns
#' @param use_aoi sf. Name of sf object defining the area of interest
#' @param x Character. Name of column with x coord
#' @param y Character. Name of column with y coord
#' @param crs_df Anything that will return a legitimate crs when passed to the
#' crs attribute of st_transform or st_as_sf
#' @param bbox_only Logical. Filter to just a bbox around `use_aoi`?
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
                             , bbox_only = FALSE
                             ) {

  collect_cols <- names(df)

  if(grepl("arrow", tolower(class(df)))) bbox_only = TRUE

  points <- df |>
    dplyr::distinct(dplyr::across(tidyselect::any_of(c(x,y))))

  crs_aoi <- sf::st_crs(use_aoi)

  if(is.numeric(crs_df)) crs_df <- paste0("epsg:", crs_df)

  if(! bbox_only) {

    result <- points |>
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

  if(bbox_only) {

    if(grepl("arrow", tolower(class(df)))) {

      use_bbox <- use_aoi |>
        sf::st_segmentize(dfMaxLength = 50000) |>
        sf::st_transform(crs = crs_df) |>
        sf::st_bbox()

      bbox_points <- points |>
        dplyr::filter(!!rlang::ensym(y) <= use_bbox["ymax"]
                      , !!rlang::ensym(y) >= use_bbox["ymin"]
                      , !!rlang::ensym(x) <= use_bbox["xmax"]
                      , !!rlang::ensym(x) >= use_bbox["xmin"]
                      )

      result <- df |>
        dplyr::inner_join(bbox_points
                          , copy = TRUE
                          )

    } else {

      use_bbox <- sf::st_bbox(use_aoi)

      result <- points |>
        dplyr::bind_cols(sf::sf_project(from = crs_df
                                        , to = crs_aoi
                                        , pts = points
                                        , keep = TRUE
                                        ) |>
                           tibble::as_tibble(.name_repair = "unique_quiet") |>
                           dplyr::rename(x_new = 1, y_new = 2)
                         ) |>
        dplyr::filter(y_new <= use_bbox["ymax"]
                      , y_new >= use_bbox["ymin"]
                      , x_new <= use_bbox["xmax"]
                      , x_new >= use_bbox["xmin"]
                      ) |>
        dplyr::inner_join(df) |>
        dplyr::select(tidyselect::any_of(collect_cols))

    }

  }

  return(result)

}

