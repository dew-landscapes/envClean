
#' Filter data frame to specified spatial reliability
#'
#' @param df Dataframe.
#' @param distCol Character. Name of the column containing the spatial reliability.
#' @param dist Numeric. In units of the maxDist column.
#' @param visit Character. Column names defining a 'visit'.
#' @param dfRel Dataframe. Lookup from reliability id to reliability distance.
#'
#' @return Dataframe with records of greater than dist filtered. Filtering is done
#' at 'visit' level.
#' @export
#'
#' @examples
  filter_spat_rel <- function(df
                              , distCol = "maxDist"
                              , dist = relDist
                              , visit = c("lat","long","dataName","date")
                              , dfRel = luRel
                              ){

    visRel <- df %>%
      dplyr::left_join(dfRel) %>%
      dplyr::mutate(distCol := if_else(is.na(!!ensym(distCol)),relDist,!!ensym(distCol))) %>%
      dplyr::distinct(across(any_of(visit)),!!ensym(distCol)) %>%
      dplyr::filter(!!ensym(distCol) <= dist) %>%
      dplyr::select(-!!ensym(distCol))

    df %>%
      dplyr::inner_join(visRel) %>%
      as_tibble()

  }


#' Filter a dataframe with e/n or lat/long to an area of interest polygon (sf)
#'
#' @param df Dataframe. Needs coordinate columns
#' @param AOI sf. Name of sf object defining the area of interest
#' @param x Character. Name of column with x coord
#' @param y Character. Name of column with y coord
#' @param crsDf Anything that will return a legitimate crs when passed to the
#' crs attribute of st_transform or st_as_sf
#' @param crsAOI as for crsDf
#'
#' @return Dataframe filtered to area of interest
#' @export
#'
#' @examples
  filter_aoi <- function(df
                         , AOI = aoi
                         , x = "long"
                         , y = "lat"
                         , crsDf = 4326
                         , crsAOI = useEPSG
                         ) {

    df %>%
      dplyr::distinct(!!ensym(x),!!ensym(y)) %>%
      sf::st_as_sf(coords = c(x,y)
                   , crs = crsDf
                   , remove = FALSE
                   ) %>%
      sf::st_transform(crs = crsAOI) %>%
      sf::st_filter(AOI) %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::inner_join(df) %>%
      as_tibble()

  }
