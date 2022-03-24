
#' Create an 'area of interest'
#'
#' @param polygons sf.
#' @param filterpolys character. Used to filter `filterpolyscol`.
#' @param filterpolyscol character. Which column to filter on.
#' @param buffer numeric. Create a buffer around the area of interest of
#' `buffer` metres.
#' @param domask logical. If FALSE, just use extent of `buffer`.
#' @param usecrs numeric. [EPSG](https://epsg.io/) code giving coordinate
#' system to use in output sf.
#'
#' @return sf
#' @export
#'
#' @examples
make_aoi <- function(polygons
                     , filterpolys = FALSE
                     , filterpolyscol = NULL
                     , buffer
                     , domask = TRUE
                     , usecrs = 3577
                     ) {

  if(!isFALSE(filterpolys)) {

    keepRows <- polygons %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::pull(!!ensym(filterpolyscol)) %>%
      grep(paste0(filterpolys,collapse = "|"),.)

    polygons <- polygons %>%
      dplyr::slice(keepRows)

  }

  polygons <- if(domask) {

    polygons %>%
      dplyr::mutate(dissolve = 1) %>%
      dplyr::summarise(Include = n()) %>%
      sf::st_cast() %>%
      sf::st_buffer(buffer)

  } else {

    polygons %>%
      dplyr::mutate(dissolve = 1) %>%
      dplyr::summarise(Include = n()) %>%
      sf::st_cast() %>%
      sf::st_buffer(buffer) %>%
      sf::st_bbox() %>%
      sf::st_as_sfc() %>%
      sf::st_sf() %>%
      dplyr::mutate(Include = 1)

  }

  polygons <- polygons %>%
    sf::st_transform(crs = usecrs)

}
