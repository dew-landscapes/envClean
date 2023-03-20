

#' Filter data frame to specified spatial reliability
#'
#' @param df Dataframe.
#' @param dist_col Character. Name of the column containing the spatial
#' reliability.
#' @param dist Numeric. Filter spatial reliability greater than this value. In
#' the same units as `dist_col`.
#' @param context Character. column names defining the context.
#' @param over_ride Named list. List names must be the same as column names. Any
#' names in `over_ride` will be matched to column names in `df` and any values
#' in that list element will not be filtered (on spatial reliability).
#'
#' @return Dataframe with records of greater than `dist` filtered. Filtering is
#' done at `context` level.
#' @export
#'
#' @examples
  filter_geo_rel <- function(df
                              , dist_col = "rel_metres"
                              , dist = 50
                              , context
                              , over_ride = NULL
                              ){

    .df <- df

    if(isTRUE(!is.null(over_ride))) {

      for(i in seq_along(over_ride)) {

        col <- names(over_ride)[[i]]

        .df <- .df %>%
          dplyr::mutate(!!rlang::ensym(dist_col) := dplyr::if_else(!!rlang::ensym(col) %in% over_ride[[i]]
                                                , dist
                                                , !!rlang::ensym(dist_col)
                                                )
                        )

      }

    }

    vis_rel <- .df %>%
      dplyr::distinct(dplyr::across(tidyselect::any_of(context))
                      , !!rlang::ensym(dist_col)
                      ) %>%
      dplyr::filter(!is.na(!!rlang::ensym(dist_col))) %>%
      dplyr::filter(!!rlang::ensym(dist_col) <= dist) %>%
      dplyr::select(-!!rlang::ensym(dist_col)) %>%
      dplyr::distinct()

    df %>%
      dplyr::inner_join(vis_rel)

  }

