

#' Add a spatial reliability column, binned to contexts
#'
#'
#'
#' @param df Dataframe.
#' @param dist_col Character. Name of the column containing the spatial
#' reliability.
#' @param dist_min Numeric. In the same units as `dist_col`. The target spatial
#' reliability that will be filtered on later in the workflow.
#' @param dist_max Numeric. In the same units as `dist_col`. In some cases,
#' there is good reason to believe that `dist_col` is an under estimate of
#' spatial reliability. These cases are identified by `over_ride_metres` where
#' those instances have `dist_col <= dist_max `. In those cases, `dist_col_adj`
#' will contain `dist_min` rather than `dist_col`. Only needed if
#' `over_ride_metres` is used
#' @param context Character. Column names defining the context.
#' @param over_ride_na Named list. List names must be the same as column names.
#' Any names in `over_ride_na` will be matched to column names in `df` and any
#' values in that list element will be given the value `dist_min`. This is mainly
#' used to prevent filtering data sources that do not have a concept equivalent
#' to `rel_metres`.
#' @param over_ride_metres Named list. List names must be the same as column
#' names. Any names in `over_ride_metres` will be matched to column names in `df`
#' and any values in that list element will be given the value `dist_min`.
#'
#' @return tibble with additional column `rel_metres_adj` containing the
#' minimum `rel_metres` available within that context, potentially taking into
#' account any over rides. Unlike `reduce_geo_rel()` (which only returns a
#' single row per context), with `bin_geo_rel()` the original `df` is only
#' altered by the additional column `rel_metres_adj`.
#'
#' @export
#'
#' @examples
  bin_geo_rel <- function(df
                             , dist_col = "rel_metres"
                             , dist_min = 100
                             , dist_max = 250
                             , context
                             , over_ride_na = NULL
                             , over_ride_metres = NULL
                             ){

    res <- df

    adj_col <- paste0(dist_col, "_adj")

    res[adj_col] <- res$rel_metres

    context <- unique(c(context, extra_cols))

    if(!is.null(over_ride_na)) {

      for(i in seq_along(over_ride_na)) {

        col <- names(over_ride_na)[[i]]

        res[which(is.na(res[adj_col]) & res[[col]] %in% over_ride_na[[i]]), adj_col] <- dist_min

      }

    }

    if(!is.null(over_ride_metres)) {

      for(i in seq_along(over_ride_metres)) {

        col <- names(over_ride_metres)[[i]]

        ids <- which(res[adj_col] > dist_min & res[adj_col] <= dist_max & res[[col]] %in% over_ride_metres[[i]])

        res[ids, adj_col] <- dist_min

      }

    }

    res[[adj_col]][is.na(res[[adj_col]])] <- 999999999

    res <- res %>%
      dplyr::group_by(dplyr::across(tidyselect::any_of(context))) %>%
      dplyr::mutate(!!rlang::ensym(adj_col) := min(!!rlang::ensym(adj_col), na.rm = TRUE)) %>%
      dplyr::ungroup()

    res[[adj_col]][res[[adj_col]] == 999999999] <- NA

    return(res)

  }

