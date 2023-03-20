

#' Filter any context with less instances than a threshold value
#'
#' @param df Dataframe with column names defining context.
#' @param context Character. columns defining context within which to count
#' instances.
#' @param thresh Numeric. Threshold (inclusive of thresh) below which to filter.
#'
#' @return Filtered dataframe with same names as df
#' @export
#'
#' @examples
  filter_counts <- function(df, context, thresh = 1) {

    df %>%
      dplyr::add_count(dplyr::across(tidyselect::any_of(context))) %>%
      dplyr::filter(n > thresh) %>%
      dplyr::select(names(df))

  }

