
#' Find local outliers
#'
#' Thin wrapper around `dbscan::lof()`
#'
#' @param df Dataframe with `context` and all other columns defining the space
#' in which to look for outliers (usually environmental variables such as
#' climate or satellite variables)
#' @param context  Character. Name of columns defining context.
#' @param iqrMult Used in `quantile(x, probs = 0.75) + iqrMult * IQR(x)` to set
#' the threshold for an outlier. e.g. `ggplot2::geom_boxplot()` default value is
#' `1.5`.
#' @param ... Passed to `dbscan::lof()`
#'
#' @return
#' @export
#'
#' @examples
flag_local_outlier <- function(df
                               , context
                               , iqrMult = 2
                               , ...
                               ) {

  lof <- dbscan::lof(df |>
                       dplyr::select(! tidyselect::any_of(context))
                     , ...
                     )

  res <- df |>
    dplyr::select(tidyselect::any_of(context)) |>
    dplyr::mutate(lof = lof
                  , thresh = quantile(lof, probs = 0.75, na.rm = TRUE) + iqrMult * IQR(lof)
                  , outlier_lof = lof > thresh
                  )

}
