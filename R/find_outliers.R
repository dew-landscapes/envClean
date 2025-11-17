
#' Find local outliers
#'
#' You're probably better off using `flag_local_outliers()` instead
#'
#' @param df Dataframe with `context` and all other columns defining the space
#' in which to look for outliers.
#' @param context  Character. Name of columns defining context.
#' @param lof_minPts [dbscan::lof()] `minPts` argument.
#' @param iqrMult Used in `quantile(x, probs = 0.75) + iqrMult * IQR(x)`.
#' [ggplot2::geom_boxplot()] default value is `1.5`.
#'
#' @return
#' @export
#'
#' @examples
find_outliers <- function(df
                          , context
                          , lof_minPts = 5
                          , iqrMult = 2
                          ) {

  df_use <- df %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(context))) %>%
    dplyr::mutate(ID = dplyr::cur_group_id()) %>%
    dplyr::ungroup()

  res <- df_use %>%
    dplyr::select(c(ID, tidyselect::any_of(context)))

  lof <- dbscan::lof(df_use %>%
                       dplyr::select(!c(ID, tidyselect::any_of(context)))
                     , minPts = lof_minPts
                     )

  res %>%
    dplyr::mutate(out_lof = lof) |>
    tidyr::pivot_longer(tidyselect::matches("out_")) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(thresh = quantile(value, probs = 0.75, na.rm = TRUE) + iqrMult * IQR(value)) %>%
    dplyr::mutate(outlier = value > thresh) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(context))) %>%
    dplyr::mutate(outlier = sum(outlier) > 1)

}
