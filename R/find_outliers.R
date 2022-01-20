
#' Find local outliers
#'
#' @param df Dataframe with `context` and all other columns defining the space
#' in which to look for outliers.
#' @param context  Character. Name of columns defining context.
#' @param do_out Character vector of methods to use.
#' @param lof_minPts [dbscan::lof()] `minPts` argument.
#' @param LOOP_k [DDoutlier::LOOP()] `k` argument.
#' @param LOOP_lambda [DDoutlier::LOOP()] `lambda` argument.
#' @param iqrMult Used in `quantile(x, probs = 0.75) + iqrMult * IQR(x)`.
#' [ggplot::geom_boxplot()] default value is `1.5`.
#'
#' @return
#' @export
#'
#' @examples
find_outliers <- function(df
                          , context
                          , do_out = c("lof", "LOOP")
                          , lof_minPts = 5
                          , LOOP_k = 5
                          , LOOP_lambda = 3 # default from LOOP
                          , iqrMult = 2
                          ) {

  df_use <- df %>%
    dplyr::group_by(across(any_of(context))) %>%
    dplyr::mutate(ID = dplyr::cur_group_id()) %>%
    dplyr::ungroup()

  res <- df_use %>%
    dplyr::select(c(ID, any_of(context)))

  if("lof" %in% do_out) {

    lof <- dbscan::lof(df_use %>%
                         dplyr::select(!c(ID, any_of(context)))
                       , minPts = lof_minPts
                       )

    res <- res %>%
      dplyr::mutate(out_lof = lof)

  }

  if("LOOP" %in% do_out) {

    LOOP <- DDoutlier::LOOP(df_use %>%
                              dplyr::select(!c(ID, any_of(context)))
                            , k = LOOP_k
                            , lambda = LOOP_lambda
                            )

    if(!all(is.na(LOOP))) {

      res <- res %>%
        dplyr::mutate(out_LOOP = LOOP)

    }

  }

  res %>%
    tidyr::pivot_longer(matches("out_")) %>%
    dplyr::group_by(name) %>%
    dplyr::mutate(thresh = quantile(value, probs = 0.75, na.rm = TRUE) + iqrMult * IQR(value)) %>%
    dplyr::mutate(outlier = value > thresh) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(across(any_of(context))) %>%
    dplyr::mutate(outlier = sum(outlier) > 1)

}
