
#' Flag local outliers using `dbscan` package
#'
#'
#'
#' @param df Dataframe with `context` and all other columns defining the space
#' in which to look for outliers (usually environmental variables such as
#' climate or satellite variables)
#' @param context  Character. Name of columns defining context.
#' @param vars Character. Name of column(s) to investigate for outliers
#' @param min_points Numeric. Don't attempt reverse jackknife calculations
#' unless there are at least this number of data points.
#' @param geo_rel_col Character. Name of column containing geographic
#' reliability information. Set to `NULL` to ignore.
#' @param geo_rel_thresh Numeric. Threshold in `geo_rel_col` below which to
#' filter that row from analysis. Needed for, say, coarse spatial reliability
#' but satellite variables (e.g. no point checking if a point is an outlier
#' against satellite variables (with resolution of, say 30 m) if the geographic
#' reliability of that point is 10 km). Ignored if `geo_rel_col` is `NULL`.
#' @param ... Passed to `dbscan::lof()`. e.g. `minPts` argument
#'
#' @return tibble with `context` and two extra columns: `cluster` from
#' `dbscan::dbscan()` (with `0` indicating an outlier) and `lo` (local outlier:
#' `as.logical(cluster)`).
#' @export
#'
#' @examples
flag_local_outliers <- function(df
                                , context
                                , vars = context
                                , min_points = 30
                                , geo_rel_col = "rel_metres_adj"
                                , geo_rel_thresh = 100
                                , ...
                                ) {

  collect_vars <- unique(c(context, vars))

  if(!is.null(geo_rel_col)) {

    df_use <- df |>
      dplyr::filter(!!rlang::ensym(geo_rel_col) <= geo_rel_thresh)

  } else df_use <- df

  df_use <- df_use |>
    dplyr::distinct(dplyr::across(tidyselect::any_of(collect_vars))) |>
    janitor::remove_empty(which = "cols") |>
    na.omit()

  if(nrow(df_use) >= min_points &
     any(grepl(paste0(vars, collapse = "|"), names(df_use)))
     ) {

    min_pts <- 2 * ncol(df_use)

    knn_dist <- dbscan::kNNdist(df_use
                                , k = min_pts
                                )

    lo <- dbscan::dbscan(df_use
                         , minPts = min_pts
                         , eps = quantile(knn_dist, probs = 0.9)
                         )

    lo$outlier <- as.logical(! lo$cluster)

    res <- df_use |>
      dplyr::select(tidyselect::any_of(context)) |>
      dplyr::mutate(clust = lo$cluster
                    , lo = lo$outlier
                    )

    attr(res, "na.action") <- NULL

  } else res <- tibble::tibble()

  return(res)

}
