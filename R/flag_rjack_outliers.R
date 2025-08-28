#' Flag reverse jackknife outliers
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
#' @param prop_thresh Numeric. What proportion of variables (i.e.
#' proportion of `vars`) need to be reverse jackknife outliers for a point to be
#' flagged as an outlier?
#'
#' @returns tibble
#' @export
#'
#' @examples
flag_rjack_outliers <- function(df
                                , context
                                , vars = context
                                , min_points = 30
                                , geo_rel_col = "rel_metres_adj"
                                , geo_rel_thresh = 100
                                , prop_thresh = 1 / 3
                                ) {

  collect_vars <- unique(c(context, vars))

  if(!is.null(geo_rel_col)) {

    df_use <- df |>
      dplyr::filter(!!rlang::ensym(geo_rel_col) <= geo_rel_thresh)

  } else df_use <- df

  df_use <- df_use |>
    dplyr::distinct(dplyr::across(tidyselect::any_of(collect_vars))) |>
    janitor::remove_empty(which = "cols") |>
    janitor::remove_constant() |>
    na.omit()

  vars <- vars[vars %in% names(df_use)]

  if(all(nrow(df_use) > min_points
         , any(grepl(paste0(vars, collapse = "|"), names(df_use)))
         , length(vars)
         )
     ) {

    n <- nrow(df_use)

    rev_jack <- matrix(ncol = length(vars), nrow = n)

    for(i in 1:length(vars)) {

      xc <- df_use |>
        dplyr::pull(vars[i])

      fe2 <- rjack(d = xc) # reverse jackknife

      res <- rep(0, n)
      res[fe2] <- 1

      rev_jack[, i] <- res

    }

    colnames(rev_jack) <- paste0("jack___", vars)

    jack <- df_use |>
      dplyr::select(tidyselect::any_of(context)) |>
      dplyr::bind_cols(tibble::as_tibble(rev_jack) |>
                         dplyr::select(tidyselect::matches(paste0(vars, collapse = "|")))
                       )

    if(any(grepl(paste0(vars, collapse = "|"), names(jack)))) {

      res <- jack |>
        tidyr::pivot_longer(tidyselect::any_of(paste0("jack___", names(df_use)))) |>
        dplyr::group_by(dplyr::across(tidyselect::any_of(context))) |>
        dplyr::summarise(vars_outliers = sum(value, na.rm = TRUE)
                         , vars_n = dplyr::n()
                         ) |>
        dplyr::ungroup() |>
        dplyr::mutate(outlier_prop = vars_outliers / vars_n
                      , outlier = outlier_prop >= prop_thresh
                      )

    }

  } else res <- tibble::tibble()

  return(res)

}
