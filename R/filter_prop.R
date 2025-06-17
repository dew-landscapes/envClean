

#' Filter taxa recorded in less than x proportion of contexts
#'
#' @param df Dataframe with taxa and context
#' @param context Character. Column names that define context.
#' @param keep Character. taxa that should not be dropped. Can be used to
#' decrease x lower than `default_prop` to ensure `keep` taxa appear in
#' the filtered results. The lowest value of x required to keep any `keep` taxa
#' then overrides `default_prop` for the whole dataset, but only up to
#' `min_prop`.
#' @param default_prop Numeric (proportion). If `keep` is NULL (or if no `keep`
#' taxa occur within the `df`), filter any taxa recorded in less than
#' `default_prop` proportion of contexts.
#' @param min_prop Numeric (proportion). If `keep` is not null, do not lower x
#' below `min_prop`.
#'
#' @return df filtered of taxa that occur in less than x% of contexts, taking
#' into account `default_prop`, `keep` and/or `min_prop`.
#' @export
#'
#' @examples
filter_prop <- function(df
                        , context = "cell"
                        , keep = NULL
                        , default_prop = 0.05
                        , min_prop = 0.01
                        ) {

  thresh <- if(isTRUE(!is.null(keep))) {

    df_prep <- df |>
      dplyr::mutate(visits = dplyr::n_distinct(dplyr::across(tidyselect::any_of(context)))) %>%
      dplyr::select(visits, taxa) |>
      dplyr::filter(grepl(paste0(keep, collapse = "|"), taxa)) |>
      dplyr::count(taxa, visits, name = "records") |>
      dplyr::mutate(prop = records / visits) |>
      dplyr::filter(prop < default_prop)

    if(nrow(df_prep)) {

      low_prop <- df_prep |>
        dplyr::filter(prop > min_prop) |>
        dplyr::pull(prop)

      if(length(low_prop)) {

        low_prop <- if(low_prop > min_prop) low_prop else min_prop

      } else default_prop

    } else default_prop

  } else default_prop

  drop_taxa <- df |>
    dplyr::mutate(visits = dplyr::n_distinct(dplyr::across(tidyselect::any_of(context)))) |>
    dplyr::select(visits, taxa) |>
    dplyr::count(taxa, visits, name = "records") %>%
    dplyr::mutate(prop = records / visits) %>%
    dplyr::filter(prop < thresh) %>%
    dplyr::distinct(taxa)

  df %>%
    dplyr::anti_join(drop_taxa)

}

