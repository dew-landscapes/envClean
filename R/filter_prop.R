

#' Filter taxa recorded in less than x percent of contexts
#'
#' @param df Dataframe with taxa and context
#' @param context Character. Column names that define context.
#' @param keep Character. taxa that should not be dropped. Can be used to set x
#' percent of sites to lower than `default_per` to ensure `keep` taxa appear in
#' the filtered results. The lowest value of x required to keep any `keep` taxa
#' then overrides `default_per` for the whole dataset.
#' @param default_per Numeric (percentage). If `keep` is NULL (or if no `keep`
#' taxa occur within the `df`), filter any taxa recorded in less than
#' `default_per`% of contexts.
#' @param min_per Numeric (percentage). If `keep` is not null, do not lower x
#' below `min_per`.
#'
#' @return df filtered of taxa that occur in less than x% of contexts, taking
#' into account `default_per`, `keep` and/or `min_per`.
#' @export
#'
#' @examples
  filter_prop <- function(df
                          , context = "cell"
                          , keep = NULL
                          , default_per = 5
                          , min_per = 1
                          ) {

    thresh <- if(isTRUE(!is.null(keep))) {

      df %>%
        dplyr::mutate(visits = dplyr::n_distinct(dplyr::across(tidyselect::any_of(context)))) %>%
        dplyr::filter(taxa %in% keep) %>%
        dplyr::count(taxa, visits, name = "records") %>%
        dplyr::mutate(per = round(100 * records / visits, 2)) %>%
        dplyr::pull(per) %>%
        min(c(., default_per)) %>%
        max(c(., min_per))

    } else default_per

    drop_taxa <- df %>%
      dplyr::mutate(n_visits = dplyr::n_distinct(dplyr::across(tidyselect::any_of(context)))) %>%
      dplyr::group_by(taxa, n_visits) %>%
      dplyr::summarise(n_records = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(per = 100 * n_records / n_visits) %>%
      dplyr::filter(per < thresh) %>%
      dplyr::distinct(taxa)

    df %>%
      dplyr::anti_join(drop_taxa)

  }

