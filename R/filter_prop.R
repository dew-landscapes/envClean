

#' Filter taxa recorded at less than x percent of visits
#'
#' @param df Dataframe with taxa and context
#' @param context Character. Column names that define context, usually a 'visit'
#' to a 'cell'.
#' @param min_sites Absolute minimum sites at which a taxa should be recorded.
#' @param keep Character. taxa that should not be dropped. Used to set x
#' percent of sites.
#' @param default_per If keeptaxa is NULL, what is the minimum percent of sites
#' at which a taxa should be recorded.
#'
#' @return df filtered to exclude taxa recorded at less than x percent of
#' visits.
#' @export
#'
#' @examples
  filter_prop <- function(df
                          , context = "cell"
                          , min_sites = 15
                          , keep = NULL
                          , default_per = 1
                          ) {

    thresh <- if(isTRUE(!is.null(keep))) {

      dont_drop_df <- df %>%
        dplyr::mutate(visits = dplyr::n_distinct(dplyr::across(tidyselect::any_of(context)))) %>%
        dplyr::filter(taxa %in% keep) %>%
        dplyr::count(taxa, visits, name = "records") %>%
        dplyr::mutate(per = round(100 * records / visits, 2)) %>%
        dplyr::filter(records > min_sites / 2) %>%
        dplyr::pull(per) %>%
        min(c(., default_per))

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

