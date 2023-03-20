
#' Get unique lifeform across taxa, perhaps including further context
#'
#' There are two tasks here:
#'
#' * find the most frequent lifeform assigned to a taxa (perhaps including
#' other context)
#' * ensure there are no duplicates across the relevant context
#'
#' @param df Dataframe with taxa column.
#' @param taxa_col Character. Name of taxa column.
#' @param lf_col Character. Name of lifeform (id) column.
#' @param context Charcter or NULL. Set of columns that define a context within
#' which to generate lifeform.
#' @param lulife Dataframe lookup for lifeform.
#'
#' @return Dataframe with columns taxa_col, visit col(s), lifeform
#' @export
#'
#' @examples
  make_lifeform <- function(df
                              , taxa_col = "taxa"
                              , lf_col = "lifeform"
                              , context = NULL
                              , lulife
                              ) {

    df %>%
      dplyr::filter(!is.na(!!rlang::ensym(lf_col))) %>%
      dplyr::count(dplyr::across(!!rlang::ensym(taxa_col)),dplyr::across(!!rlang::ensym(lf_col)),dplyr::across(tidyselect::any_of(context)), name = "lifeform_records") %>%
      dplyr::group_by(dplyr::across(!!rlang::ensym(taxa_col)),dplyr::across(tidyselect::any_of(context))) %>%
      dplyr::mutate(taxa_records = sum(lifeform_records,na.rm = TRUE)
                    , per = 100*lifeform_records/taxa_records
                    ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(per > 5) %>%
      dplyr::left_join(lulife) %>%
      dplyr::mutate(ht_test = dplyr::if_else(lifeform == "J",ht + 0.01, ht)) %>%
      dplyr::group_by(dplyr::across(!!rlang::ensym(taxa_col)),dplyr::across(tidyselect::any_of(context))) %>%
      dplyr::slice(which(ht_test == max(ht_test, na.rm=TRUE))) %>%
      dplyr::slice(which(lifeform_records == max(lifeform_records, na.rm=TRUE))) %>%
      dplyr::slice(which(sort == max(sort, na.rm=TRUE))) %>%
      dplyr::ungroup() %>%
      dplyr::select(tidyselect::any_of(context),!!rlang::ensym(taxa_col),lifeform) %>%
      dplyr::filter(!is.na(!!rlang::ensym(taxa_col))) %>%
      dplyr::distinct()

  }

