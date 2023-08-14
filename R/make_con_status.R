
#' Make conservation status from existing status codes
#'
#' Useful for assigning a 'threatened' status to a binomial where different
#' statuses apply at trinomial level. The resulting 'status' is simply the
#' concatenated values from lower levels of the taxonomic hierarchy.
#'
#' @param df Dataframe with `taxa_col` and `status_col`.
#' @param taxa_col Character. Name of column with taxa.
#' @param status_col Character. Name of column with conservation status.
#' @param statuses Character. Possible status codes to concatenate.
#'
#' @return Dataframe with unique taxa and their conservation status.
#' @export
#'
#' @examples
  make_con_status <- function(df
                              , taxa_col = "taxa"
                              , status_col = "epbc"
                              , statuses = c("ssp"
                                             , "VU"
                                             , "EN"
                                             , "sp"
                                             , "CR"
                                             , "EX"
                                             , "CD"
                                             , "V"
                                             , "E"
                                             , "SP"
                                             )
                              ) {

    df %>%
      dplyr::distinct(dplyr::across(!!rlang::ensym(taxa_col))
                      , dplyr::across(!!rlang::ensym(status_col))
                      ) %>%
      dplyr::filter(!!rlang::ensym(status_col) %in% statuses) %>%
      dplyr::group_by(!!rlang::ensym(taxa_col)) %>%
      dplyr::summarise(!!rlang::ensym(status_col) := vec_to_sentence(!!rlang::ensym(status_col))) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(df %>%
                          dplyr::select(-rlang::ensym(status_col))
                        )

  }

