
#' Make indigenous status lookup
#'
#' @param df Dataframe with taxa column and column indicating indigenous
#' status.
#' @param taxa_col Character. Name of column with taxa.
#' @param ind_col Character. Name of column with indigenous status (assumed to
#' be, largely, `Y` or `N`).
#'
#' @return Dataframe with unique taxa and their indigenous status.
#' @export
#'
#' @examples
  make_ind_status <- function(df, taxa_col = "taxa", ind_col = "ind") {

    df %>%
      dplyr::count(dplyr::across(!!rlang::ensym(taxa_col)),dplyr::across(!!rlang::ensym(ind_col))) %>%
      dplyr::filter(!!rlang::ensym(ind_col) %in% c("Y", "N")) %>%
      dplyr::group_by(!!rlang::ensym(taxa_col)) %>%
      dplyr::filter(n == max(n, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(!!rlang::ensym(taxa_col),!!rlang::ensym(ind_col)) %>%
      dplyr::right_join(df %>%
                          dplyr::distinct(!!rlang::ensym(taxa_col))
                        ) %>%
      dplyr::mutate(!!rlang::ensym(ind_col) := dplyr::if_else(grepl("\\?",!!rlang::ensym(ind_col)),"U",!!rlang::ensym(ind_col))
                    , !!rlang::ensym(ind_col) := dplyr::if_else(is.na(!!rlang::ensym(ind_col)),"U",!!rlang::ensym(ind_col))
                    )

  }

