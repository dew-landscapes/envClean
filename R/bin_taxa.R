

#' Add `taxa` column
#'
#' `taxa` column is the best match for the name in `taxa_col` based on the
#' GBIF Backbone Taxonomy.
#'
#' @param df Dataframe to clean, filter and tidy with respect to taxonomy.
#' @param taxa_col Character. Name of column with taxa.
#' @param taxonomy list with (at least) named elements `lutaxa` and `taxonomy`.
#' Usually resulting from call to `envClean::make_taxonomy()`.
#'
#' @return Dataframe with added column `taxa`
#' @export
#'
#' @examples
  filter_taxa <- function(df
                          , taxa_col = "original_name"
                          , taxonomy
                          ) {

    return_names <- names(df)

    if(taxa_col == "taxa") {

      return_names <- c("original_name", return_names)

    }

    df <- df %>%
      dplyr::mutate(original_name = !!rlang::ensym(taxa_col))

    return <- df %>%
      dplyr::distinct(original_name) %>%
      dplyr::left_join(taxonomy$lutaxa) %>%
      dplyr::left_join(taxonomy$taxonomy) %>%
      dplyr::left_join(df) %>%
      dplyr::select(taxa
                    , tidyselect::any_of(return_names)
                    )

    return(return)

  }

