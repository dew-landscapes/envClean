

#' Add `taxa` column
#'
#' `taxa` column is the best match for the name in `taxa_col` based on the
#' GBIF Backbone Taxonomy.
#'
#' @param df Dataframe to attribute with best result from GBIF Backbone Taxonomy
#' @param taxa_col Character. Name of column in `df` with taxa.
#' @param taxonomy list with (at least) named elements `lutaxa` and `taxonomy`.
#' Usually resulting from call to `envClean::make_taxonomy()`.
#' @param include_level Character (or `NULL`). Name(s) of any extra columns in
#' taxa$taxonomy to return. e.g. `include_levels = "kingdom"` will return the
#' kingdom for each taxa.
#'
#' @return Dataframe with added column `taxa`, and possibly `include_level`s
#' @export
#'
#' @examples
  bin_taxa <- function(df
                       , taxa_col = "original_name"
                       , taxonomy
                       , include_levels = NULL
                       ) {

    return_names <- names(df)

    if(taxa_col == "taxa") {

      return_names <- c("original_name", return_names)

    }

    if(!is.null(include_levels)) {

      return_names <- c(include_levels, return_names)

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

