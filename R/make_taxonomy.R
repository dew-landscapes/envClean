

#' Make taxonomy lookups
#'
#' @param df Dataframe with taxa column.
#' @param taxa_col Character. Name of column with taxa names
#' @param out_file Character. Path to save results to.
#' @param target_rank Character. Default is 'species'. At what level of the
#' taxonomic hierarchy are results desired.
#' @param limit Logical. If true (default), the output taxonomy will be limited
#' to the input names in `taxa_col`. Otherwise, any taxa in `out_file` will be
#' returned.
#' @param ... Passed to `get_taxonomy()`
#'
#' @return named list with elements:
#'     \item{lutaxa}{Dataframe. For each `original_name` a taxa to use.}
#'     \item{taxonomy}{Dataframe. For each taxa a row of taxonomic hierarchy and
#'      matching gbif usageKeys to `target_rank` level}
#'
#' @export
#'
#' @examples
#'
  make_taxonomy <- function(df
                            , taxa_col = "original_name"
                            , taxonomy_file
                            , target_rank = "species"
                            , limit = TRUE
                            , ...
                            ) {

    tax_res <- list()

    # raw -------

    raw <- get_taxonomy(df = df
                        , taxa_col = taxa_col
                        , taxonomy_file = taxonomy_file
                        , ...
                        ) %>%
      dplyr::filter(!is.na(canonicalName))


    # limit------

    if(limit) {

      raw <- raw %>%
        dplyr::inner_join(df %>%
                            dplyr::distinct(!!rlang::ensym(taxa_col))
                          , by = c("original_name" = taxa_col)
                          )

    }


    # fix keys ------
    # levels of hierarchy below species do not have keys in the gbif output
    # chars and keys is designed to fix that for the next steps

    missing_ranks <- setdiff(lurank$rank, names(raw))

    chars <- raw %>%
      dplyr::filter(!is.na(canonicalName)) %>%
      dplyr::left_join(
        lapply(missing_ranks
               , function(x) raw %>%
                 dplyr::mutate(!!rlang::ensym(x) := dplyr::case_when(rank == x ~ canonicalName
                                                                     , TRUE ~ NA_character_
                                                                     )
                               ) %>%
                 dplyr::select(kingdom, original_name, searched_name, !!rlang::ensym(x))
               ) %>%
          purrr::reduce(dplyr::left_join)
        ) %>%
      dplyr::select(original_name
                    , tidyselect::any_of(lurank$rank)
                    ) %>%
      dplyr::distinct()


    keys <- raw %>%
      dplyr::filter(!is.na(canonicalName)) %>%
      dplyr::left_join(
        lapply(missing_ranks
               , function(x) raw %>%
                 dplyr::filter(!is.na(canonicalName)) %>%
                 dplyr::mutate(!!rlang::ensym(x) := dplyr::case_when(rank == x ~ usageKey
                                                                     , TRUE ~ NA_integer_
                                                                     )
                               ) %>%
                 dplyr::select(kingdom, original_name, !!rlang::ensym(x)) %>%
                 dplyr::filter(!is.na(!!rlang::ensym(x))) %>%
                 dplyr::rename(!!paste0(x, "Key") := 3)
               ) %>%
          purrr::reduce(dplyr::left_join)
        ) %>%
      dplyr::select(original_name
                    , dplyr::where(is.numeric)
                    ) %>%
      dplyr::distinct()


    # lutaxa --------

    tax_res$lutaxa <- chars %>%
      dplyr::left_join(keys) %>%
      tidyr::pivot_longer(tidyselect::any_of(lurank$rank)
                          , names_to = "rank"
                          , values_to = "taxa"
                          ) %>%
      dplyr::filter(!is.na(taxa)) %>%
      dplyr::left_join(lurank) %>%
      dplyr::mutate(rank = factor(rank
                                  , levels = levels(lurank$rank)
                                  , ordered = TRUE
                                  )
                    ) %>%
      dplyr::group_by(original_name) %>%
      dplyr::filter(rank >= target_rank) %>%
      dplyr::filter(sort == max(sort)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(rank_key = paste0(rank, "Key")) %>%
      tidyr::pivot_longer(dplyr::where(is.numeric)
                          , values_to = "best_key"
                          ) %>%
      dplyr::filter(rank_key == name) %>%
      dplyr::select(!!rlang::ensym(taxa_col) := original_name
                    , taxa
                    , rank
                    , best_key
                    ) %>%
      dplyr::distinct()


    # taxonomy ------

    tax_res$taxonomy <- tax_res$lutaxa %>%
      dplyr::left_join(raw %>%
                         dplyr::select(original_name
                                       , tidyselect::matches(paste0(lurank$rank
                                                                    , collapse = "|"
                                                                    )
                                                             )
                                       )
                       ) %>%
      dplyr::distinct(taxa
                      , best_key
                      , dplyr::across(tidyselect::matches(paste0(lurank$rank, collapse = "|")))
                      )

    return(tax_res)

  }
