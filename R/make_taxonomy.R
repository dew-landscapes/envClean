

#' Make taxonomy lookups
#'
#' @param df Dataframe with taxa column.
#' @param taxa_col Character. Name of column with taxa names
#' @param out_file Character. Path to save results to.
#' @param target_rank Character. Default is 'species'. At what level of the
#' taxonomic hierarchy are results desired.
#' @param do_common Logical. If true, an attempt will be made to find a common
#' name for each taxa at `target_rank`.
#' @param limit Logical. If true (default), the output taxonomy will be limited
#' to the input names in `taxa_col`. Otherwise, any taxa in `out_file` will be
#' returned.
#' @param ... Passed to `get_taxonomy()`
#'
#' @return named list with elements:
#'     \item{lutaxa}{Dataframe. For each `original_name` a taxa to use.}
#'     \item{taxonomy}{Dataframe. For each taxa a row of taxonomic hierarchy and
#'      matching gbif usageKeys to `target_rank` level}
#'     \item{common}{If `do_common` a dataframe with common name for each
#'     taxa in `taxonomy`.}
#'
#' @export
#'
#' @examples
#'
  make_taxonomy <- function(df
                            , taxa_col = "original_name"
                            , taxonomy_file
                            , out_file = here::here("out", "taxonomy.rds")
                            , target_rank = "species"
                            , do_common = FALSE
                            , limit = TRUE
                            , ...
                            ) {


    tax_res <- list()

    # raw -------

    raw <- get_taxonomy(df = df
                        , taxa_col = taxa_col
                        , out_file = taxonomy_file
                        , ...
                        ) %>%
      dplyr::filter(!is.na(canonicalName)
                    , stamp == max(stamp)
                    )

    # limit------

    if(limit) {

      raw <- raw %>%
        dplyr::inner_join(df %>%
                            dplyr::distinct(!!rlang::ensym(taxa_col))
                          , by = c("original_name" = taxa_col)
                          )

    }


    # lutaxa------

    return_taxonomy <- c("taxa"
                         , lurank$rank
                         , "best_key"
                         )

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
                    )


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
                    )

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
                      , dplyr::across(tidyselect::matches(paste0(lurank$rank, collapse = "|")))
                      )


    # common -------
    if(do_common) {

      if("common" %in% names(tax_res$taxonomy)){

        old_common <- tax_res$taxonomy$common %>%
          dplyr::filter(searched)

      }

      new_common <- tax_res$taxonomy %>%
        dplyr::distinct(taxa, best_key) %>%
        {if(exists("old_common")) (.) %>% dplyr::anti_join(old_common) else (.)} %>%
        dplyr::mutate(common = purrr::map_chr(best_key
                                              , get_gbif_common
                                              )
                      )

      tax_res$taxonomy <- purrr::reduce(mget(ls(pattern = "new_common|old_common"))
                                    , dplyr::bind_rows
                                    ) %>%
        dplyr::right_join(tax_res$taxonomy) %>%
        dplyr::distinct() %>%
        dplyr::mutate(searched = TRUE)

    }

    return(tax_res)

  }
