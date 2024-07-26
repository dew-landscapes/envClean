

#' Make taxonomy lookups
#'
#' @param df Dataframe with `taxa_col`.
#' @param taxa_col Character. Name of column in `df` with taxa names
#' @param taxonomy_file Character. Path to results from
#' `envClean::get_taxonomy()`
#' @param target_rank Character. Default is 'species'. At what level of the
#' taxonomic hierarchy are results desired. This is the most detailed taxonomy
#' returned. i.e. if genus is the `target_rank`, no taxa below genus are
#' returned. See `envClean::lurank` `rank` column.
#' @param limit Logical. If true (default), the output taxonomy will be limited
#' to the input names in `df`. Otherwise, all taxa found in `taxonomy_file` will
#' be returned.
#' @param fixes Data frame with columns `resolved_to` and `prefer`. Any `taxa`
#' result in `lutaxa` that matches a name in `resolved_to` will be changed to
#' `prefer`. Mainly used where legitimate names are used in areas where they do
#' not exist. e.g. Eastern osprey _Pandion cristatus_ does not occur in South
#' Australia but records of this species in South Australia are assumed to be
#' legitimate Osprey (_Pandion haliaetus_) records.
#' @param overrides Data frame with columns `original` and `prefer`. Any
#' `original_name` result in `lutaxa` that matches a name in `original` will be
#' have its corresponding `taxa` changed to `prefer`. Useful where GBIF Backbone
#' Taxonomy provides a spurious result. e.g. The GBIF Backbone Taxonomy changes
#' _Thinornis rubricollis_ to _Phalaropus lobatus_ rather than the preferred
#' _Charadrius cucullatus_.
#'
#' @param ... Passed to `envClean::get_taxonomy()`
#'
#' @return named list with elements:
#'     \item{lutaxa}{Dataframe. For each unique name in `taxa_col`, the best
#'     `taxa` to use (taking into account `target_rank`)}
#'     \item{taxonomy}{Dataframe. For each `taxa` in `lutaxa` a row of taxonomic
#'     hierarchy and matching gbif usageKeys}
#'
#' @export
#'
#' @examples
#'
  make_gbif_taxonomy <- function(df
                            , taxa_col = "original_name"
                            , taxonomy_file
                            , target_rank = "species"
                            , limit = TRUE
                            , fixes = NULL
                            , overrides = NULL
                            , ...
                            ) {

    tax_res <- list()

    # raw -------

    use_df <- df %>%
      dplyr::distinct(!!rlang::ensym(taxa_col)) %>%
      dplyr::rename(original_name = !!rlang::ensym(taxa_col)) %>%
      dplyr::bind_rows(tibble::tibble(original_name := c(fixes$prefer
                                                         , fixes$resolved_to
                                                         , overrides$prefer
                                                         )
                                      )
                       ) %>%
      dplyr::distinct()

    raw <- get_taxonomy(df = use_df
                        , taxa_col = taxa_col
                        , taxonomy_file = taxonomy_file
                        , ...
                        ) %>%
      dplyr::filter(!is.na(canonicalName))


    # limit------

    if(limit) {

      raw <- raw %>%
        dplyr::inner_join(use_df %>%
                            dplyr::distinct(original_name)
                          , by = c("original_name" = taxa_col)
                          )

    }


    # best -------

    best <- raw %>%
      dplyr::select(original_name
                    , original_rank = rank
                    , status
                    , kingdom
                    , tidyselect::contains("Key")
                    ) %>%
      {if(target_rank %in% c("subspecies", "form", "variety")) {

        # this sometimes puts, say, subspeciesKey in where it is actually, say, a speciesKey. Fixed below
        (.) %>%
          dplyr::mutate("{target_rank}Key" := dplyr::if_else(status != "ACCEPTED"
                                                             , acceptedUsageKey
                                                             , usageKey
                                                             )
                        )

        } else {

          (.)

        }
      } %>%
      tidyr::pivot_longer(tidyselect::matches(paste0(envClean::lurank$rank, collapse = "Key|"))
                          , names_to = "use_rank"
                          , values_to = "best_key"
                          ) %>%
      dplyr::filter(!is.na(best_key)) %>%
      dplyr::mutate(use_rank = gsub("Key", "", use_rank)
                    , use_rank = factor(use_rank, levels = levels(envClean::lurank$rank)
                                    , ordered = TRUE
                                    )
                    ) %>%
      # fix for any errors introduced by if statement above
      dplyr::group_by(original_name, best_key) %>%
      dplyr::filter(use_rank == max(use_rank)) %>%
      dplyr::ungroup() %>%
      # filter anything below target rank
      dplyr::filter(use_rank >= target_rank) %>%
      # for each original name find the best available taxa
      dplyr::group_by(original_name) %>%
      dplyr::filter(use_rank == min(use_rank)) %>%
      dplyr::ungroup()


    # accepted -----

    accepted_tax_file <- gsub("\\.", "_accepted.", taxonomy_file)

    accepted <- rio::import(accepted_tax_file
                            , setclass = "tibble"
                            )

    # lutaxa-------

    tax_res$lutaxa <- best %>%
      dplyr::inner_join(accepted %>%
                         dplyr::select(taxa = canonicalName, best_key = usageKey)
                       )


    # taxonomy-------

    tax_res$taxonomy <- tax_res$lutaxa %>%
      dplyr::distinct(best_key) %>%
      dplyr::left_join(accepted %>%
                         dplyr::select(taxa = canonicalName
                                       , best_key = usageKey
                                       , everything()
                                       )
                       )


    if(!is.null(fixes)) {

      tax_res$lutaxa <- tax_res$lutaxa %>%
        dplyr::mutate(taxa = stringi::stri_replace_all_regex(taxa
                                                             , pattern = fixes$resolved_to
                                                             , replacement = fixes$prefer
                                                             , vectorize = FALSE
                                                             )
                      )

    }

    if(!is.null(overrides)) {

      tax_res$lutaxa <- tax_res$lutaxa %>%
        dplyr::left_join(overrides %>%
                           dplyr::select(original, prefer, prefer_rank)
                         , by = c("original_name" = "original")
                         ) %>%
        dplyr::mutate(taxa = dplyr::case_when(!is.na(prefer) ~ prefer
                                              , TRUE ~ taxa
                                              )
                      , use_rank = dplyr::case_when(!is.na(prefer_rank) ~ prefer_rank
                                              , TRUE ~ use_rank
                                              )
                      )

    }

    tax_res$lutaxa <- tax_res$lutaxa %>%
      dplyr::select(-tidyselect::matches("key|prefer"))

    # return-----

    return(tax_res)

  }
