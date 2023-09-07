

#' Make taxonomy lookups
#'
#' @param df Dataframe with taxa column.
#' @param taxa_col Character. Name of column with taxa names
#' @param taxonomy_file Character. Path to results from `envFunc::get_taxonomy()`
#' @param target_rank Character. Default is 'species'. At what level of the
#' taxonomic hierarchy are results desired. This is the most detailed taxonomy
#' returned. i.e. if genus is the `target_rank`, no taxa below genus are
#' returned. See `envFunc::lurank` `rank` column.
#' @param limit Logical. If true (default), the output taxonomy will be limited
#' to the input names in `df`. Otherwise, all taxa found in `taxonomy_file` will
#' be returned.
#' @param ... Passed to `get_taxonomy()`
#'
#' @return named list with elements:
#'     \item{lutaxa}{Dataframe. For each unique name in `taxa_col`, the best
#'     `taxa` to use (taking into account `target_rank`)}
#'     \item{taxonomy}{Dataframe. For each `taxa`in `lutaxa` a row of taxonomic
#'     hierarchy and matching gbif usageKeys}
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


    # best -------

    best <- raw %>%
      dplyr::select(original_name
                    , original_rank = rank
                    , status
                    , kingdom
                    , contains("Key")
                    ) %>%
      {if(target_rank %in% c("subspecies", "form", "variety"))
        # this sometimes puts, say, subspeciesKey in where it is actually, say, a speciesKey. Fixed below
        (.) %>% dplyr::rename("{target_rank}Key" := acceptedUsageKey) else (.) %>% dplyr::select(-acceptedUsageKey)
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


    # taxonomy-------

    tax_res$taxonomy <- accepted %>%
      dplyr::select(taxa = canonicalName
                    , best_key = usageKey
                    , everything()
                    ) %>%
      dplyr::inner_join(best %>%
                          dplyr::distinct(best_key)
                        )


    # lutaxa-------

    tax_res$lutaxa <- best %>%
      dplyr::left_join(tax_res$taxonomy %>%
                         dplyr::select(taxa, best_key)
                       )


    # return-----

    return(tax_res)

  }
