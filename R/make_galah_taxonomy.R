

#' Get taxonomy via `galah::taxa_search()`
#'
#' Only queries galah for taxa not already in `taxonomy_file`. Can return a list,
#' for several levels of taxonomic hierarchy, with the 'best' match at that
#' level. For example, if 'genus' is provided in `needed_ranks`, the returned
#' list will have an element 'genus' that contains, in a column named `taxa`,
#' and for each of the original names provided, the best result at genus level
#' _or higher_ (in cases where no genus level match was available).
#'
#' Previous `envClean::make_taxonomy()` function is still available via
#' `envClean::make_gbif_taxonomy()`
#'
#' @param df Dataframe with `taxa_col`. Can be NULL only if taxonomy_file
#' already exists.
#' @param taxa_col Character or index. Name or index of column with taxa names.
#' Each unique taxa in this column will appear in the results list element
#' `lutaxa`in a column called `original_name`
#' @param taxonomy_file Character. File path to save results to. File type
#' ignored. .parquet file used, partitioned on kingdom. If using this file
#' directly (rather than via return_taxonomy = TRUE) remember to ungroup the
#' result returned via `arrow::open_dataset()`.
#' @param force_new List with elements `taxa_col` and `difftime`. If
#' `taxonomy_file` already exists any `taxa_col` matches between `force_new` and
#' `taxonomy_file` will be requeried. Likewise any `original_name` that has not
#' been searched since `difftime` will be requeried. Note the name `taxa_col`
#' should be as provided as per the `taxa_col` argument. Set either to `NULL`
#' to ignore.
#' @param remove_taxa Character. Rows with regular expressions that match
#' `remove_taxa` are removed (rows are removed).
#' @param remove_strings Character. Text that matches `remove_strings` is
#' removed from the string before searching (text, not row, is removed).
#' @param tri_strings Character. Text that matches `tri_strings` is
#' used to indicate if the original_name is trinomial (original_is_tri field in lutaxa).
#' @param non_name_strings Character. Text that matches `non_name_strings` is
#' used to remove non-names from original_names and count the number of words in the string
#' to indicate if the original_name is trinomial (original_is_tri field in lutaxa).
#' @param return_taxonomy Logical. If `TRUE`, a list is returned containing the
#' best match for each original_name in `lutaxa` and additional elements named
#' for their rank (see `envClean::lurank`) with unique rows for that rank. One
#' element per rank provided in `needed_ranks`
#' @param limit Logical. If `TRUE` the returned list will be limited to those
#' `original_name`s in `df`
#' @param needed_ranks Character vector of ranks required in the returned list.
#' Can be "all" or any combination of ranks from `envClean::lurank` greater than
#' or equal to _subspecies_.
#' @overrides Used to override results returned by `galah::search_taxa()`.
#' Dataframe with (at least) columns: `taxa_col` and `taxa_to_search`.
#' Can also contain any number of `use_x` columns where `x` is any of
#' `r envFunc::vec_to_sentence(lurank$rank)`. A two step process then attempts
#' to find better results than if searched on `taxa_col`. Step 1 searches for
#' `taxa_to_search` instead of `taxa_col`. If any `use_x` columns are present,
#' step 2 then checks that the results from step 1 have a result at `x`. If not,
#' level `x` results will be taken from `use_x`.
#'
#' @return Null or list (depending on `return_taxonomy`). Writes
#' `taxonomy_file`. `taxa_col` will be `original_name` in any outputs.
#' If list, then elements:
#' \itemize{
#'   \item raw - the 'raw' results returned from `galah::search_taxa()`, tweaked
#'     by column `rank` being an ordered factor as per `envClean::lurank`.
#'   \item needed_ranks - One element for each rank specified in `needed_ranks`.
#'      \itemize{
#'        \item lutaxa - dataframe. For each unique name in `taxa_col`, the best
#'        `taxa` taxonomic bin to use, for each `original_name`, taking into
#'        account each level of `needed_ranks`
#'          \itemize{
#'            \item original_name - unique values from `taxa_col`
#'            \item match_type - directly from `galah::search_taxa()`
#'            \item matched_rank - `rank` column from `galah::search_taxa()`
#'            \item returned_rank - the rank of the `taxa` returned for each
#'            `original_name`. This will never be lower than `needed_rank` but
#'            may be higher than `needed_rank` if no match was available at
#'            `needed_rank`. Use this 'rank' to filter bins in a cleaning
#'            workflow
#'            \item taxa - the best taxa available for `original_name` at
#'            `needed_rank`, perhaps taking into account `overrides`
#'            \item override - is the `taxa` the result of an override?
#'            \item original_is_tri - is the `original_name` a trinomial?
#'            Useful for extra filtering of cases where the returned rank is species
#'            but is incorrect and is supposed to be subspecies or below.
#'            Based on the `tri_strings` and more than two words derived after
#'            removing `non_name_strings`.
#'          }
#'        \item taxonomy - dataframe. For each `taxa` in `lutaxa` a row of
#'        taxonomic hierarchy
#'      }
#'  }
#'
#' @export
#'
#' @example inst/examples/make_galah_taxonomy_ex.R
#'
#'
  make_taxonomy <- function(df = NULL
                            , taxa_col = "original_name"
                            , taxonomy_file = tempfile()
                            , force_new = list(original_name = NULL
                                               , timediff = as.difftime(26
                                                                        , units = "weeks"
                                                                        )
                                               )
                            , remove_taxa = c("bold:"
                                              , "BOLD:"
                                              , "unverified"
                                              , "annual herb"
                                              , "annual grass"
                                              , "incertae sedis"
                                              , "\\?"
                                              , "another species"
                                              , "not naturalised in SA"
                                              , "unidentified"
                                              , "unverified"
                                              , "annual tussock grass"
                                              , "*no id"
                                              )
                            , remove_strings = c("\\s\\-\\-\\s.*" # blah -- abc xyz
                                                 , "\\ssp\\.$" # blah sp.END
                                                 , "\\sssp\\.$" # blah ssp.END
                                                 , "\\sspec\\.$" # blah spec.END
                                                 , "dead"
                                                 ) # blah not removed, everything else removed
                            , tri_strings = c("\\sssp\\.\\s"
                                               , "\\svar\\.\\s"
                                               , "\\ssubsp"
                                               , "\\sform\\)"
                                               , "\\sform\\s"
                                               , "\\sf\\."
                                               )
                            , non_name_strings = c("\\s\\([^\\)]+\\)"
                                                    ,"\\svar\\."
                                                    ,"\\ssp\\."
                                                    ,"\\sssp\\."
                                                    ,"\\ssubsp\\."
                                                    ,"\\ssubspecies"
                                                    ,"\\snov\\."
                                                    ,"-"
                                                    ,"\\sf\\."
                                                    ,"\\sx\\s"
                                                    )
                            , atlas = c("Australia")
                            , return_taxonomy = TRUE
                            , limit = TRUE
                            , needed_ranks = c("species")
                            , overrides = NULL
                            ) {

    # setup ------
    lurank <- envClean::lurank

    if(tools::file_ext(taxonomy_file) == "") taxonomy_file <- paste0(taxonomy_file, ".parquet")

    ## needed ranks -------

    if("all" %in% needed_ranks) needed_ranks <- unique(c(needed_ranks, as.character(lurank$rank)))

    needed_ranks <- factor(needed_ranks[needed_ranks %in% lurank$rank]
                           , levels = levels(lurank$rank)
                           , ordered = TRUE
                           )

    needed_ranks <- needed_ranks[needed_ranks >= "subspecies"]

    if(!is.null(df)) {
      # If taxonomy_file already exists, bring it in then remove any force_new
      if(file.exists(taxonomy_file)) {

        # previous -------
        previous <- arrow::open_dataset(taxonomy_file) %>%
          dplyr::collect() %>%
          dplyr::ungroup() %>%
          dplyr::filter(!grepl(paste0(remove_taxa, collapse = "|"), original_name))

        # remove any 'force_new'
        if(!is.null(force_new$timediff)) {

          previous <- previous %>%
            dplyr::filter(difftime(Sys.time()
                                   , stamp
                                   , units = "days"
                                   ) <
                            as.numeric(force_new$timediff
                                       , units = "days"
                                       )
                          )
        }

        if(!is.null(force_new$original_name)) {

          previous <- previous %>%
            dplyr::filter(!(original_name %in% force_new[taxa_col]))

        }

      } else {

        previous <- tibble::tibble(original_name = NA_character_)

      }

      # Rename taxa_col (and make a 'searched' name)

      rename_taxa_col <- c(original_name = names(df[taxa_col]))

      to_check <- df %>%
        dplyr::rename(tidyselect::all_of(rename_taxa_col)) %>%
        dplyr::distinct(dplyr::across(tidyselect::any_of(lurank$rank)), original_name) %>%
        dplyr::filter(!grepl(paste0(remove_taxa
                                    , collapse = "|"
                                    )
                             , original_name
                             )
                      , original_name != ""
                      ) %>%
        dplyr::anti_join(previous) %>%
        dplyr::mutate(searched_name = gsub(paste0(remove_strings
                                                    , collapse = "|"
                                                    )
                                             , ""
                                             , original_name
                                             )
                      , searched_name = stringr::str_squish(searched_name)
                      ) %>%
        # remove names that contain only NA, blanks, digits or dates
        dplyr::filter(is.na(as.numeric(gsub("[^[[:alnum:]]+"
                                            , ""
                                            , original_name
                                            )
                                       )
                            )
                      , is.na(as.numeric(lubridate::dmy(original_name)))
                      , !is.na(original_name)
                      , !is.na(searched_name)
                      , searched_name != ""
                      ) %>%
        dplyr::arrange(dplyr::across(tidyselect::any_of(lurank$rank)), original_name)

      if("kingdom" %in% names(to_check)) {

        if(sum(is.na(to_check$kingdom)) > 0) {

          # redundant if every original name has a kingdom

          # remove kingdom == NA if original_name already in to_check with !is.na(kingdom)
          to_check <- to_check %>%
            dplyr::count(searched_name) %>%
            dplyr::filter(n > 1) %>%
            dplyr::inner_join(to_check) %>%
            dplyr::filter(dplyr::if_any(tidyselect::any_of(lurank$rank)
                                        , is.na
                                        )
                          ) %>%
            dplyr::select(tidyselect::any_of(names(to_check))) %>%
            dplyr::mutate(keep = FALSE) %>%
            dplyr::right_join(to_check) %>%
            dplyr::mutate(keep = dplyr::if_else(is.na(keep), TRUE, keep)) %>%
            dplyr::filter(keep) %>%
            dplyr::select(tidyselect::any_of(names(to_check)))

        }

      }

      # get taxonomy--------
      if(length(to_check$searched_name) > 0){

        old_atlas <- galah::galah_config()$atlas$region

        galah::galah_config(atlas = atlas)

        new <- to_check %>%
          dplyr::select(original_name) %>%
          dplyr::bind_cols(galah::search_taxa(to_check %>%
                                                dplyr::select(tidyselect::any_of(lurank$rank)
                                                              , scientificName = searched_name
                                                              )
                                              )
                           ) %>%
          dplyr::mutate(stamp = Sys.time())

        galah::galah_config(atlas = old_atlas)

      } else {

        new <- tibble::tibble(original_name = NA_character_)

      }


      # clean up -------

      new <- previous %>%
        dplyr::bind_rows(new) %>%
        dplyr::filter(!is.na(original_name)) %>%
        dplyr::distinct() %>%
        dplyr::mutate(subspecies = dplyr::case_when(rank <= "subspecies" ~ gsub("\\s\\(.*\\)\\s", " ", scientific_name)
                                                    , TRUE ~ NA_character_
                                                    )
                      ) %>%
        dplyr::select(!matches("^issues$")) %>%
        dplyr::mutate(rank = factor(rank
                                    , levels = levels(envClean::lurank$rank)
                                    , ordered = TRUE
        )
        )

      out_names <- c(names(new), "override")

      # overrides --------
      if(!is.null(overrides)) {

        # rename taxa_col
        overrides <- overrides %>%
          dplyr::rename(tidyselect::any_of(rename_taxa_col)) %>%
          dplyr::mutate(taxa_to_search = dplyr::if_else(is.na(taxa_to_search)
                                                        , original_name
                                                        , taxa_to_search
                                                        )
                        )

        new <- new %>%
          dplyr::anti_join(overrides %>%
                             dplyr::distinct(original_name)
                           )


        # attempt 1: match by galah::search_taxa
        combined_overrides <- overrides %>%
          dplyr::bind_cols(galah::search_taxa(overrides$taxa_to_search)) %>%
          dplyr::mutate(rank = factor(rank, levels = levels(lurank$rank), ordered = TRUE)
                        , subspecies = dplyr::case_when(rank <= "subspecies" ~ gsub("\\s\\(.*\\)\\s", " ", scientific_name)
                                                        , TRUE ~ NA_character_
                        )
          )

        # attempt 2: replace with override if match was not at suitable level in galah::search_taxa
        if(any(grepl("use_", names(overrides)))) {

          overrides_long <- overrides %>%
            dplyr::select(original_name, tidyselect::matches("use_")) %>%
            tidyr::pivot_longer(tidyselect::matches("use_"), names_to = "returned_rank", values_to = "new_taxa") %>%
            dplyr::filter(!is.na(new_taxa)) %>%
            dplyr::mutate(returned_rank = factor(gsub("use_", "", returned_rank), levels = levels(lurank$rank), ordered = TRUE))

          combined_overrides <- combined_overrides %>%
            tidyr::pivot_longer(tidyselect::any_of(lurank$rank), names_to = "returned_rank", values_to = "taxa") %>%
            dplyr::left_join(overrides_long) %>%
            dplyr::mutate(change_taxa =is.na(taxa) & !is.na(new_taxa)) %>%
            dplyr::mutate(taxa = dplyr::case_when(change_taxa ~ new_taxa
                                                  , TRUE ~ taxa
                                                  )
                          ) %>%
            dplyr::select(-new_taxa, -change_taxa) %>%
            tidyr::pivot_wider(names_from = returned_rank, values_from = taxa) %>%
            dplyr::mutate(stamp = Sys.time()) %>%
            dplyr::mutate(rank = factor(rank
                                          , levels = levels(envClean::lurank$rank)
                                          , ordered = TRUE
                                          )
                          )

        }

        new <- new %>%
          dplyr::bind_rows(combined_overrides %>%
                             dplyr::mutate(override = TRUE)
          ) %>%
          dplyr::select(tidyselect::any_of(out_names)) %>%
          dplyr::mutate(override = dplyr::if_else(is.na(override), FALSE, override)
                        , words=stringr::str_count(gsub(paste(non_name_strings,collapse="|"),"",original_name),"\\w+")
                        , original_is_tri=dplyr::case_when(grepl(paste(tri_strings,collapse="|"),original_name) ~ TRUE
                                                           ,words>2 & !grepl("\\ssp\\.|\\sall\\ssubspecies",original_name) ~ TRUE
                                                           ,.default = FALSE
                        )
          ) %>%
          dplyr::relocate(subspecies, .after = "species") %>%
          dplyr::select(-words) %>%
          dplyr::distinct()

      }

      # save -------

      message("saving results to ", taxonomy_file)

      arrow::write_dataset(new
                           , path = taxonomy_file
                           )

    }

    # res -------

    if(return_taxonomy) {

      if(!exists("new", inherits = FALSE)) {

        # if make_taxonomy called with df = NULL
        new <- arrow::open_dataset(taxonomy_file) %>%
          dplyr::collect() %>%
          dplyr::ungroup()

      }

      res <- list(raw = new %>%
                    {if(all(limit, !is.null(df))) (.) %>% dplyr::inner_join(df %>%
                                                                              dplyr::rename(tidyselect::any_of(rename_taxa_col)) %>%
                                                                              dplyr::distinct(original_name)
                                                                            ) else (.)
                    } %>%
                    dplyr::distinct()
                  )

      # long ------
      long <- res$raw %>%
        dplyr::rename(matched_rank = rank) %>%
        tidyr::pivot_longer(tidyselect::matches(paste0(envClean::lurank$rank, collapse = "|"))
                            , names_to = "returned_rank"
                            , values_to = "taxa"
                            ) %>%
        dplyr::filter(!is.na(taxa)) %>%
        dplyr::mutate(returned_rank = factor(returned_rank, levels = levels(lurank$rank), ordered = TRUE)) %>%
        dplyr::select(original_name, match_type, returned_rank, matched_rank, taxa
                      , original_is_tri
                      , tidyselect::any_of("override")
                      )

      # needed ranks -------
      all_ranks <- purrr::map(needed_ranks
                              , \(x) {

                                this_rank <- as.character(x)

                                base <- long %>%
                                  dplyr::filter(returned_rank >= x)

                                rank_taxonomy <- list()

                                rank_taxonomy$lutaxa <- base %>%
                                  dplyr::group_by(original_name) %>%
                                  dplyr::filter(returned_rank == min(returned_rank)) %>%
                                  dplyr::ungroup() %>%
                                  dplyr::distinct(original_name, match_type
                                                  , matched_rank, returned_rank, taxa
                                                  , original_is_tri
                                                  , dplyr::across(tidyselect::any_of("override"))
                                                  )

                                rank_taxonomy$taxonomy <- rank_taxonomy$lutaxa %>%
                                  dplyr::distinct(original_name, taxa) %>%
                                  dplyr::left_join(base %>%
                                                     tidyr::pivot_wider(names_from = "returned_rank", values_from = "taxa") %>%
                                                     dplyr::select(original_name, tidyselect::any_of(lurank$rank))
                                                   ) %>%
                                  dplyr::select(-original_name) %>%
                                  dplyr::distinct()

                                return(rank_taxonomy)

                                }
                              )

      names(all_ranks) <- needed_ranks

      res <- c(res, all_ranks)

    }

    return(if(return_taxonomy) res else invisible(NULL))

  }
