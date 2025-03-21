

#' Get taxonomy via `galah::taxa_search()`
#'
#' Only queries galah for taxa not already in `taxonomy_file`. Can return a list,
#' for several levels of taxonomic hierarchy, with the 'best' match at that
#' level. For example, if 'genus' is provided in `needed_ranks`, the returned
#' list will have an element 'genus' that contains, in a column named `taxa`,
#' and for each of the original names provided, the best result at genus level
#' _or higher_ (in cases where no genus level match was available).
#'
#' The argument `tweak_species` replaces the `galah::search_taxa()` result in
#' the `species` column with the result in the `scientific_name` column. This
#' attempt to deal with instances where `galah::search_taxa()` returns odd
#' results in `species` but good results in `scientific_name`. e.g.
#' galah::search_taxa("Acacia sp. Small Red-leaved Wattle (J.B.Williams 95033)")
#' returns `spec.` in the species column but
#' `Acacia sp. Small Red-leaved Wattle (J.B.Williams 95033)` in the
#' `scientific_name` column
#'
#' Previous `envClean::make_taxonomy()` function is still available via
#' `envClean::make_gbif_taxonomy()`
#'
#' @param df Dataframe with `taxa_col`. Can be `NULL` only if taxonomy_file
#' already exists.
#' @param taxa_col Character or index. Name or index of column with taxa names.
#' Each unique taxa in this column will be queried against `galah::search_taxa`
#' and appear in the results list element `lutaxa`in a column called
#' `original_name`
#' @param taxonomy_file Character. File path to save results to. File type
#' ignored. .parquet file used.
#' @param force_new List with elements `difftime` and any column name from
#' `taxonomy_file`. If `taxonomy_file` already exists any column matches between
#' `force_new` and `taxonomy_file`, matching levels within that column will be
#' requeried. Likewise any `original_name` that has not been searched since
#' `difftime` will be requeried. Set either to `NULL` to ignore.
#' @param remove_taxa Character. Rows with regular expressions in `taxa_col`
#' that match `remove_taxa` are removed (rows are removed).
#' @param remove_strings Character. Text that matches `remove_strings` is
#' removed from the `taxa_col` before searching (text, not row, is removed).
#' @param not_names Character. Text that matches `non_name_strings` is used to
#' remove non-names from original_names before a word count to indicate (guess)
#' if the original_name is trinomial (original_is_tri field in lutaxa).
#' @param tri_strings Character. Text that matches `tri_strings` is
#' used to indicate if the original_name is trinomial (original_is_tri field in
#' output lutaxa).
#' @param atlas Character. Name of galah atlas to use.
#' @param tweak_species. Logical. If `TRUE` (default) and the returned `species`
#' column result ends in a full stop, the values returned in the `species`
#' column will be directly taken from the `scientific_name` column. See details.
#' @param return_taxonomy Logical. If `TRUE`, a list is returned containing the
#' best match for each original_name in `lutaxa` and additional elements named
#' for their rank (see `envClean::lurank`) with unique rows for that rank. One
#' element per rank provided in `needed_ranks`
#' @param limit Logical. If `TRUE` the returned list will be limited to those
#' `original_name`s in `df`
#' @param needed_ranks Character vector of ranks required in the returned list.
#' Can be "all" or any combination of ranks from `envClean::lurank` greater than
#' or equal to _subspecies_.
#' @param overrides Used to override results returned by `galah::search_taxa()`.
#' Dataframe with (at least) columns: `taxa_col` and `taxa_to_search`.
#' Can also contain any number of `use_x` columns where `x` is any of
#' `r envFunc::vec_to_sentence(lurank$rank)`. A two step process then attempts
#' to find better results than if searched on `taxa_col`. Step 1 searches for
#' `taxa_to_search` instead of `taxa_col`. If any `use_x` columns are present,
#' step 2 then checks that the results from step 1 have a result at `x`. If not,
#' level `x` results will be taken from `use_x`.
#'
#' @return Null or list (depending on `return_taxonomy`). Writes
#' `taxonomy_file`. `taxa_col` will be `original_name` in any outputs. Note that
#' `taxa_col`, as `original_name`, will have any quotes removed.
#' If list, then elements:
#' \itemize{
#'   \item raw - the 'raw' results returned from `galah::search_taxa()`, tweaked
#'     by: column `rank` is an ordered factor as per `envClean::lurank`;
#'     rank_adj is a new column that will reflect the rank column unless rank is
#'     less than subspecies, in which case it will be subspecies; and
#'     original_is_tri is a new column
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
#'            \item original_is_tri,original_is_bi - Experimental. Is the
#'            `original_name` a trinomial or binomiail? Highlights cases where
#'            the matched rank is > subspecies but the `original_name` is
#'            probably a subspecies. Guesses are based on a word count after
#'            removal of: `not_names`; numbers; punctuation; capitalised words
#'            that are not the first word; and single letter 'words'.
#'            `bi_strings` or `tri_strings` override the guess - flagging TRUE.
#'            Note, clearly, this is only an (informed) guess at whether the
#'            `original_name` is binomial or trinomial.
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
                                              , "spec\\."
                                              )
                            , remove_strings = c("\\s\\-\\-\\s.*" # blah -- abc xyz
                                                 , "\\ssp\\.$" # blah sp.END
                                                 , "\\sssp\\.$" # blah ssp.END
                                                 , "\\sspec\\.$" # blah spec.END
                                                 , "dead"
                                                 ) # blah not removed, everything else removed
                            , not_names = c("sp"
                                            , "ssp"
                                            , "var"
                                            , "subsp"
                                            , "subspecies"
                                            , "form"
                                            , "race"
                                            , "nov"
                                            , "aff"
                                            , "cf"
                                            , "lineage"
                                            , "group"
                                            , "et"
                                            , "al"
                                            , "and"
                                            , "pl"
                                            , "revised"
                                            , "nov"
                                            , "sensu"
                                            , "lato"
                                            )
                            , tri_strings = c("\\sssp\\s"
                                              ,"\\sssp\\.\\s"
                                              , "\\svar\\s"
                                              , "\\svar\\.\\s"
                                              , "\\ssubsp\\."
                                              , "\\ssubspecies"
                                              , "\\sform\\)"
                                              , "\\sform\\s"
                                              , "\\sf\\."
                                              , "\\srace\\s"
                                              , "\\srace\\)"
                                              , "\\sp\\.v\\."
                                              )
                            , bi_strings = c("\\ssp\\s"
                                             ,"\\ssp\\.\\s"
                                             , "\\sspecies"
                                             )
                            , atlas = c("Australia")
                            , tweak_species = TRUE
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
        previous <- rio::import(taxonomy_file) %>%
          dplyr::filter(!grepl(paste0(remove_taxa, collapse = "|"), original_name)) %>%
          clean_quotes()

        # force_new -------
        ## timediff--------
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

        ## other force_new-------
        if(any(names(force_new) %in% names(previous))) {

          force_new_cols <- force_new[names(force_new) %in% names(previous)]

          for(i in 1:length(force_new_cols)) {

            x <- names(force_new_cols)[i]
            y <- force_new_cols[[i]]

            previous <- previous[! grepl(paste0("^", y, "$", collapse = "|"), previous[x][[1]]), ]

          }

        }

      } else {

        previous <- tibble::tibble(original_name = NA_character_)

      }

      # to_check---------
      # Rename taxa_col (and make a 'searched' name)

      rename_taxa_col <- c(original_name = names(df[taxa_col]))

      to_check <- df %>%
        dplyr::rename(tidyselect::all_of(rename_taxa_col)) %>%
        clean_quotes() %>%
        dplyr::distinct(dplyr::across(tidyselect::any_of(lurank$rank)), original_name) %>%
        # Remove 'remove_taxa"
        dplyr::filter(!grepl(paste0(remove_taxa
                                    , collapse = "|"
                                    )
                             , original_name
                             )
                      , original_name != ""
                      ) %>%
        # Don't check taxa that already have a result
        dplyr::anti_join(previous %>%
                           clean_quotes()
                         ) %>%
        # Don't check taxa that will be checked in overrides
        {if(!is.null(overrides)) (.) %>%
           dplyr::anti_join(overrides %>%
                              dplyr::select(tidyselect::all_of(rename_taxa_col)) %>%
                              clean_quotes()
                            ) else (.)
          } %>%
        # Remove strings
        dplyr::mutate(searched_name = gsub(paste0(remove_strings
                                                    , collapse = "|"
                                                    )
                                             , ""
                                             , original_name
                                             )
                      , searched_name = stringr::str_squish(searched_name)
                      ) %>%
        # remove names that contain only NA, blanks, digits or dates
        dplyr::filter(suppressWarnings(is.na(as.numeric(gsub("[^[[:alnum:]]+"
                                                             , ""
                                                             , original_name
                                                             )
                                                        )
                                             )
                                       )
                      , suppressWarnings(is.na(as.numeric(lubridate::dmy(original_name))))
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

        # Hack to ensure all column names available even when there are no taxa to search
        # e.g. because all df is overrides
        new <- base::suppressMessages(galah::search_taxa(c("Eucalyptus viminalis", "blah"))) %>%
          dplyr::mutate(original_name = search_term
                        , stamp = Sys.time()
                        ) %>%
          dplyr::filter(search_term == "blah")

      }


      # clean up -------

      new <- previous %>%
        dplyr::bind_rows(new) %>%
        dplyr::filter(!is.na(original_name)
                      , original_name != "blah"
                      ) %>%
        dplyr::select(!matches("^issues$")) %>%
        clean_quotes() %>%
        dplyr::group_by(original_name) %>%
        dplyr::filter(stamp == base::suppressWarnings(base::max(stamp))) %>%
        dplyr::ungroup() %>%
        dplyr::distinct() %>%
        make_subspecies_col()

      if(tweak_species) {

        new <- new %>%
          dplyr::mutate(species = dplyr::case_when(grepl("\\.$", species) ~ scientific_name
                                                   , TRUE ~ species
                                                   )
                        )

      }

      out_names <- c(names(new), "override") %>%
        grep("searched_name", ., value = TRUE, invert = TRUE)

      # overrides --------
      if(!is.null(overrides)) {

        if("override" %in% names(new)) {

          new <- new %>%
            dplyr::mutate(override = dplyr::if_else(is.na(override), FALSE, override))

        } else {

          new <- new %>% dplyr::mutate(override = FALSE)

        }


        # rename taxa_col
        overrides <- overrides %>%
          dplyr::rename(tidyselect::any_of(rename_taxa_col)) %>%
          dplyr::mutate(taxa_to_search = dplyr::if_else(is.na(taxa_to_search)
                                                        , original_name
                                                        , taxa_to_search
                                                        )
                        ) %>%
          clean_quotes()

        ## check unique overrides------
        overrides <- overrides %>%
          dplyr::distinct()

        override_check <- overrides %>%
          dplyr::count(original_name) %>%
          dplyr::filter(n > 1)

        if(nrow(override_check)) {

          stop("duplicates exist in override column '"
               , taxa_col
               , "': "
               , envFunc::vec_to_sentence(override_check$original_name)
               )

        }

        new <- new %>%
          dplyr::anti_join(overrides %>%
                             dplyr::distinct(original_name)
                           )


        # attempt 1: match by galah::search_taxa
        searched_overrides <- overrides %>%
          dplyr::bind_cols(galah::search_taxa(overrides$taxa_to_search)) %>%
          dplyr::select(- matches("issues")) %>%
          make_subspecies_col()

        # attempt 2: replace with override if match was not at suitable level in galah::search_taxa
        if(any(grepl("use_", names(overrides)))) {

          overrides_long <- overrides %>%
            dplyr::select(original_name, tidyselect::matches("use_")) %>%
            tidyr::pivot_longer(tidyselect::matches("use_"), names_to = "returned_rank", values_to = "new_taxa") %>%
            dplyr::filter(!is.na(new_taxa)) %>%
            dplyr::mutate(returned_rank = factor(gsub("use_", "", returned_rank), levels = levels(lurank$rank), ordered = TRUE))

          combined_overrides <- searched_overrides %>%
            tidyr::pivot_longer(tidyselect::any_of(lurank$rank)
                                , names_to = "matched_rank"
                                , values_to = "taxa"
                                ) %>%
            dplyr::mutate(returned_rank = matched_rank) %>%
            dplyr::left_join(overrides_long) %>%
            dplyr::mutate(change_taxa = is.na(taxa) & !is.na(new_taxa)) %>%
            dplyr::mutate(taxa = dplyr::case_when(change_taxa ~ new_taxa
                                                  , TRUE ~ taxa
                                                  )
                          , rank_adj = dplyr::case_when(change_taxa ~ returned_rank
                                                        , TRUE ~ rank_adj
                                                        )
                          , rank_adj = factor(rank_adj, levels = levels(lurank$rank), ordered = TRUE)
                          ) %>%
            dplyr::group_by(original_name) %>%
            dplyr::mutate(rank_adj = min(rank_adj, na.rm = TRUE)) %>%
            dplyr::ungroup() %>%
            dplyr::select(-new_taxa, -change_taxa, -matched_rank) %>%
            tidyr::pivot_wider(names_from = returned_rank, values_from = taxa) %>%
            dplyr::select(tidyselect::any_of(out_names)) %>%
            dplyr::mutate(stamp = Sys.time())

        }

        new <- new %>%
          dplyr::bind_rows(combined_overrides %>%
                             dplyr::mutate(override = TRUE)
                           ) %>%
          dplyr::select(tidyselect::any_of(out_names))

      }

      # final cleanup of override col
      if("override" %in% names(new)) {

        new <- new %>%
          dplyr::mutate(override = dplyr::if_else(is.na(override), FALSE, override))

      }

      # original_is_bi or tri -------
      authors <- new %>%
        dplyr::select(original_name, scientific_name_authorship) %>%
        tidytext::unnest_tokens(word, scientific_name_authorship) %>%
        dplyr::filter(!is.na(word)) %>%
        tidyr::nest(authors = c(word))

      new <- new %>%
        # simple removals (digits then hyphens)
        dplyr::mutate(original_name = stringr::str_squish(original_name)
                      , check_name = gsub("[[:digit:]]+", "", original_name)
                      # make sure hyphenated names come through as one, not two, names
                      , check_name = gsub("[a-z]\\-[a-z]", "", check_name)
                      ) %>%
        # convert to 'long' format on each word boundary in original_name
        tidytext::unnest_tokens(word, check_name, to_lower = FALSE) %>%
        # filter single letter words
        dplyr::filter(! (base::nchar(word) == 1)) %>%
        # filter not_names
        dplyr::filter(! (base::tolower(word) %in% not_names)) %>%
        # add in a row number
        dplyr::group_by(original_name) %>%
        dplyr::mutate(row_n = dplyr::row_number()) %>%
        dplyr::ungroup() %>%
        # filter capitals (Authors) that are not the 'first' word
        dplyr::filter(! (row_n > 1 & grepl("[A-Z]", word))) %>%
        # filter rows that match 'authors'
        dplyr::left_join(authors) %>%
        dplyr::filter(purrr::map2_lgl(word, authors
                                      , \(x, y) ! base::tolower(x) %in% base::tolower(y)
                                      )
                      ) %>%
        dplyr::count(original_name, name = "words") %>%
        dplyr::right_join(new) %>%
        dplyr::mutate(original_is_tri = dplyr::case_when(
          # a few cases referencing 'all subspecies"
          base::grepl("all\\ssubspecies", original_name) ~ FALSE,
          # the original name matches tri_strings
          base::grepl(paste0(tri_strings
                             , collapse = "|"
                             )
                      , original_name
                      ) ~ TRUE,
          # more than 2 words (after cleaning up words in original_name)
          words > 2 ~ TRUE,
          # odd case where the matched name has tri_string but the original_name didn't
          base::grepl(paste0(tri_strings
                             , collapse = "|"
                             )
                      , scientific_name
                      ) ~ TRUE,
          # anything else is not a trinomial
          TRUE ~ FALSE
          )
          ) %>%
        dplyr::mutate(original_is_bi = dplyr::case_when(
          # a few cases referencing 'all subspecies"
          ! base::grepl("all\\ssubspecies", original_name) ~ TRUE,
          # the original name matches tri_strings
          base::grepl(paste0(bi_strings
                             , collapse = "|"
                             )
                      , original_name
                      ) ~ TRUE,
          # more than 2 words (after cleaning up words in original_name)
          words == 2 ~ TRUE,
          # odd case where the matched name has tri_string but the original_name didn't
          base::grepl(paste0(bi_strings
                             , collapse = "|"
                             )
                      , scientific_name
                      ) ~ TRUE,
          # anything else is not a trinomial
          TRUE ~ FALSE
          )
          ) %>%
        dplyr::mutate(original_is_bi = dplyr::if_else(original_is_tri, FALSE, original_is_bi)) |>
        dplyr::select(-words) %>%
        dplyr::distinct()

      # save -------
      message("saving results to ", taxonomy_file)

      rio::export(new
                  , file = taxonomy_file
                  )

      no_matches <- new %>%
        dplyr::filter(dplyr::if_all(tidyselect::any_of(lurank$rank)
                                    , \(x) is.na(x)
                                    )
                      ) %>%
        dplyr::pull(original_name)

      if(length(no_matches)) {

        message("The following were completely unmatched: "
                , envFunc::vec_to_sentence(no_matches)
                , ". Consider providing more taxonomic levels, or an override, for each unmatched taxa?"
                )

      }

    }

    # res -------

    if(return_taxonomy) {

      if(!exists("new", inherits = FALSE)) {

        # if make_taxonomy called with df = NULL
        new <- rio::import(taxonomy_file)

      }

      res <- list(raw = new %>%
                    {if(all(limit, !is.null(df))) (.) %>% dplyr::inner_join(df %>%
                                                                              dplyr::rename(tidyselect::any_of(rename_taxa_col)) %>%
                                                                              dplyr::distinct(original_name) %>%
                                                                              clean_quotes()
                                                                            ) else (.)
                    } %>%
                    dplyr::distinct()
                  )

      # long ------
      pivot_ranks <- envClean::lurank$rank[envClean::lurank$rank != "kingdom"]

      long <- res$raw %>%
        dplyr::rename(matched_rank = rank) %>%
        tidyr::pivot_longer(tidyselect::matches(paste0(pivot_ranks, collapse = "|"))
                            , names_to = "returned_rank"
                            , values_to = "taxa"
                            ) %>%
        dplyr::filter(!is.na(taxa)) %>%
        dplyr::mutate(returned_rank = factor(returned_rank, levels = levels(lurank$rank), ordered = TRUE)) %>%
        dplyr::select(original_name, kingdom, match_type, returned_rank, matched_rank, taxa
                      , original_is_tri
                      , tidyselect::any_of("override")
                      )

      # needed ranks -------
      all_ranks <- purrr::map(needed_ranks
                              , \(x) {

                                this_rank <- as.character(x)

                                base <- long %>%
                                  dplyr::filter(returned_rank >= x) %>%
                                  dplyr::distinct()

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

                                if(any(purrr::map_lgl(rank_taxonomy$taxonomy, \(x) is.list(x)))) {

                                  stop("Duplicates have created list columns. The following taxa are duplicated: "
                                       , rank_taxonomy$taxonomy %>% dplyr::count(taxa) %>% dplyr::filter(n > 1) %>% dplyr::inner_join(rank_taxonomy$lutaxa) %>% dplyr::pull(original_name) %>% unique() %>% envFunc::vec_to_sentence()
                                       )

                                }

                                return(rank_taxonomy)

                                }
                              )

      names(all_ranks) <- needed_ranks

      res <- c(res, all_ranks)

    }

    return(if(return_taxonomy) res else invisible(NULL))

  }
