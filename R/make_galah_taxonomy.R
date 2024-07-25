

#' Get taxonomy via `galah::taxa_search()`
#'
#' Only queries galah for taxa not already in `taxonomy_file`.
#'
#' @param df Dataframe with `taxa_col`
#' @param taxa_col Character or index. Name or index of column with taxa names.
#' Each unique taxa in this column will appear in the results in a column called
#' `original_name`
#' @param taxonomy_file Character. Path to save results to.
#' @param force_new List with elements `taxa_col` and `difftime`. If
#' `taxonomy_file` already exists any `taxa_col` matches between `force_new` and
#'  `taxonomy_file` will be requeried. Likewise any `original_name` that has not
#'  been searched since `difftime` will be requeried. Note the name `taxa_col`
#'  should be as provided as per the `taxa_col` argument. Set either to `NULL`
#'  to ignore.
#' @param remove_taxa Character. Regular expressions to be matched. Any matches
#' will be filtered before searching. Removes any rows that match.
#' @param remove_strings Character. Regular expressions to be matched. Any
#' matches will be removed from the string before searching. Removes any
#' text that matches, but the row remains.
#' @param make_taxonomy Logical. If `TRUE`, a list is returned containing the
#' best match for each original_name in `lutaxa` and additional elements named
#' for their rank (see `envClean::lurank`) with unique rows for that rank. One
#' element per rank provided in `needed_ranks`
#' @param limit Logical. If `TRUE` the returned list will be limited to those
#' `original_name`s in `df`
#' @param needed_ranks Character vector of ranks required in the returned list.
#' Can be "all" or any combination of ranks from `envClean::lurank` greater than
#' or equal to _species_. Note that only results obtaining that rank are
#' returned per list element. Thus, an `original_name` at binomial level that
#' was only matched at genus level will not appear in the "species" list
#' element.
#'
#' @return Null or list depending on `make_taxonomy`. Writes `taxonomy_file`.
#' @export
#'
#' @examples
  make_galah_taxonomy <- function(df
                                  , taxa_col = "original_name"
                                  , taxonomy_file = tempfile()
                                  , force_new = list(original_name = NULL
                                                     , timediff = as.difftime(26
                                                                              , units = "weeks"
                                                                              )
                                                     )
                                  , remove_taxa = c("BOLD:"
                                                    , "dead"
                                                    , "unverified"
                                                    , "annual herb"
                                                    , "annual grass"
                                                    , "incertae sedis"
                                                    , "\\?"
                                                    )
                                  , remove_strings = c("\\sx\\s.*" # blah x abc xyz
                                                       , "\\sX\\s.*" # blah X abc xyz
                                                       , "\\s\\-\\-\\s.*" # blah -- abc xyz
                                                       #, "\\s\\(.*\\)"  # blah (abc xyz) blah (note second space left)
                                                       , "\\ssp\\.$" # blah sp.END
                                                       , "\\sssp\\.$" # blah ssp.END
                                                       , "\\sspec\\.$" # blah spec.END
                                                       ) # blah not removed, everything else removed
                                  , remove_dead = FALSE
                                  , atlas = c("Australia")
                                  , make_taxonomy = TRUE
                                  , limit = TRUE
                                  , needed_ranks = c("all")
                                  ) {

    # needed ranks -------

    if("all" %in% needed_ranks) needed_ranks <- unique(c(needed_ranks, as.character(lurank$rank)))

    needed_ranks <- factor(needed_ranks[needed_ranks %in% lurank$rank]
                           , levels = levels(lurank$rank)
                           , ordered = TRUE
                           )

    needed_ranks <- needed_ranks[needed_ranks >= "species"]


    # clean -------

    if(remove_dead) {

      # any 'name' including dead removed altogether
      remove_taxa <- c(remove_taxa, "dead", "DEAD", "Dead")

    } else {

      # any instance of 'dead' removed from string to give better chance of name match
      remove_strings <- c(remove_strings, "dead", "DEAD", "Dead")

    }

    lurank <- envClean::lurank

    taxa_col <- names(df[taxa_col])

    if(tools::file_ext(taxonomy_file) == "") taxonomy_file <- paste0(taxonomy_file, ".parquet")

    # If taxonomy_file already exists, bring it in then remove any force_new
    if(file.exists(taxonomy_file)) {

      previous <- arrow::open_dataset(taxonomy_file) %>%
        dplyr::collect() %>%
        dplyr::ungroup()

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

      previous <- tibble::tibble(original_name = NA)

    }

    # Collect any unfound taxa_col (and make a 'searched' name)
    to_check <- df %>%
      dplyr::rename(original_name = !!rlang::ensym(taxa_col)) %>%
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
        dplyr::mutate(stamp = Sys.time()) %>%
        dplyr::mutate(rank = factor(rank
                                      , levels = levels(envClean::lurank$rank)
                                      , ordered = TRUE
                                      )
                      )

      galah::galah_config(atlas = old_atlas)

    }


    # clean up -------

    new <- previous %>%
      dplyr::bind_rows(new) %>%
      dplyr::filter(!is.na(original_name)) %>%
      dplyr::group_by(original_name) %>%
      dplyr::filter(stamp == max(stamp)) %>%
      dplyr::ungroup()


    # save -------

    arrow::write_dataset(new
                         , path = taxonomy_file
                         )

    # res -------

    if(make_taxonomy) {

      res <- list(raw = new %>%
                    {if(limit) (.) %>% dplyr::inner_join(df %>% dplyr::distinct(original_name)
                                                         , by = c("original_name" = taxa_col)
                                                         ) else (.)
                      } %>%
                    dplyr::mutate(subspecies = dplyr::if_else(rank <= "subspecies", scientific_name, NA_character_))
                  )

      # long ------
      long <- res$raw %>%
        dplyr::mutate(subspecies = dplyr::if_else(rank == "subspecies", scientific_name, NA_character_)) %>%
        dplyr::rename(matched_rank = rank) %>%
        tidyr::pivot_longer(tidyselect::matches(paste0(envClean::lurank$rank, collapse = "|"))
                            , names_to = "rank"
                            , values_to = "taxa"
                            ) %>%
        dplyr::filter(!is.na(taxa)) %>%
        dplyr::mutate(rank = factor(rank, levels = levels(lurank$rank), ordered = TRUE)) %>%
        dplyr::select(original_name, match_type, rank, matched_rank, taxa)

      # needed ranks -------
      all_ranks <- purrr::map(needed_ranks
                              , \(x) {

                                this_rank <- as.character(x)

                                rank_taxonomy <- long %>%
                                  dplyr::filter(rank >= x) %>%
                                  dplyr::group_by(original_name) %>%
                                  dplyr::filter(rank == min(rank)) %>%
                                  dplyr::ungroup() %>%
                                  dplyr::distinct(original_name, matched_rank, rank, taxa) %>%
                                  dplyr::left_join(res$raw %>%
                                                     dplyr::select(-rank)
                                                   ) %>%
                                  janitor::remove_empty(which = "cols")

                                }
                              )

      names(all_ranks) <- needed_ranks

      res <- c(res, all_ranks)

    }

    return(if(make_taxonomy) res else invisible(NULL))

  }
