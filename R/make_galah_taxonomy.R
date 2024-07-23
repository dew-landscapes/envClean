

#' Get taxonomy via `galah::taxa_search()`
#'
#' Only queries galah for taxa not already in `taxonomy_file`.
#'
#' @param df Dataframe with taxa column.
#' @param taxa_col Character. Name of column with taxa names. Each unique taxa
#' in this column will appear in the results in a column called `original_name`
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
#' for their rank (see `envClean::lurank`) with unique rows for that rank.
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
                                                    , "\\?"
                                                    )
                                  , remove_strings = c("\\sx\\s.*" # blah x abc xyz
                                                       , "\\sX\\s.*" # blah X abc xyz
                                                       , "\\s\\-\\-\\s.*" # blah -- abc xyz
                                                       #, "\\s\\(.*\\)"  # blah (abc xyz) blah (note second space left)
                                                       #, "\\ssp\\.$" # blah sp.END
                                                       #, "\\sssp\\.$" # blah ssp.END
                                                       #, "\\sspec\\.$" # blah spec.END
                                                       ) # blah not removed, everything else removed
                                  , remove_dead = FALSE
                                  , atlas = c("both", "Australia", "gbif")
                                  , make_taxonomy = TRUE
                                  ) {



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

      df_res <- arrow::open_dataset(taxonomy_file) %>%
        dplyr::collect() %>%
        dplyr::ungroup()

      if(!is.null(force_new$timediff)) {

        df_res <- df_res %>%
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

        df_res <- df_res %>%
          dplyr::filter(!(original_name %in% force_new[taxa_col]))

      }

    } else {

      df_res <- tibble::tibble(original_name := NA)

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
      dplyr::anti_join(df_res) %>%
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


    # name_backbone--------
    if(length(to_check$searched_name) > 0){

      if(any(atlas %in% c("galah", "both"))) {

        old_atlas <- galah::galah_config()$atlas$region

        ## galah ------
        galah::galah_config(atlas = "Australia")

        tax_aus <- to_check %>%
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

      if(any(atlas %in% c("gbif", "both"))) {

        if(exists("tax_aus")) {

          to_check <- tax_aus %>%
            dplyr::filter(grepl("higher", match_type)) %>%
            dplyr::select(original_name) %>%
            dplyr::inner_join(to_check)

        }

        if(nrow(to_check) > 0) {

          ## gbif -------
          galah::galah_config(atlas = "gbif")

          tax_gbif <- to_check %>%
            dplyr::select(original_name) %>%
            dplyr::bind_cols(galah::search_taxa(to_check$searched_name)) %>%
            dplyr::mutate(stamp = Sys.time()) %>%
            dplyr::mutate(rank = factor(rank
                                          , levels = levels(envClean::lurank$rank)
                                          , ordered = TRUE
                                          )
                          ) %>%
            dplyr::mutate(taxon_concept_id = as.character(taxon_concept_id))

          galah::galah_config(atlas = "Australia")

        }

      }

      df_res <- df_res %>%
        {if(exists("tax_aus")) (.) %>% dplyr::bind_rows(tax_aus) else (.)} %>%
        {if(exists("tax_gbif")) (.) %>%
             dplyr::filter(!grepl("higher", match_type)) %>%
            dplyr::bind_rows(tax_gbif) else (.)
          } %>%
        dplyr::filter(!is.na(original_name))

    }

    # clean up -------

    df_res <- df_res %>%
      dplyr::filter(!is.na(original_name)) %>%
      dplyr::group_by(original_name) %>%
      dplyr::filter(stamp == max(stamp)) %>%
      dplyr::ungroup()


    # save -------

    arrow::write_dataset(df_res %>%
                           dplyr::group_by(kingdom)
                         , path = taxonomy_file
                         )

    # res -------

    if(make_taxonomy) {

      res <- list()

      res$lutaxa <- df_res

      make_ranks <- lurank$rank[lurank$rank >= "subspecies"]

      all_ranks <- purrr::map(make_ranks
                              , \(x) df_res %>%
                                dplyr::rename(subspecies = scientific_name) %>%
                                dplyr::select(tidyselect::any_of(make_ranks[make_ranks >= x])) %>%
                                dplyr::distinct()
                              )

      names(all_ranks) <- make_ranks

      res <- c(res, all_ranks)

    }

    return(if(make_taxonomy) res else invisible(NULL))

  }
