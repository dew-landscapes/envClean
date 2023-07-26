

#' Make taxonomy lookups
#'
#' @param df Dataframe with taxa column.
#' @param taxa_col Character. Name of column with taxa names
#' @param out_file Character. Path to save results to.
#' @param target_rank Character. Default is 'species'. At what level of the
#' taxonomic hierarchy are results desired.
#' @param do_common Logical. If true, an attempt will be made to find a common
#' name. Common name matches are attempted at `target_rank` (or higher taxonomic
#' rank if no taxa was matched at `target_rank` for the name supplied in
#' `taxa_col`).
#' `envClean::get_gbif_tax`.
#' @param limit Logical. If true, the output taxonomy will be limited to the
#' input names in `taxa_col`. Otherwise, any taxa in `out_file` will be
#' returned.
#' @param lifespan_col Character. Optional name of column containing lifespan
#' information.
#' @param ind_col Character. Optional name of columns containing indigenous
#' status of taxa in taxa_col.
#' @param remove_taxa Character. Regular expressions to be matched. These will
#' be filtered before searching.
#' @param remove_strings Character. Regular expressions to be matched. These
#' will be removed from the string before searching.
#' @param ... Arguments passed to `rgbif::name_backbone_checklist()`.
#'
#' @return named list with elements:
#'     \item{raw}{Dataframe. Results from `envClean::get_gbif_tax()`}
#'     \item{lutaxa}{dataframe. A lookup from unique values in `taxa_col` to
#'     matched taxonomy from GBIF backbone}
#'     \item{taxonomy}{dataframe with unique taxa and associated taxonomic
#'     information}
#'     \item{common}{If `do_common` a dataframe with unique taxa and associated
#'     common name}
#'
#' @export
#'
#' @examples
#'
  make_taxonomy <- function(df
                            , taxa_col = "original_name"
                            , out_file = tempfile()
                            #, king_type = "Plantae"
                            , target_rank = "species"
                            , do_common = TRUE
                            , limit = TRUE
                            , lifespan_col = NULL
                            , ind_col = NULL
                            , remove_taxa = c("BOLD:"
                                              , "dead"
                                              , "unverified"
                                              , "annual herb"
                                              , "annual grass"
                                              , "\\?"
                                              )
                            , remove_strings = NULL
                            , ...
                            ) {

    # setup ------

    res <- if(file.exists(out_file)) rio::import(out_file) else list()

    lurank <- envClean::lurank

    taxa_col <- names(df[taxa_col])

    if(tools::file_ext(out_file) == "") out_file <- paste0(out_file, ".rds")

    target_sort <- lurank %>%
      dplyr::filter(rank == target_rank) %>%
      dplyr::pull(sort)

    already_done_01 <- if(file.exists(out_file)) res$raw %>%
      dplyr::distinct(searched_name) %>%
      dplyr::pull()

    already_done <- sort(unique(c(get0("already_done_01"), get0("already_done_02"))))

    to_check <- df %>%
      dplyr::select(tidyselect::all_of(taxa_col)) %>%
      dplyr::distinct() %>%
      dplyr::filter(!grepl(paste0(remove_taxa
                                  , collapse = "|"
                                  )
                           , !!rlang::ensym(taxa_col)
                           )
                    ) %>%
      dplyr::mutate(original_name = !!rlang::ensym(taxa_col)
                    , searched_name = gsub(paste0(remove_strings
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
                                          , !!rlang::ensym(taxa_col)
                                          )
                                     )
                          )
                    , is.na(as.numeric(lubridate::dmy(!!rlang::ensym(taxa_col))))
                    , !is.na(!!rlang::ensym(taxa_col))
                    , !is.na(searched_name)
                    , searched_name != ""
                    )


    taxas <- tibble::tibble(searched_name = setdiff(to_check$searched_name
                                                    , already_done
                                                    )
                            ) %>%
      dplyr::distinct(searched_name) %>%
      dplyr::arrange(searched_name)

    # name_backbone--------
    if(length(taxas$searched_name) > 0){

      tax_gbif <- taxas %>%
        dplyr::inner_join(to_check) %>%
        dplyr::bind_cols(rgbif::name_backbone_checklist((.) %>% dplyr::rename(name = 1))) %>%
        dplyr::mutate(stamp = Sys.time())

      if(file.exists(out_file)) {

        res$raw <- res$raw %>%
          dplyr::bind_rows(tax_gbif)

      } else {

        res$raw <- tax_gbif

      }

    }

    return_taxonomy <- c("taxa"
                         , rgbif::taxrank()
                         , "best_key"
                         )

    # res$lutaxa------
    res$lutaxa <- res$raw %>%
      dplyr::mutate(subspecies = dplyr::case_when(rank == "SUBSPECIES" ~ canonicalName
                                                  , TRUE ~ NA_character_
                                                  )
                    ) %>%
      tidyr::pivot_longer(tidyselect::any_of(rgbif::taxrank())
                          , names_to = "hierarchy"
                          ) %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::left_join(lurank
                       , by = c("hierarchy" = "rank")
                       ) %>%
      dplyr::filter(sort <= target_sort) %>%
      dplyr::group_by(searched_name) %>%
      dplyr::filter(sort == max(sort)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(best_key = paste0(hierarchy, "Key")
                    , best_key = dplyr::if_else(grepl("subspecies", best_key)
                                                , "usageKey"
                                                , best_key
                                                )
                    ) %>%
      tidyr::pivot_longer(where(is.numeric)
                          , names_to = "key"
                          , values_to = "key_value"
                          ) %>%
      dplyr::filter(best_key == key) %>%
      dplyr::select(taxa = canonicalName
                    , searched_name
                    , original_name
                    , rank
                    , best_key = key_value
                    ) %>%
      dplyr::distinct() %>%
      tibble::as_tibble()

    # res$taxonomy-------
    res$taxonomy <- res$raw %>%
      dplyr::left_join(res$lutaxa) %>%
      dplyr::distinct(dplyr::across(tidyselect::any_of(return_taxonomy))) %>%
      tibble::as_tibble()


    # lifespan-----
    if(isTRUE(!is.null(lifespan_col))) {

      spp_lifespan <- df %>%
        dplyr::filter(!is.na(lifespan)) %>%
        dplyr::rename(original_name = !!rlang::ensym(taxa_col)) %>%
        dplyr::left_join(res$lutaxa) %>%
        dplyr::count(taxa, lifespan) %>%
        dplyr::group_by(taxa) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) %>%
        dplyr::rename(spp_lifespan = lifespan) %>%
        dplyr::distinct()

      gen_lifespan <- df %>%
        dplyr::filter(!is.na(lifespan)) %>%
        dplyr::rename(original_name = !!rlang::ensym(taxa_col)) %>%
        dplyr::left_join(res$lutaxa) %>%
        dplyr::left_join(res$taxonomy) %>%
        dplyr::count(genus, lifespan) %>%
        dplyr::group_by(genus) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) %>%
        dplyr::rename(gen_lifespan = lifespan) %>%
        dplyr::distinct()

      fam_lifespan <- df %>%
        dplyr::filter(!is.na(lifespan)) %>%
        dplyr::rename(original_name = !!rlang::ensym(taxa_col)) %>%
        dplyr::left_join(res$lutaxa) %>%
        dplyr::left_join(res$taxonomy) %>%
        dplyr::count(family, lifespan) %>%
        dplyr::group_by(family) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) %>%
        dplyr::rename(fam_lifespan = lifespan) %>%
        dplyr::distinct()

      res$taxonomy <- res$taxonomy %>%
        dplyr::left_join(spp_lifespan) %>%
        dplyr::left_join(gen_lifespan) %>%
        dplyr::left_join(fam_lifespan) %>%
        dplyr::distinct() %>%
        dplyr::mutate(lifespan = if_else(!is.na(spp_lifespan)
                                     , spp_lifespan
                                     , if_else(!is.na(gen_lifespan)
                                               , gen_lifespan
                                               , fam_lifespan
                                               )
                                     )
                      ) %>%
        dplyr::select(names(res$taxonomy),lifespan)

    }

    # ind ------
    if(isTRUE(!is.null(ind_col))) {

      ind_df <- df %>%
        dplyr::rename(original_name = !!rlang::ensym(taxa_col)) %>%
        dplyr::left_join(res$lutaxa) %>%
        make_ind_status(taxa_col = "taxa") %>%
        dplyr::add_count(taxa) %>%
        dplyr::mutate(ind = if_else(n > 1,"U",ind)) %>%
        dplyr::select(-n) %>%
        dplyr::distinct()

      res$taxonomy <- res$taxonomy %>%
        dplyr::inner_join(ind_df)

    }

    # res$common -------
    if(do_common) {

      if("common" %in% names(res)){

        old_common <- res$common %>%
          dplyr::filter(searched)

      }

      new_common <- res$taxonomy %>%
        dplyr::distinct(taxa, best_key) %>%
        {if(exists("old_common")) (.) %>% dplyr::anti_join(old_common) else (.)} %>%
        dplyr::mutate(common = purrr::map_chr(best_key
                                              , get_gbif_common
                                              )
                      )

      res$common <- dplyr::bind_rows(mget(ls(pattern = "new_common|old_common"))) %>%
        dplyr::distinct() %>%
        dplyr::mutate(searched = TRUE)

    }


    # export -------

    rio::export(res
                , out_file
                )


    # limit------

    if(limit) {

      raw <- res$raw %>%
        dplyr::inner_join(df %>%
                            dplyr::distinct(!!rlang::ensym(taxa_col))
                          , by = c("original_name" = taxa_col)
                          )

      lutaxa <- res$lutaxa %>%
        dplyr::inner_join(df %>%
                            dplyr::distinct(!!rlang::ensym(taxa_col))
                          , by = c("original_name" = taxa_col)
                          )

      taxonomy <- res$taxonomy %>%
        dplyr::inner_join(lutaxa %>%
                            dplyr::distinct(taxa)
                          )

      common <- res$common %>%
        dplyr::inner_join(lutaxa %>%
                            dplyr::distinct(taxa)
                          )

      res <- list(raw = raw
                  , lutaxa = lutaxa
                  , taxonomy = taxonomy
                  , common = common
                  )

    }

    return(res)

  }
