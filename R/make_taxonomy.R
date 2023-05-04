

#' Make taxonomy lookups
#'
#' @param df Dataframe with taxa column.
#' @param taxa_col Character. Name of column with taxa names
#' @param out_file Character. Path to save results to. Saving is iterative as
#' retrieving names can take some time.
#' @param king_type Character. Kingdom type (i.e. Plantae, Animalia etc.)
#' @param target_rank Character. Default is 'species'. At what level of the
#' taxonomic hierarchy are results desired.
#' @param do_common Logical. If true, an attempt will be made to find a common
#' name. Common name matches are attempted at `target_rank` (or higher taxonomic
#' rank if no taxa was matched at `target_rank` for the name supplied in
#' `taxa_col`). `target_rank` is supplied in `...` to`envClean::get_gbif_tax()`.
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
                            , king_type = "Plantae"
                            , target_rank = "species"
                            , do_common = TRUE
                            , limit = TRUE
                            , lifespan_col = NULL
                            , ind_col = NULL
                            , remove_taxa = c("BOLD:.*\\d{4}"
                                              , "dead"
                                              , "unverified"
                                              , "annual herb"
                                              , "annual grass"
                                              , "[[:alpha:]]{1}\\d{5,}"
                                              , "^[[:alpha:]]{1,3}$"
                                              , "\\?"
                                              )
                            , remove_strings = c("\\s*\\(.*\\)"
                                                 , "\\'"
                                                 , " spp\\."
                                                 ,  "ssp\\."
                                                 , " sp\\."
                                                 , " var\\."
                                                 , "subsp\\."
                                                 #, " ex"
                                                 , " [A-Z].*"
                                                 , "#"
                                                 , "\\s^"
                                                 , " x .*$| X .*$"
                                                 )
                            ) {

    # setup ------

    res <- if(file.exists(out_file)) rio::import(out_file) else list()

    lurank <- envClean::lurank

    taxa_col <- names(df[taxa_col])

    if(tools::file_ext(out_file) == "") out_file <- paste0(out_file, ".rds")

    tmp_file <- paste0(gsub(paste0("\\."
                                   , tools::file_ext(out_file)
                                   )
                            , ""
                            , out_file
                            )
                       , "_temp.rds"
                       )

    target_sort <- lurank %>%
      dplyr::filter(rank == target_rank) %>%
      dplyr::pull(sort)

    already_done_01 <- if(file.exists(out_file)) res$raw %>%
      dplyr::distinct(searched_name) %>%
      dplyr::pull()

    already_done_02 <- if(file.exists(tmp_file)) rio::import(tmp_file) %>%
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
      dplyr::filter(is.na(as.numeric(gsub("[^[[:alnum:]]+"
                                          , ""
                                          , !!rlang::ensym(taxa_col)
                                          )
                                     )
                          )
                    , is.na(as.numeric(lubridate::dmy(!!rlang::ensym(taxa_col))))
                    , !is.na(!!rlang::ensym(taxa_col))
                    , !is.na(searched_name)
                    ) # remove names that contain only digits or dates


    taxas <- tibble::tibble(searched_name = setdiff(to_check$searched_name
                                                    , already_done
                                                    )
                            ) %>%
      dplyr::distinct(searched_name) %>%
      dplyr::arrange(searched_name)

    counter <- 1

    # name_backbone--------
    if(length(taxas$searched_name) > 0){

      for (i in taxas$searched_name){

        print(paste0(counter
                     , " of "
                     , nrow(taxas)
                     , " ("
                     , round(100 * counter / nrow(taxas), 1)
                     , "%): "
                     , i
                     )
              )

        tax_gbif <- rgbif::name_backbone(i
                                         , kingdom = king_type
                                         ) %>%
          dplyr::mutate(searched_name = i)

        tax_gbif <- if(sum(grepl("acceptedUsageKey"
                                 , names(tax_gbif)
                                 )
                           ) > 0
                       ) {

          rgbif::name_usage(tax_gbif$acceptedUsageKey)$data %>%
            dplyr::mutate(matchType = "Synonym") %>%
            dplyr::rename(usageKey = key
                          , status = taxonomicStatus
                          ) %>%
            dplyr::mutate(searched_name = i)

          } else {

            tax_gbif

          }

        best <- tax_gbif %>%
          tidyr::pivot_longer(where(is.numeric), names_to = "key") %>%
          dplyr::mutate(key = purrr::map_chr(key
                                             , ~ gsub("Key"
                                                      , ""
                                                      , .
                                                      )
                                             )
                        ) %>%
          dplyr::filter(key %in% lurank$rank) %>%
          dplyr::left_join(lurank
                           , by = c("key" = "rank")
                           ) %>%
          dplyr::filter(sort <= target_sort) %>%
          dplyr::filter(sort == max(sort))

        tax_gbif$taxa <- best %>%
          dplyr::select(taxa = tolower(lurank$rank[lurank$sort == .$sort])) %>%
          dplyr::pull()

        tax_gbif$best_key <- best %>%
          dplyr::select(value) %>%
          dplyr::pull()

        tax_gbif$stamp <- Sys.time()

        tax_gbif <- taxas %>%
          dplyr::inner_join(tax_gbif) %>%
          dplyr::inner_join(to_check)

        if(file.exists(tmp_file)) {

          rio::export(tax_gbif %>%
                        dplyr::bind_rows(rio::import(tmp_file)) %>%
                        dplyr::select(1
                                      , 2
                                      , taxa
                                      , everything()
                                      )
                      , tmp_file
                      )

        } else {

          rio::export(tax_gbif %>%
                        dplyr::select(1
                                      , 2
                                      , taxa
                                      , everything()
                                      )
                      , tmp_file
                      )

        }

        counter <- counter + 1

      }

      # Clean up results
      res$raw <- rio::import(tmp_file) %>%
        {if(!file.exists(out_file)) (.) else (.) %>% dplyr::bind_rows(rio::import(out_file)$raw)} %>%
        dplyr::group_by(original_name) %>%
        dplyr::filter(stamp == max(stamp)) %>%
        dplyr::ungroup()

      rio::export(res, out_file)

      file.remove(tmp_file)

    }

    # res$raw ------
    res$raw <- res$raw %>%
      # hack to ensure any searched_name can also be an 'original_name'
      dplyr::bind_rows((.) %>%
                         tibble::as_tibble() %>%
                         dplyr::mutate(original_name = searched_name) %>%
                         dplyr::distinct()
                       ) %>%
      dplyr::distinct() %>%
      dplyr::mutate(rank = tolower(rank)
                    , rank = factor(rank
                                    , levels = levels(lurank$rank)
                                    , ordered = TRUE
                                    )
                    )

    return_taxonomy <- c("taxa"
                         , rgbif::taxrank()
                         , "best_key"
                         )

    # res$taxonomy-------
    res$taxonomy <- res$raw %>%
      dplyr::distinct(dplyr::across(tidyselect::any_of(return_taxonomy))) %>%
      tibble::as_tibble()

    dups <- res$taxonomy %>%
      dplyr::add_count(taxa) %>%
      dplyr::filter(n > 1) %>%
      dplyr::select(-n)

    keep_dups <- dups %>%
      dplyr::group_by(taxa) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    res$taxonomy <- res$taxonomy %>%
      dplyr::anti_join(dups) %>%
      dplyr::bind_rows(keep_dups)

    # res$lutaxa------
    res$lutaxa <- res$raw %>%
      dplyr::select(taxa
                      , searched_name
                      , original_name
                      , rank
                      ) %>%
      dplyr::distinct() %>%
      tibble::as_tibble()

    # lifespan-----
    if(isTRUE(!is.null(lifespan_col))) {

      spp_lifespan <- df %>%
        dplyr::filter(!is.na(lifespan)) %>%
        dplyr::rename(original_name = !!rlang::ensym(taxa_col)) %>%
        dplyr::left_join(res$lutaxa) %>%
        dplyr::count(taxa,lifespan) %>%
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
        dplyr::count(genus,lifespan) %>%
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
        dplyr::count(family,lifespan) %>%
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
