

#' Get GBIF backbone taxonomy
#'
#' Only queries GBIF for taxa not already in `taxonomy_file`.
#'
#' Common (vernacularName) no longer supported here. Use `get_gbif_common()` on
#' a downstream result. It may be helpful to keep a usageKey through the
#' cleaning process for use in getting common names. Part of the reason for
#' removing that functionality here was the ambiguity of which key to use,
#' particularly around species vs subspecies.
#'
#' @param df Dataframe with taxa column.
#' @param taxa_col Character. Name of column with taxa names
#' @param taxonomy_file Character. Path to save results to.
#' @param remove_taxa Character. Regular expressions to be matched. These will
#' be filtered before searching.
#' @param remove_strings Character. Regular expressions to be matched. These
#' will be removed from the string before searching.
#' @param ... Arguments passed to `rgbif::name_backbone_checklist()`.
#'
#' @return Dataframe. Results from `envClean::get_gbif_tax()`. Tweaked by column
#' `rank` being lowercase and ordered factor as per `envClean::lurank`. Writes
#' `taxonomy_file` and `gsub("\\.", "_accepted.", taxonomy_file)`
#' @export
#'
#' @examples
  get_taxonomy <- function(df
                           , taxa_col = "original_name"
                           , taxonomy_file = tempfile()
                           , remove_taxa = c("BOLD:"
                                             , "dead"
                                             , "unverified"
                                             , "annual herb"
                                             , "annual grass"
                                             , "\\?"
                                             )
                           , remove_strings = c("\\sx\\s.*|\\sX\\s.*")
                           , ...
                           ) {

    if(file.exists(taxonomy_file)) {

      res <- rio::import(taxonomy_file
                         , setclass = "tibble"
                         )

    } else res <- list()

    lurank <- envClean::lurank

    taxa_col <- names(df[taxa_col])

    if(tools::file_ext(taxonomy_file) == "") taxonomy_file <- paste0(taxonomy_file, ".rds")

    already_done_01 <- if(file.exists(taxonomy_file)) res %>%
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
        dplyr::mutate(stamp = Sys.time()) %>%
        dplyr::mutate(rank = tolower(rank)
                      , rank = factor(rank
                                      , levels = levels(envClean::lurank$rank)
                                      , ordered = TRUE
                                      )
                      )

      if(file.exists(taxonomy_file)) {

        res <- res %>%
          dplyr::bind_rows(tax_gbif)

      } else {

        res <- tax_gbif

      }

      # export -------

      res <- tibble::as_tibble(res) %>%
        dplyr::group_by(original_name) %>%
        dplyr::filter(stamp == max(stamp)) %>%
        dplyr::ungroup()

      rio::export(res
                  , taxonomy_file
                  )

    }

    # best key------
    # build taxonomy lookup for any key

    all_accepted_keys <- res %>%
      dplyr::filter(status == "ACCEPTED") %>%
      dplyr::select(tidyselect::contains("Key")) %>%
      unlist() %>%
      unname() %>%
      na.omit() %>%
      unique() %>%
      sort()

    # missing keys (full taxonomy)------

    accepted_tax_file <- fs::path(gsub("\\."
                                   , "_accepted."
                                   , taxonomy_file
                                   )
                              )

    if(file.exists(accepted_tax_file)) {

      accepted_tax <- rio::import(accepted_tax_file
                                  , setclass = "tibble"
                                  )

    } else accepted_tax <- tibble::tibble(usageKey = NA)

    missing_key <- setdiff(all_accepted_keys
                           , unique(accepted_tax$usageKey)
                           )

    if(length(missing_key)) {

      chunks <- split(missing_key
                      , ceiling(seq_along(missing_key) / 100)
                      )

      for(i in chunks) {

          accepted_tax <- purrr::map_df(i
                                        , ~ rgbif::name_usage(.)$data
                                        ) %>%
            dplyr::rename(usageKey = key) %>%
            dplyr::select(tidyselect::any_of(names(res))) %>%
            dplyr::mutate(rank = tolower(rank)
                          , rank = factor(rank
                                          , levels = levels(envClean::lurank$rank)
                                          , ordered = TRUE
                                          )
                          ) %>%
            dplyr::bind_rows(accepted_tax) %>%
            dplyr::filter(!is.na(usageKey)) %>%
            dplyr::arrange(usageKey)

        rio::export(accepted_tax
                    , accepted_tax_file
                    )

      }

    }

    return(res)

  }
