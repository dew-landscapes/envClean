

#' Get GBIF backbone taxonomy
#'
#' Only queries GBIF for taxa not already in `out_file`.
#'
#' @param df Dataframe with taxa column.
#' @param taxa_col Character. Name of column with taxa names
#' @param out_file Character. Path to save results to.
#' @param do_common Logical. If true, an attempt will be made to find a common
#' name.
#' @param remove_taxa Character. Regular expressions to be matched. These will
#' be filtered before searching.
#' @param remove_strings Character. Regular expressions to be matched. These
#' will be removed from the string before searching.
#' @param ... Arguments passed to `rgbif::name_backbone_checklist()`.
#'
#' @return Dataframe. Results from `envClean::get_gbif_tax()`. Tweaked by column
#' `rank` being lowercase and factor as per `envClean::lurank`.
#' @export
#'
#' @examples
  get_taxonomy <- function(df
                           , taxa_col = "original_name"
                           , out_file = tempfile()
                           , do_common = FALSE
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

    if(file.exists(out_file)) {

      res <- rio::import(out_file)

    } else res <- list()

    lurank <- envClean::lurank

    taxa_col <- names(df[taxa_col])

    if(tools::file_ext(out_file) == "") out_file <- paste0(out_file, ".rds")

    already_done_01 <- if(file.exists(out_file)) res %>%
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

      if(file.exists(out_file)) {

        res <- res %>%
          dplyr::bind_rows(tax_gbif)

      } else {

        res <- tax_gbif

      }

    }

    # common -------
    if(do_common) {

      if("common" %in% names(res)){

        old_common <- res$common %>%
          dplyr::filter(searched)

      }

      new_common <- res %>%
        dplyr::distinct(original_name, usageKey) %>%
        {if(exists("old_common")) (.) %>% dplyr::anti_join(old_common) else (.)} %>%
        dplyr::mutate(common = purrr::map_chr(usageKey
                                              , get_gbif_common
                                              )
                      )

      res <- dplyr::left_join(res
                              , purrr::reduce(mget(ls(pattern = "new_common|old_common"))
                                              , dplyr::bind_rows
                                              )
                              ) %>%
        dplyr::distinct() %>%
        dplyr::mutate(searched = TRUE)

    }

    res <- tibble::as_tibble(res)

    # export -------

    rio::export(res
                , out_file
                )

    return(res)

  }
