

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
                           , remove_strings = c("\\sx\\s.*|\\sX\\s.*")
                           , ...
                           ) {

    lurank <- envClean::lurank

    taxa_col <- names(df[taxa_col])

    if(tools::file_ext(taxonomy_file) == "") taxonomy_file <- paste0(taxonomy_file, ".rds")

    # If taxonomy_file already exists, bring it in then remove any force_new
    if(file.exists(taxonomy_file)) {

      res <- rio::import(taxonomy_file
                         , setclass = "tibble"
                         )

      if(!is.null(force_new$timediff)) {

        res <- res %>%
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

        res <- res %>%
          dplyr::filter(!(!!rlang::ensym(taxa_col) %in% force_new[taxa_col]))

      }

    } else {

      res <- tibble::tibble(!!rlang::ensym(taxa_col) := NA
                            , usageKey = NA
                            )

    }

    # Collect any unfound original_name to check in gbif (and make a 'searched' name)
    to_check <- unique(df[taxa_col]) %>%
      dplyr::filter(!grepl(paste0(remove_taxa
                                  , collapse = "|"
                                  )
                           , !!rlang::ensym(taxa_col)
                           )
                    , !!rlang::ensym(taxa_col) != ""
                    ) %>%
      dplyr::anti_join(res) %>%
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


    # name_backbone--------
    if(length(to_check$searched_name)){

      tax_gbif <- to_check %>%
        dplyr::bind_cols(rgbif::name_backbone_checklist(to_check %>% dplyr::select(name = searched_name))) %>%
        dplyr::mutate(stamp = Sys.time()) %>%
        dplyr::mutate(rank = tolower(rank)
                      , rank = factor(rank
                                      , levels = levels(envClean::lurank$rank)
                                      , ordered = TRUE
                                      )
                      )

      res <- res %>%
        dplyr::bind_rows(tax_gbif)

    }

    # export -------

    res <- res %>%
      dplyr::filter(!is.na(original_name)) %>%
      dplyr::group_by(original_name) %>%
      dplyr::filter(stamp == max(stamp)) %>%
      dplyr::ungroup()

    rio::export(res
                , taxonomy_file
                )

    # best key------
    # build taxonomy lookup for any key

    not_accepted_keys <- res %>%
      dplyr::filter(status != "ACCEPTED") %>%
      dplyr::select(usageKey) %>%
      unlist() %>%
      unname() %>%
      na.omit() %>%
      unique() %>%
      sort()

    all_accepted_keys <- res %>%
      dplyr::filter(status == "ACCEPTED") %>%
      dplyr::select(tidyselect::contains("Key")) %>%
      unlist() %>%
      unname() %>%
      na.omit() %>%
      unique() %>%
      sort()

    all_accepted_keys <- all_accepted_keys[!all_accepted_keys %in% not_accepted_keys]

    # missing keys (full taxonomy)------

    accepted_tax_file <- fs::path(gsub("\\."
                                   , "_accepted."
                                   , taxonomy_file
                                   )
                              )

    if(file.exists(accepted_tax_file)) {

      accepted_tax <- rio::import(accepted_tax_file
                                  , setclass = "tibble"
                                  ) %>%
        dplyr::filter(!usageKey %in% not_accepted_keys)

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
