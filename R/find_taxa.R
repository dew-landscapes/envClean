#' Find how taxa changed through the cleaning/filtering/tidying process
#'
#' @param taxa_to_find Character. Taxa name to find.
#' @param taxa_cols Character. Name of column(s) across data frames containing
#' taxa information.
#' @param lookup_taxa Dataframe with columns names matching `taxa_cols`.
#'
#' @return
#' @export
#'
#' @examples
find_taxa <- function(taxa_to_find
                      , taxa_cols = c("original_name", "taxa")
                      , lookup_taxa = lutaxa
                      ) {

  cols_out <- c("name", "taxas")

  to_find <- character()

  for(i in taxa_cols) {

    to_find <- c(to_find
                , unname(grep(paste0(taxa_to_find
                                  , collapse = "|"
                                  )
                              , unlist(lookup_taxa[i])
                              , value = TRUE
                              )
                         )
                )

  }

  ls(pattern = "flor_") %>%
    tibble::enframe(name = NULL, value = "name") %>%
    dplyr::mutate(obj = purrr::map(name
                                   , get
                                   )
                  , has_stamp = purrr::map_lgl(obj
                                               , ~"ctime" %in% names(attributes(.))
                                               )
                  ) %>%
    dplyr::filter(has_stamp) %>%
    dplyr::mutate(nrow = purrr::map_dbl(obj
                                        , nrow
                                        )
                  ) %>%
    dplyr::mutate(ctime = purrr::map(obj
                                     , attr
                                     , "ctime"
                                     )
                  ) %>%
    tidyr::unnest(cols = c(ctime)) %>%
    dplyr::mutate(obj = purrr::map(obj
                             , . %>%
                               stats::setNames(gsub(paste0(taxa_cols
                                                    , collapse = "|"
                                                    )
                                             , "taxa"
                                             , names(.)
                                             )
                                        ) %>%
                               dplyr::filter(grepl(paste0(to_find
                                                          , collapse = "|"
                                                          )
                                                   , taxa
                                                   )
                                             ) %>%
                               dplyr::count(taxa
                                            , name = "n"
                                            ) %>%
                               dplyr::summarise(taxas = paste0(taxa
                                                               , " ("
                                                               , n
                                                               , " records)"
                                                               , collapse = ", "
                                                               )
                                                )
                             )
                  ) %>%
    tidyr::unnest(cols = c(obj)
                  , keep_empty = TRUE
                  ) %>%
    dplyr::arrange(desc(nrow),ctime) %>%
    dplyr::select(any_of(cols_out))

}
