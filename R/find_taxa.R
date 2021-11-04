#' Find how taxa changed through the cleaning/filtering/tidying process
#'
#' `find_taxa` does not work hierarchically. For example _Eucalyptus_ will only
#' match genus level records, not species records, such as
#' _Eucalyptus leucoxylon_.
#'
#' @param taxa Character. Taxa name to find.
#' @param taxa_cols Character. Name of column(s) across data frames containing
#' taxa information.
#' @param lookup_taxa Dataframe with columns names matching `taxa_cols`.
#' @param filt_df_prefix Character. Prefix used in each of the data frames
#' created at each step in the filtering process.
#'
#' @return
#' @export
#'
#' @example examples/find_taxa.R
find_taxa <- function(taxa
                      , taxa_cols = c("original_name", "taxa")
                      , lookup_taxa = lutaxa
                      , filt_df_prefix = "flor_"
                      ) {

  cols_out <- c("name", "taxas")

  find_string <- paste0(taxa, collapse = "|")

  to_find <- lookup_taxa %>%
    dplyr::filter(if_any(any_of(taxa_cols)
                         , ~ grepl(find_string, .)
                         )
                  ) %>%
    dplyr::select(any_of(taxa_cols)) %>%
    as.matrix() %>%
    as.vector() %>%
    unique()

  ls(pattern = filt_df_prefix
     , envir = globalenv()
     ) %>%
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
                                            )
                             )
                  , summary = purrr::map_chr(obj
                                  , ~paste0(taxa
                                           , " has "
                                           , sum(.$n)
                                           , " records"
                                           )
                                  )
                  ) %>%
    dplyr::arrange(desc(nrow), ctime)

}
