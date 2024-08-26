#' Find how taxa changed through the cleaning/filtering/tidying process
#'
#' `find_taxa` does not work hierarchically. For example _Eucalyptus_ will only
#' match genus level records, not species records, such as
#' _Eucalyptus leucoxylon_.
#'
#' @param taxa_to_find Character. Taxa name to find.
#' @param taxa_cols Character. Name of column(s) across data frames containing
#' taxa information.
#' @param lutaxa Dataframe with column names matching `taxa_cols`, usually,
#' say, `taxonomy$species$lutaxa` as a result of `make_taxonomy()`.
#' @param filt_df_prefix Character. Prefix used in each of the data frames
#' created at each step in the filtering process.
#'
#' @return
#' @export
#'
#' @example inst/examples/find_taxa_ex.R
find_taxa <- function(taxa_to_find
                      , taxa_cols = c("original_name", "taxa")
                      , lutaxa
                      , filt_df_prefix = "flor_"
                      ) {

  cols_out <- c("name", "taxas")

  find_string <- paste0(taxa_to_find, collapse = "|")

  to_find <- lutaxa %>%
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
    dplyr::filter(purrr::map_lgl(obj, \(x) ! "sf" %in% class(x))) %>%
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
    dplyr::mutate(founds = purrr::map(obj
                                      , \(x) x %>%
                                        dplyr::select(tidyselect::any_of(taxa_cols)) %>%
                                        dplyr::filter(dplyr::if_any(tidyselect::any_of(taxa_cols), \(x) x %in% to_find))
                                      )
                  , taxa = taxa_to_find
                  , records = purrr::map_dbl(founds, base::nrow)
                  , found = purrr::map_chr(founds
                                           , \(x) x %>%
                                             dplyr::distinct() %>%
                                             base::unlist() %>%
                                             base::unique() %>%
                                             envFunc::vec_to_sentence()
                                           )
                  ) %>%
    dplyr::arrange(desc(nrow), ctime)

}
