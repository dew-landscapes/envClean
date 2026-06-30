#' Find how taxa changed through the cleaning/filtering/tidying process
#'
#'
#' @param taxa_to_find Character. Taxa name to find.
#' @param taxa_cols Character. Name of column(s) across data frames containing
#' taxa information.
#' @param lutaxa Dataframe with column names matching `taxa_cols`, usually,
#' say, `taxonomy$species$lutaxa` as a result of `make_taxonomy()`.
#' @param filt_df_prefix Character. Prefix used in each of the data frames
#' created at each step in the filtering process.
#' @param store Character. Path to a targets store. If available, use this as an
#' alternative to ensuring the objects to be found using `filt_df_prefix` are in
#' the global environment. Other than finding by `filt_df_prefix` objects in the
#' store are also filtered by `type == "stem"` and `format == "parquet"`
#'
#' @return
#' @export
#'
#' @example inst/examples/find_taxa_ex.R
find_taxa <- function(taxa_to_find
                      , taxa_cols = c("original_name", "taxa")
                      , context = NULL
                      , lutaxa
                      , filt_df_prefix = "^flor_|^bio_"
                      , store = NULL
                      ) {

  find_string <- paste0(taxa_to_find, collapse = "|")

  to_find <- lutaxa |>
    dplyr::filter(if_any(any_of(taxa_cols)
                         , ~ grepl(find_string, .)
                         )
                  ) |>
    dplyr::select(any_of(taxa_cols)) |>
    as.matrix() |>
    as.vector() |>
    unique()

  if(is.null(store)) {

    dfs <- ls(pattern = filt_df_prefix
              , envir = globalenv()
              ) |>
      tibble::enframe(name = NULL, value = "name") |>
      dplyr::mutate(obj = purrr::map(name, get))


  } else {

    dfs <- targets::tar_meta(store = store) |>
      dplyr::filter(grepl(filt_df_prefix, name)
                    , type == "stem"
                    , format == "parquet"
                    ) |>
      dplyr::select(name) |>
      dplyr::mutate(obj = purrr::map(name
                                     , \(x) arrow::open_dataset(fs::path(store, "objects", x)) |>
                                       dplyr::select(tidyselect::any_of(c(taxa_cols, context))) |>
                                       dplyr::collect()
                                     )
                    )

  }

  dfs |>
    dplyr::mutate(obj = purrr::map(obj
                                   , \(x) x %>%
                                     dplyr::select(tidyselect::any_of(c(taxa_cols, context))) %>%
                                     dplyr::filter(dplyr::if_any(tidyselect::any_of(taxa_cols), \(x) x %in% to_find))
                                   )
                  , taxa = taxa_to_find
                  , records = purrr::map_dbl(obj, base::nrow)
                  , found = purrr::map_chr(obj
                                           , \(x) x |>
                                             dplyr::select(tidyselect::any_of(taxa_cols)) |>
                                             dplyr::distinct() |>
                                             base::unlist() |>
                                             base::unique() |>
                                             envFunc::vec_to_sentence()
                                           )
                  ) |>
    dplyr::arrange(desc(records))

}
