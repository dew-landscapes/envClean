
#' Describte change in taxa, records, visits and sites between cleaning steps
#'
#' @param prefix Character. Prefix used to name objects during the cleaning
#' process
#' @param save_ends Logical. Save the first and last data frames created by the
#' cleaning process?
#' @param save_dir Character. Path to save objects to if `save_ends == TRUE`
#' @param ... Passed to `envClean::rec_vis_sit_tax()`
#'
#' @return
#' @export
#'
#' @examples
cleaning_summary <- function(prefix = "bio_"
                             , save_ends = TRUE
                             , save_dir = "out"
                             , ...
                             ) {

  dots <- list(...)

  eval(substitute(alist(...)))

  all_contexts <- unique(c(unname(unlist(dots))))

  df_text <- ls(pattern = prefix
                , envir = .GlobalEnv
                ) %>%
    tibble::enframe(name = NULL
                    , value = "name"
                    ) %>%
    dplyr::mutate(obj = purrr::map(name
                                   , get
                                   )
                  ) %>%
    dplyr::filter(purrr::map_lgl(obj, is.data.frame)) %>%
    dplyr::filter(purrr::map_lgl(obj, ~ "ctime" %in% names(attributes(.)))) %>%
    dplyr::mutate(ctime = purrr::map(obj
                              , attr
                              , "ctime"
                              )
                  , n = purrr::map_dbl(obj, nrow)
                  ) %>%
    tidyr::unnest(cols = c(ctime)) %>%
    dplyr::arrange(ctime
                   , desc(n)
                   ) %>%
    dplyr::mutate(obj_names = purrr::map(obj, names)
                  , contexts = purrr::map(obj_names, ~ all_contexts[all_contexts %in% .])
                  , contexts_lag = lag(contexts, default = NULL)
                  , contexts_add = purrr::map2_chr(contexts, contexts_lag
                                                   , ~ vec_to_sentence(setdiff(.x, .y))
                                                   )
                  , contexts_lost = purrr::map2_chr(contexts_lag, contexts
                                                    ,  ~ vec_to_sentence(setdiff(.x, .y))
                                                    )
                  , contexts = purrr::map_chr(contexts, vec_to_sentence)
                  , contexts_lag = purrr::map_chr(contexts_lag, vec_to_sentence)
                  ) %>%
    dplyr::mutate(clean = gsub(prefix
                                 , ""
                                 , name
                                 )
                  , summary = purrr::map(obj
                                         , rec_vis_sit_tax
                                         , ...
                                         # , site_cols = dots$site_cols
                                         # , visit_cols = dots$visit_cols
                                         # , taxa_cols = dots$taxa_cols
                                         )
                  ) %>%
    tidyr::unnest(cols = c(summary))

  if(save_ends) {

    to_save <- df_text %>%
      dplyr::slice(1, nrow(.)) %>%
      dplyr::select(name, obj) %>%
      dplyr::mutate(save_path = fs::path(save_dir
                                         , paste0(c("clean_start"
                                                    , "clean_end"
                                                    )
                                                  , ".parquet"
                                                  )
                                         )
                    )

    purrr::walk2(to_save$obj
                 , to_save$save_path
                 , arrow::write_parquet
                 )

  }

  df_text <- df_text %>%
    dplyr::select(! c(where(is.list))) %>%
    dplyr::mutate(row = dplyr::row_number())

  return(df_text)

}

