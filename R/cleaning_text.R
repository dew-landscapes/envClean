
#' Write a sentence describing change in taxa, records, visits and sites between
#' two cleaning steps
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
cleaning_text <- function(prefix = "bio_"
                          , save_ends = TRUE
                          , save_dir = "out"
                          , ...
                          ) {

  dots <- list(...)

  eval(substitute(alist(...)))

  test_func_01 <- function(obj_name) {

    "data.frame" %in% class(get(obj_name))

  }

  test_func_02 <- function(obj_name) {

    "ctime" %in% names(attributes(get(obj_name)))

  }

  make_text <- function(row, taxa, records, visits, sites, desc
                        , lag_taxa, lag_records, lag_visits, lag_sites
                        ) {

    text <- if(row == 1) {

      paste0("The initial data set contained "
             , taxa
             , " taxa, "
             , records
             , " records, "
             , visits
             , " visits, and "
             , sites
             , " sites."
             )

    } else if(row != 1) {

      if(records == lag_records) {

        "This step in the cleaning process did not remove any taxa, records, visits or sites."

      } else {

      make_text <- function(trvs, lag, now) {

        if(lag > now) {

          paste0("number of "
                 , trvs
                 , " from "
                 , lag
                 , " to "
                 , now
                 )

        } else if (lag < now) {

          paste0("number of "
                 , trvs
                 , " from "
                 , lag
                 , " to "
                 , now
                 , ". An increase was possible due to the addition of further context during this step in the cleaning process."
                 )

        } else {

          paste0("did not change the number of "
                 , trvs
                 , "("
                 , now
                 , ")"
                 )

        }

      }

      # make text result
      paste0("The cleaning step '"
             , desc
             , "' took the "
             , make_text("taxa"
                         , lag_taxa
                         , taxa
                         )
             , ", "
             , make_text("records"
                         , lag_records
                         , records
                         )
             , ", "
             , make_text("visits"
                         , lag_visits
                         , visits
                         )
             , ", "
             , make_text("sites"
                         , lag_sites
                         , sites
                         )
             , "."
             )

        }

    }

  }

  make_context_text <- function(df
                                , lag_df
                                , contexts
                                ) {

    df_con <- contexts[contexts %in% names(df)]

    df_con_lag <- contexts[contexts %in% names(lag_df)]

    con_added <- setdiff(df_con, df_con_lag)
    con_lost <- setdiff(df_con_lag, df_con)

    if(is.null(lag_df)) {

      text <- paste0("At the start of the cleaning process context was defined by "
                     , envFunc::vec_to_sentence(paste0("`"
                                                       , df_con
                                                       , sep = "`"
                                                       )
                                                )
                     )

    } else {

      text <- paste0("At this stage of the cleaning process context was defined by "
                     , envFunc::vec_to_sentence(paste0("`"
                                                       , df_con
                                                       , sep = "`"
                                                       )
                                                )
                     , "."
                     , if(length(con_added) > 0) " "
                     , if(length(con_added) > 0) {
                       paste0(envFunc::vec_to_sentence(paste0("`"
                                                              , con_added
                                                              , sep = "`"
                                                              )
                                                       )
                              , if(length(con_added) > 1) " were" else " was"
                              , " added"
                              , if(length(con_lost) > 0) " and " else "."
                              )
                     }
                     , if(length(con_added) == 0 & length(con_lost) > 0) " "
                     , if(length(con_lost) > 0) {
                       paste0(envFunc::vec_to_sentence(paste0("`"
                                                              , con_lost
                                                              , sep = "`"
                                                              )
                                                       )
                              , if(length(con_lost) > 1) " were" else " was"
                              , " removed."
                              )
                       }
                     )

    }

  }

  df <- ls(pattern = prefix
           , envir = .GlobalEnv
           ) %>%
    tibble::enframe(name = NULL
                    , value = "name"
                    ) %>%
    envFunc::filter_test_func(test_col = "name"
                     , test_func = test_func_01
                     ) %>%
    envFunc::filter_test_func(test_col = "name"
                     , test_func = test_func_02
                     ) %>%
    dplyr::mutate(obj = purrr::map(name
                            , get
                            )
                  , ctime = purrr::map(obj
                              , attr
                              , "ctime"
                              )
                  , n = purrr::map_dbl(obj, nrow)
                  ) %>%
    tidyr::unnest(cols = c(ctime)) %>%
    dplyr::arrange(ctime
                   , desc(n)
                   ) %>%
    dplyr::mutate(context_text = purrr::map2(obj
                                             , lag(obj)
                                             , make_context_text
                                             , contexts = unique(c(dots$site_cols
                                                                   , dots$visit_cols
                                                                   )
                                                                 )
                                             )
                  , clean = gsub(prefix
                               , ""
                               , name
                               )
                  , summary = purrr::map(obj
                                         , rec_vis_sit_tax
                                         , ...
                                         # , site_cols = site_cols
                                         # , visit_cols = visit_cols
                                         # , taxa_cols = taxa_cols
                                         )
                  ) %>%
    tidyr::unnest(cols = c(context_text, summary)) %>%
    dplyr::left_join(luclean) %>%
    dplyr::mutate(row = dplyr::row_number()
                  , dplyr::across(where(is.numeric)
                                    , ~ format(.x
                                               , big.mark = ","
                                               , trim = TRUE
                                               )
                                  , .names = "text_{.col}"
                                  )
                  , dplyr::across(where(is.numeric)
                                  , ~ format(lag(.x)
                                             , big.mark = ","
                                             , trim = TRUE
                                             )
                                  , .names = "lag_{.col}"
                                  )
                  , text = purrr::pmap_chr(.l = list(row = .data$text_row
                                                 , taxa = .data$text_taxa
                                                 , records = .data$text_records
                                                 , visits = .data$text_visits
                                                 , sites = .data$text_sites
                                                 , desc = .data$desc
                                                 , lag_taxa = .data$lag_taxa
                                                 , lag_records = .data$lag_records
                                                 , lag_visits = .data$lag_visits
                                                 , lag_sites = .data$lag_sites
                                                 )
                                       , .f = make_text
                                       )
                  , childID = gsub("[^[:alnum:]]", "", name)
                  )

  if(save_ends) {

    to_save <- df %>%
      dplyr::slice(1, nrow(.)) %>%
      dplyr::select(name, obj) %>%
      dplyr::mutate(save_path = fs::path(save_dir
                                         , paste0(c("clean_start"
                                                    , "clean_end"
                                                    )
                                                  , ".rds"
                                                  )
                                         )
                    )

    purrr::walk2(to_save$obj
                 , to_save$save_path
                 , rio::export
                 )

  }

  df <- df %>%
    dplyr::select(! c(where(is.list)
                      , tidyselect::matches("^lag_|^text_")
                      )
                  )


  return(df)

}

