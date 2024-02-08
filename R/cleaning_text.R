
#' Write a sentence describing change in taxa, records, visits and sites between
#' two cleaning steps
#'
#' @param cleaning_summary Dataframe. Result of call to
#' `envClean::cleaning_summary()`
#'
#' @return Dataframe with added columns that can be used in .Rmd to report on
#' the cleaning process.
#' @export
#'
#' @examples
cleaning_text <- function(cleaning_summary) {

  format_big <- function(number) {

    format(number, big.mark = ",")

  }

  make_text <- function(row, taxa, records, visits, sites, desc
                        , lag_taxa, lag_records, lag_visits, lag_sites
                        , step
                        ) {

    text <- if(row == 1) {

      paste0("The initial data set contained "
             , taxa %>% format_big
             , " taxa, "
             , records %>% format_big
             , " records, "
             , visits %>% format_big
             , " visits, and "
             , sites %>% format_big
             , " sites."
             )

    } else if(row != 1) {

      if(all(taxa == lag_taxa, records == lag_records, visits == lag_visits, sites == lag_sites)) {

        paste0("The cleaning step '"
               , desc
               , " did not change the number of taxa ("
               , taxa %>% format_big
               , "), records ("
               , records %>% format_big
               , "), visits ("
               , visits %>% format_big
               , ") or sites ("
               , sites %>% format_big
               , ")."
               )

      } else {

      make_text <- function(trvs, lag, now, step) {

        if(lag > now) {

          paste0("took the number of "
                 , trvs
                 , " from "
                 , lag %>% format_big
                 , " to "
                 , now %>% format_big
                 )

        } else if (lag < now) {

          paste0("took the number of "
                 , trvs
                 , " from "
                 , lag %>% format_big
                 , " to "
                 , now %>% format_big
                 , if(step == "bio_lists") {

                   " (an increase was possible due to the addition of pseudo-absences)"

                 } else {

                   " (an increase was possible due to the addition of further context during this step in the cleaning process)"

                 }

                 )

        } else {

          paste0("did not change the number of "
                 , trvs
                 , " ("
                 , now %>% format_big
                 , ")"
                 )

        }

      }

      # make text result
      paste0("The cleaning step '"
             , desc
             , "': "
             , make_text("taxa"
                         , lag_taxa
                         , taxa
                         , step
                         )
             , "; "
             , make_text("records"
                         , lag_records
                         , records
                         , step
                         )
             , "; "
             , make_text("visits"
                         , lag_visits
                         , visits
                         , step
                         )
             , "; and "
             , make_text("sites"
                         , lag_sites
                         , sites
                         , step
                         )
             , "."
             )

        }

    }

  }

  make_context_text <- function(row
                                , contexts
                                , contexts_lag
                                , contexts_add
                                , contexts_lost
                                ) {

    if(row == 1) {

      text <- paste0("At the start of the cleaning process context was defined by "
                     , contexts
                     , "."
                     )

    } else {

      text <- paste0("At this stage of the cleaning process context was defined by "
                     , contexts
                     , "."
                     , if(contexts_add != "") " "
                     , if(contexts_add != "") {
                       paste0(contexts_add
                              , if(grepl("and", contexts_add)) " were" else " was"
                              , " added"
                              , if(contexts_lost != "") " and " else "."
                              )
                     }
                     , if(contexts_add == "" & contexts_lost != "") " "
                     , if(contexts_lost != "") {
                       paste0(contexts_lost
                              , if(grepl("and", contexts_lost)) " were" else " was"
                              , " removed."
                              )
                       }
                     )

    }

  }

  df_text <- clean_summary %>%
    dplyr::mutate(context_text = purrr::pmap_chr(list(row
                                                      , contexts
                                                      , contexts_lag
                                                      , contexts_add
                                                      , contexts_lost
                                                      )
                                                 , make_context_text
                                                 )
                  ) %>%
    dplyr::left_join(luclean) %>%
    dplyr::mutate(dplyr::across(where(is.numeric)
                                  , lag
                                  , .names = "lag_{.col}"
                                  )
                  , text = purrr::pmap_chr(.l = list(row = .data$row
                                                     , taxa = .data$taxa
                                                     , records = .data$records
                                                     , visits = .data$visits
                                                     , sites = .data$sites
                                                     , desc = .data$desc
                                                     , lag_taxa = .data$lag_taxa
                                                     , lag_records = .data$lag_records
                                                     , lag_visits = .data$lag_visits
                                                     , lag_sites = .data$lag_sites
                                                     , step = .data$name
                                                     )
                                           , .f = make_text
                                           )
                  , childID = gsub("[^[:alnum:]]", "", name)
                  ) %>%
    dplyr::select(! c(where(is.list)
                      , tidyselect::matches("^lag_|^text_")
                      )
                  )


  return(df_text)

}

