

#' Make a single (numeric, proportion) cover column from different sorts of
#' input cover
#'
#' Assumes a numeric (percentage) cover column called 'cover' and character
#' column called 'cover_code' that is the modified Braun-Blanquet
#' [@RN4265] cover value using
#' [Biological Databases of South Australia](https://www.environment.sa.gov.au/topics/Science/Information_data/Biological_databases_of_South_Australia)
#' codes (`COVCODE` field in BDBSA). Example of `lucov` at
#' `envEcosystems::lucover`.
#'
#' @param df Dataframe containing columns to consolidate.
#' @param taxa_col Character. Name of column containing taxa.
#' @param context Character. Name of columns defining the context.
#' @param lucov Dataframe. Lookup from `cover_code` to numeric cover values.
#' @param cov_type Character. Name of column in `lucov` that is used to convert
#' character `cover_code` to numeric values.
#'
#' @return Dataframe with consolidated `use_cover` column.
#' @references
#'   \insertAllCited{}
#' @export
#'
#' @examples
  make_cover <- function(df
                         , taxa_col = "taxa"
                         , context = NULL
                         , lucov
                         , cov_type = "cover_mid"
                         ) {


    suppressWarnings({

      df %>%
        dplyr::filter(!is.na(cover) | !is.na(cover_code)) %>%
        dplyr::mutate(cover = ifelse(cover <= 0, NA, cover)
                      , cover = ifelse(cover > 100, NA, cover)
                      , cover = ifelse(cover > 1, cover / 100, cover)
                      ) %>%
        dplyr::filter(!is.na(cover) | !is.na(cover_code)) %>%
        dplyr::left_join(lucov) %>%
        dplyr::mutate(use_cover = dplyr::if_else(!is.na(cover)
                                                 , cover
                                                 , !!rlang::ensym(cov_type)
                                                 )
                      ) %>%
        dplyr::group_by(dplyr::across(tidyselect::any_of(context))
                        , dplyr::across(!!rlang::ensym(taxa_col))
                        ) %>%
        dplyr::summarise(use_cover = max(use_cover, na.rm = TRUE)
                         , use_cover = ifelse(is.infinite(use_cover)
                                              , NA
                                              , use_cover
                                              )
                         ) %>%
        dplyr::ungroup()

    })

  }

