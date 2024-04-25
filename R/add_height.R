

#' Generate best guess of height for each taxa*context
#'
#' @param df Dataframe with context, taxa and height columns.
#' @param context Character. Name of columns defining context.
#' @param env_prcomp List. Result from call to `envClean::env_pca()`
#' @param lustr Dataframe. Lookup from `lifeform` to numeric height values
#' @param lustr_col Character. Name of column in lustr containing height
#' values to use for any `lifeform`
#'
#' @return Dataframe with cov_col removed and replaced with best guess height in
#' column use_height
#' @export
#'
#' @examples
  add_height <- function(df
                        , context = "cell"
                        , env_prcomp
                        , lustr
                        , lustr_col = "ht"
                        ) {

    site_use_height <- df %>%
      dplyr::left_join(lustr) %>%
      dplyr::mutate(site_use_height = dplyr::case_when(if("height" %in% names(df)) !is.na(height) ~ height
                                                       , !is.na(lifeform) ~ !!rlang::ensym(lustr_col)
                                                       , TRUE ~ NA
                                                       )
                    ) %>%
      dplyr::filter(!is.na(site_use_height)
                    , site_use_height != 0
                    ) %>%
      dplyr::group_by(taxa
                      , dplyr::across(tidyselect::any_of(context))
                      ) %>%
      dplyr::summarise(site_use_height = median(site_use_height)) %>%
      dplyr::ungroup()

    pca_use_height <- df %>%
      dplyr::left_join(env_prcomp$pca_res_cell_cut) %>%
      dplyr::distinct(taxa
                      , dplyr::across(tidyselect::contains("cut_pc"))
                      , dplyr::across(tidyselect::any_of(context))
                      ) %>%
      dplyr::left_join(site_use_height %>%
                         dplyr::left_join(env_prcomp$pca_res_cell_cut) %>%
                         dplyr::group_by(taxa
                                         , dplyr::across(tidyselect::contains("cut_pc"))
                                         ) %>%
                         dplyr::summarise(pca_use_height = median(site_use_height
                                                                 , na.rm = TRUE
                                                                 )
                                          ) %>%
                         dplyr::ungroup()
                       )

    taxa_use_height <- site_use_height %>%
      dplyr::group_by(taxa) %>%
      dplyr::summarise(taxa_use_height = median(site_use_height
                                               , na.rm = TRUE
                                               )
                       ) %>%
      dplyr::ungroup()

    result <- df %>%
      dplyr::left_join(site_use_height) %>%
      dplyr::left_join(pca_use_height) %>%
      dplyr::left_join(taxa_use_height) %>%
      dplyr::mutate(use_height = dplyr::case_when(!is.na(site_use_height) ~ site_use_height
                                                 , !is.na(pca_use_height) ~ pca_use_height
                                                 , !is.na(taxa_use_height) ~ taxa_use_height
                                                 , TRUE ~ NA
                                                 )
                    ) %>%
      dplyr::select(tidyselect::all_of(names(df))
                    , tidyselect::contains("cut_pc")
                    , use_height
                    ) %>%
      dplyr::distinct(taxa
                      , dplyr::across(tidyselect::any_of(context))
                      , dplyr::across(tidyselect::contains("cut_pc"))
                      , use_height
                      )

    return(result)

  }
