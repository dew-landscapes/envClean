


#' Generate best guess of lifeform for each taxa*context
#'
#' @param df Dataframe with context, taxa and lifeform columns.
#' @param context Character. Name of columns defining context.
#' @param env_prcomp List. Result from call to `envClean::env_pca()`
#' @param lulife Dataframe lookup for lifeform.
#'
#' @return Dataframe with best guess lifeform replacing original lifeform.
#' @export
#'
#' @examples
  add_lifeform <- function(df
                           , context = "cell"
                           , env_prcomp
                           , lulife
                           ) {

    site_lifeform <- df %>%
      dplyr::filter(lifeform %in% lulife$lifeform) %>%
      dplyr::count(taxa
                   , lifeform
                   , dplyr::across(tidyselect::any_of(context))
                   ) %>%
      dplyr::left_join(lulife %>%
                         dplyr::select(lifeform, sort)
                       ) %>%
      dplyr::group_by(taxa
                      , dplyr::across(tidyselect::any_of(context))
                      ) %>%
      dplyr::filter(n == max(n)) %>%
      dplyr::filter(sort == min(sort)) %>%
      dplyr::ungroup() %>%
      dplyr::select(taxa
                    , site_lifeform = lifeform
                    , tidyselect::any_of(context)
                    )


    pca_lifeform <- df %>%
      dplyr::left_join(env_prcomp$pca_res_cell_cut) %>%
      dplyr::left_join(df %>%
                         dplyr::filter(!is.na(lifeform)) %>%
                         dplyr::left_join(env_prcomp$pca_res_cell_cut) %>%
                         dplyr::count(taxa
                                      , lifeform
                                      , dplyr::across(tidyselect::contains("cut_pc"))
                                      ) %>%
                         dplyr::left_join(lulife %>%
                                            dplyr::select(lifeform, sort)
                                          ) %>%
                         dplyr::group_by(taxa, dplyr::across(tidyselect::contains("cut_pc"))) %>%
                         dplyr::filter(n == max(n)) %>%
                         dplyr::filter(sort == min(sort)) %>%
                         dplyr::ungroup() %>%
                         dplyr::select(taxa
                                       , tidyselect::contains("cut_pc")
                                       , pca_lifeform = lifeform
                                       ) %>%
                         dplyr::distinct()
                       ) %>%
      dplyr::select(taxa
                    , pca_lifeform
                    , tidyselect::contains("cut_pc")
                    , tidyselect::any_of(context)
                    )

    taxa_lifeform <- df %>%
      dplyr::distinct(taxa
                      , dplyr::across(tidyselect::any_of(context))
                      ) %>%
      dplyr::left_join(df %>%
                         dplyr::filter(!is.na(lifeform)) %>%
                         dplyr::inner_join(lulife) %>%
                         dplyr::count(taxa, sort, lifeform) %>%
                         dplyr::group_by(taxa) %>%
                         dplyr::filter(n == max(n)) %>%
                         dplyr::filter(sort == min(sort)) %>%
                         dplyr::ungroup() %>%
                         dplyr::select(-n, -sort) %>%
                         dplyr::rename(taxa_lifeform = lifeform)
                       )

    result <- taxa_lifeform %>%
      dplyr::left_join(pca_lifeform) %>%
      dplyr::left_join(site_lifeform) %>%
      dplyr::mutate(lifeform = dplyr::case_when(!is.na(site_lifeform) ~ site_lifeform
                                                , !is.na(pca_lifeform) ~ pca_lifeform
                                                , !is.na(taxa_lifeform) ~ taxa_lifeform
                                                , TRUE ~ NA_character_
                                                )
                    ) %>%
      dplyr::distinct(dplyr::across(tidyselect::any_of(names(df))))

    return(result)

  }

