

#' Generate best guess of cover for each taxa*context
#'
#' @param df Dataframe with context, taxa and cover columns.
#' @param context Character. Name of columns defining context.
#' @param env_prcomp List. Result from call to `envClean::env_pca()`
#' @param lucover Dataframe. Lookup from `cover_code` to numeric cover values
#' @param lucover_col Character. Name of column in lucover containing cover
#' values to use for any `cover_code`
#' @param small_cov A small cover value assigned to any record for which there
#' is no available site, pca or taxa cover value.
#' @param remove_all_small Logical. If `TRUE` (default) context(s) where all
#' taxa are assigned `small_cov` are removed.
#' @param fix_1to100 Logical. Any values found in `cover` field of `df` that are
#' between above 1 and less than or equal to 100 are divided by 100.
#' @param cov_func Function to summarise cover values for any context with more
#' than one cover value.
#'
#' @return Dataframe with taxa, context and the following 'cover' columns:
#' `cover_adj` a best guess proportion cover; `use_cover_code` the
#' `cover_code` values from `lucover` that correspond to the best guess value;
#' `use_cover` the `lucover_col` column that corresponds to the
#' best guess value.
#' @export
#'
#' @examples
add_cover <- function(df
                      , context = "cell"
                      , env_prcomp
                      , lucover
                      , lucover_col = "cover_mid"
                      , small_cov = 0.009
                      , remove_all_small = TRUE
                      , fix_1to100 = TRUE
                      , cov_func = median
                      ) {

  lucover_col <- as.character(lucover_col)[[1]]

  site_use_cover <- df %>%
    dplyr::filter(dplyr::if_any(tidyselect::contains("cover")
                                , \(x) !is.na(x)
                                )
                  ) %>%
    {if(fix_1to100) (.) %>% dplyr::mutate(cover = dplyr::case_when(cover > 1 & cover <= 100 ~ cover / 100
                                                                   , cover > 100 ~ NA
                                                                   , TRUE ~ cover
                                                                   )
                                          ) else (.)} %>%
    dplyr::left_join(lucover) %>%
    dplyr::mutate(site_use_cover = dplyr::case_when(!is.na(cover_code) ~ cover_mid
                                                    , !is.na(cover) ~ cover
                                                    , TRUE ~ NA
                                                    )
                  ) %>%
    dplyr::filter(!is.na(site_use_cover)
                  , site_use_cover != 0
                  ) %>%
    dplyr::group_by(taxa
                    , dplyr::across(tidyselect::any_of(context))
                    ) %>%
    dplyr::summarise(site_use_cover = cov_func(site_use_cover)) %>%
    dplyr::ungroup()

  pca_use_cover <- df %>%
    dplyr::left_join(env_prcomp$pca_res_cell_cut) %>%
    dplyr::distinct(taxa
                    , dplyr::across(tidyselect::contains("cut_pc"))
                    , dplyr::across(tidyselect::any_of(context))
                    ) %>%
    dplyr::left_join(site_use_cover %>%
                       dplyr::left_join(env_prcomp$pca_res_cell_cut) %>%
                       dplyr::group_by(taxa
                                       , dplyr::across(tidyselect::contains("cut_pc"))
                                       ) %>%
                       dplyr::summarise(pca_use_cover = cov_func(site_use_cover
                                                                 , na.rm = TRUE
                                                                 )
                                        ) %>%
                       dplyr::ungroup()
                     )

  taxa_use_cover <- site_use_cover %>%
    dplyr::group_by(taxa) %>%
    dplyr::summarise(taxa_use_cover = cov_func(site_use_cover
                                             , na.rm = TRUE
                                             )
                     ) %>%
    dplyr::ungroup()

  result <- df %>%
    dplyr::left_join(site_use_cover) %>%
    dplyr::left_join(pca_use_cover) %>%
    dplyr::left_join(taxa_use_cover) %>%
    dplyr::mutate(cover_adj = dplyr::case_when(!is.na(site_use_cover) ~ site_use_cover
                                               , !is.na(pca_use_cover) ~ pca_use_cover
                                               , !is.na(taxa_use_cover) ~ taxa_use_cover
                                               , TRUE ~ small_cov
                                               )
                  ) %>%
    dplyr::select(tidyselect::all_of(names(df))
                  , tidyselect::contains("cut_pc")
                  , cover_adj
                  ) %>%
    {if(remove_all_small) (.) %>%
        # remove sites where all cover values had to be assigned as a small value
        dplyr::group_by(dplyr::across(tidyselect::any_of(context))
                        , dplyr::across(tidyselect::contains("cut_pc"))
                        ) %>%
        dplyr::mutate(covmean = mean(cover_adj, na.rm = TRUE)
                      , sd = sd(cover_adj, na.rm = TRUE)
                      ) %>%
        dplyr::ungroup() %>%
        dplyr::filter(covmean != small_cov & sd != 0) else (.)
      } %>%
    dplyr::distinct(taxa
                    , dplyr::across(tidyselect::any_of(context))
                    , dplyr::across(tidyselect::contains("cut_pc"))
                    , cover_adj
                    )

  result <- result |>
    dplyr::mutate(cut_cover = cut(cover_adj
                                  , breaks = c(0, lucover$cover_max)
                                  )
                  ) |>
    dplyr::left_join(lucover |>
                       dplyr::mutate(cut_cover = cut(lucover$cover_max
                                                     , breaks = c(0, lucover$cover_max)
                                                     )
                                     ) |>
                       dplyr::select(cover_code, cut_cover, !!rlang::ensym(lucover_col))
                     ) |>
    dplyr::select(-tidyselect::matches("cut_")
                  , use_cover_code = cover_code
                  , use_cover = !!rlang::ensym(lucover_col)
                  )

  return(result)

}
