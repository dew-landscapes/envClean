

#' Generate best guess of cover for each taxa*context
#'
#' @param df Dataframe with context, taxa and cover columns.
#' @param context Character. Name of columns defining context.
#' @param cov_col Character. Name of column containing cover values.
#' @param small_cov A small cover value assigned to any record for which there
#' is no available site, pca or taxa cover value.
#'
#' @return Dataframe with cov_col removed and replaced with best guess cover in
#' column use_cover
#' @export
#'
#' @examples
  add_cover <- function(df
                        , context = "cell"
                        , cov_col = "use_cover"
                        , small_cov = 0.009
                        ) {

    namesdf <- c(names(df)[names(df) != cov_col], cov_col)

    site_cov <- df %>%
      dplyr::group_by(across(contains("cut_pc")),across(all_of(context)),taxa) %>%
      dplyr::summarise(site_cover = max(!!ensym(cov_col), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(site_cover)
                    , site_cover != -Inf
                    )

    pca_cov <- df %>%
      dplyr::group_by(across(contains("cut_pc")),taxa) %>%
      dplyr::summarise(pca_cover = median(!!ensym(cov_col), na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!is.na(pca_cover)
                    , pca_cover != -Inf
                    )

    taxa_cov <- df %>%
      dplyr::group_by(taxa) %>%
      dplyr::summarise(taxa_cover = median(!!ensym(cov_col)
                                           , na.rm = TRUE
                                           )
                       ) %>%
      dplyr::ungroup()

    df %>%
      dplyr::left_join(site_cov) %>%
      dplyr::left_join(pca_cov) %>%
      dplyr::left_join(taxa_cov) %>%
      dplyr::mutate(use_cover = if_else(!is.na(site_cover)
                                   ,site_cover
                                   ,if_else(!is.na(pca_cover)
                                            , pca_cover
                                            , taxa_cover
                                            )
                                   )
                    , use_cover = if_else(is.na(use_cover),small_cov,use_cover)
                    ) %>%
      dplyr::select(all_of(names(df))) %>%
      # remove sites where all cover values had to be assigned as a small value
      dplyr::group_by(across(all_of(context))) %>%
      dplyr::mutate(covmean = mean(use_cover)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(covmean != small_cov) %>%
      dplyr::select(all_of(namesdf))

    }
