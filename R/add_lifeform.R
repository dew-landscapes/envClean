


#' Generate best guess of lifeform for each taxa*context
#'
#' @param df Dataframe with context, taxa and lifeform columns.
#' @param context Character. Name of columns defining context.
#' @param env_prcomp Optional. Output from env_pca()
#' @param lulife Dataframe lookup for lifeform.
#'
#' @return Dataframe with best guess lifeform replacing original lifeform.
#' @export
#'
#' @examples
  add_lifeform <- function(df
                           , context = "cell"
                           , env_prcomp = NULL
                           , lulife
                           ) {

    pca_lifeform <- if(isTRUE(!is.null(env_prcomp))) {

      df %>%
        dplyr::filter(!is.na(lifeform)) %>%
        dplyr::count(taxa,lifeform,across(contains("cutpc"))) %>%
        dplyr::left_join(lulife %>%
                           dplyr::select(lifeform,sort)
                         ) %>%
        dplyr::group_by(taxa,across(contains("cutpc"))) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::filter(sort == min(sort)) %>%
        dplyr::ungroup() %>%
        dplyr::select(taxa,contains("cutpc"),pca_lifeform = lifeform) %>%
        dplyr::distinct()

    } else {

      df %>%
        dplyr::select(taxa,contains("cutpc"),pca_lifeform = lifeform) %>%
        dplyr::distinct()

      }

    taxa_lifeform <- df %>%
      dplyr::filter(!is.na(lifeform)) %>%
      dplyr::left_join(lulife) %>%
      dplyr::count(taxa,sort,lifeform) %>%
      dplyr::group_by(taxa) %>%
      dplyr::filter(n == max(n)) %>%
      dplyr::filter(sort == min(sort)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-n, -sort) %>%
      dplyr::rename(taxa_lifeform = lifeform)

    df %>%
      dplyr::rename(site_lifeform = lifeform) %>%
      dplyr::left_join(pca_lifeform) %>%
      dplyr::left_join(taxa_lifeform) %>%
      dplyr::mutate(lifeform = if_else(!is.na(site_lifeform)
                                   ,site_lifeform
                                   ,if_else(!is.na(pca_lifeform)
                                            , pca_lifeform
                                            , taxa_lifeform
                                            )
                                   )
                    ) %>%
      dplyr::select(names(df))

  }

