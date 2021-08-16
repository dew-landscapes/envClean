


#' Generate best guess of lifeform for each taxa*context
#'
#' @param df Dataframe with context, taxa and lifeform columns.
#' @param context Character. Name of columns defining context.
#' @param lulf Object mapping lifeform to some form of order to break ties.
#' @param envprcomp Output from env_pca()
#'
#' @return Dataframe with best guess lifeform replacing original lifeform.
#' @export
#'
#' @examples
  add_lifeform <- function(df
                           , context = "cell"
                           , lulf = tibble::tibble(
                             lifeform = unique(df$lifeform)
                             , SortID = 1:length(unique(df$lifeform))
                             )
                           ) {

    PCALifeform <- if(isTRUE(!is.null(envprcomp))) {

      df %>%
        dplyr::filter(!is.na(lifeform)) %>%
        dplyr::count(Taxa,lifeform,across(contains("cutPC"))) %>%
        dplyr::left_join(lulf %>%
                           dplyr::select(lifeform,SortID)
                         ) %>%
        dplyr::group_by(Taxa,across(contains("cutPC"))) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::filter(SortID == min(SortID)) %>%
        dplyr::ungroup() %>%
        dplyr::select(Taxa,contains("cutPC"),PCALifeform = lifeform)

    } else {

      df %>%
        dplyr::select(Taxa,contains("cutPC"),PCALifeform = lifeform)

      }

    taxaLifeform <- df %>%
      dplyr::filter(!is.na(lifeform)) %>%
      dplyr::left_join(lulf) %>%
      dplyr::count(Taxa,SortID,lifeform) %>%
      dplyr::group_by(Taxa) %>%
      dplyr::filter(n == max(n)) %>%
      dplyr::filter(SortID == min(SortID)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-n, -SortID) %>%
      dplyr::rename(taxaLifeform = lifeform)

    df %>%
      dplyr::rename(siteLifeform = lifeform) %>%
      dplyr::left_join(PCALifeform) %>%
      dplyr::left_join(taxaLifeform) %>%
      dplyr::mutate(lifeform = if_else(!is.na(siteLifeform)
                                   ,siteLifeform
                                   ,if_else(!is.na(PCALifeform)
                                            , PCALifeform
                                            , taxaLifeform
                                            )
                                   )
                    ) %>%
      dplyr::select(names(df))

  }

