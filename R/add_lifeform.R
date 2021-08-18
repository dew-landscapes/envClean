


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
                             , sort = 1:length(unique(df$lifeform))
                             )
                           ) {

    pcalifeform <- if(isTRUE(!is.null(envprcomp))) {

      df %>%
        dplyr::filter(!is.na(lifeform)) %>%
        dplyr::count(taxa,lifeform,across(contains("cutpc"))) %>%
        dplyr::left_join(lulf %>%
                           dplyr::select(lifeform,sort)
                         ) %>%
        dplyr::group_by(taxa,across(contains("cutpc"))) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::filter(sort == min(sort)) %>%
        dplyr::ungroup() %>%
        dplyr::select(taxa,contains("cutpc"),pcalifeform = lifeform)

    } else {

      df %>%
        dplyr::select(taxa,contains("cutpc"),pcalifeform = lifeform)

      }

    taxalifeform <- df %>%
      dplyr::filter(!is.na(lifeform)) %>%
      dplyr::left_join(lulf) %>%
      dplyr::count(taxa,sort,lifeform) %>%
      dplyr::group_by(taxa) %>%
      dplyr::filter(n == max(n)) %>%
      dplyr::filter(sort == min(sort)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-n, -sort) %>%
      dplyr::rename(taxalifeform = lifeform)

    df %>%
      dplyr::rename(sitelifeform = lifeform) %>%
      dplyr::left_join(pcalifeform) %>%
      dplyr::left_join(taxalifeform) %>%
      dplyr::mutate(lifeform = if_else(!is.na(sitelifeform)
                                   ,sitelifeform
                                   ,if_else(!is.na(pcalifeform)
                                            , pcalifeform
                                            , taxalifeform
                                            )
                                   )
                    ) %>%
      dplyr::select(names(df))

  }

