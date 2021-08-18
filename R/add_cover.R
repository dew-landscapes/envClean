

#' Generate best guess of cover for each taxa*context
#'
#' @param df Dataframe with context, taxa and cover columns.
#' @param context Character. Name of columns defining context.
#' @param covcol Character. Name of column containing cover values.
#' @param smallcov A small cover value assigned to any record for which there
#' is no available site, pca or taxa cover value.
#'
#' @return Dataframe with covcol removed and replaced with best guess cover in
#' column usecover
#' @export
#'
#' @examples
  add_cover <- function(df
                        , context = "cell"
                        , covcol = "usecover"
                        , smallcov = 0.009
                        ) {

    namesdf <- c(names(df)[names(df) != covcol],"usecover")

    sitecov <- df %>%
      dplyr::group_by(across(contains("cutpc")),across(all_of(context)),taxa) %>%
      dplyr::summarise(sitecover = max(!!ensym(covcol))) %>%
      dplyr::ungroup()

    pcacov <- df %>%
      dplyr::group_by(across(contains("cutpc")),taxa) %>%
      dplyr::summarise(pcacover = median(!!ensym(covcol), na.rm = TRUE)) %>%
      dplyr::ungroup()

    taxacov <- df %>%
      dplyr::group_by(taxa) %>%
      dplyr::summarise(taxacover = median(!!ensym(covcol), na.rm = TRUE)) %>%
      dplyr::ungroup()

    df %>%
      dplyr::left_join(sitecov) %>%
      dplyr::left_join(pcacov) %>%
      dplyr::left_join(taxacov) %>%
      dplyr::mutate(usecover = if_else(!is.na(sitecover)
                                   ,sitecover
                                   ,if_else(!is.na(pcacover)
                                            , pcacover
                                            , taxacover
                                            )
                                   )
                    , usecover = if_else(is.na(usecover),smallcov,usecover)
                    ) %>%
      dplyr::select(all_of(names(df))) %>%
      # remove sites where all cover values had to be assigned as a small value
      dplyr::group_by(across(all_of(context))) %>%
      dplyr::mutate(covmean = mean(usecover)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(covmean != smallcov) %>%
      dplyr::select(all_of(namesdf))

    }
