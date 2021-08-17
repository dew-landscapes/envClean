

#' Generate best guess of cover for each taxa*context
#'
#' @param df Dataframe with context, taxa and cover columns.
#' @param context Character. Name of columns defining context.
#' @param smallcov A small cover value assigned to any record for which there
#' is no available site, pca or taxa cover value.
#'
#' @return Dataframe with covCol removed and replaced with best guess cover in
#' column useCover
#' @export
#'
#' @examples
  add_cover <- function(df
                        , context = "cell"
                        , covCol = "useCover"
                        , smallcov = 0.009
                        ) {

    namesdf <- c(names(df)[names(df) != covCol],"useCover")

    sitecov <- df %>%
      dplyr::group_by(across(contains("cutPC")),across(all_of(context)),Taxa) %>%
      dplyr::summarise(siteCover = max(!!ensym(covCol))) %>%
      dplyr::ungroup()

    pcacov <- df %>%
      dplyr::group_by(across(contains("cutPC")),Taxa) %>%
      dplyr::summarise(pcaCover = median(!!ensym(covCol), na.rm = TRUE)) %>%
      dplyr::ungroup()

    taxacov <- df %>%
      dplyr::group_by(Taxa) %>%
      dplyr::summarise(taxaCover = median(!!ensym(covCol), na.rm = TRUE)) %>%
      dplyr::ungroup()

    df %>%
      dplyr::left_join(sitecov) %>%
      dplyr::left_join(pcacov) %>%
      dplyr::left_join(taxacov) %>%
      dplyr::mutate(useCover = if_else(!is.na(siteCover)
                                   ,siteCover
                                   ,if_else(!is.na(pcaCover)
                                            , pcaCover
                                            , taxaCover
                                            )
                                   )
                    , useCover = if_else(is.na(useCover),smallcov,useCover)
                    ) %>%
      dplyr::select(all_of(names(df))) %>%
      # remove sites where all cover values had to be assigned as a small value
      dplyr::group_by(across(all_of(context))) %>%
      dplyr::mutate(covMean = mean(useCover)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(covMean != smallcov) %>%
      dplyr::select(all_of(namesdf))

    }
