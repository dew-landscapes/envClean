

#' Filter a dataframe column on character string(s)
#'
#' @param df Dataframe with column to filter.
#' @param filt_col Character. Name of column to filter.
#' @param filt_text Character string(s) to find in `filt_col`.
#' @param keep_text Logical. Keep or remove instances of `filt_text`?
#' @param df_join Optional dataframe. Joined to df before filter. No names from
#' `df_join` are returned.
#'
#' @return Filtered dataframe with same names as `df`.
#' @export
#'
#' @examples
  filter_text_col <- function(df
                         , filt_col
                         , filt_text
                         , keep_text = FALSE
                         , df_join = NULL
                         ) {

    joined <- if(isTRUE(!is.null(df_join))) {

      df %>%
        dplyr::left_join(df_join)

    } else df

    matched_rows <- joined %>%
      dplyr::distinct(!!rlang::ensym(filt_col)) %>%
      dplyr::filter(grepl(paste0(filt_text
                                 , collapse = "|"
                                 )
                          , !!rlang::ensym(filt_col)
                          )
                    )

    res <- if(keep_text) {

      joined %>%
        dplyr::inner_join(matched_rows) %>%
        dplyr::select(names(df))

    } else {

      joined %>%
        dplyr::anti_join(matched_rows) %>%
        dplyr::select(names(df))

    }

    return(res)

  }

