#' Add standardised `method` column
#'
#' `method` column in returned data frame is the match for the name in
#' `method_col` based on the supplied `methods_lu`.
#'
#' @param df Dataframe to attribute with the standardised method from the `methods_lu`.
#' @param method_col Character. Name of column in `df` with method.
#' @param methods_lu Dataframe with 'method' column for the original method name and 'method_gp' column for the standardised methods.
#' @param unassigned Character. Value for unmatched or NA methods.
#'
#' @return Dataframe with standardised `method` column.
#' @export
#'
#' @examples

bin_method <- function(df
                       , methods_lu
                       , method_col = "method"
                       , unassigned = "observed"
) {

  res <-  df %>%
    {if(method_col != "method") dplyr::rename(method = method_col) else .} |>
    dplyr::left_join(methods_lu |>
                       dplyr::distinct(method, method_gp)
    ) |>
    dplyr::select(-method) |>
    dplyr::rename(method = method_gp) |>
    dplyr::mutate(method = dplyr::if_else(is.na(method), unassigned, method))

  return(res)

}
