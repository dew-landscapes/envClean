
#' Remove any ' or " from specified columns in a dataframe
#'
#' Useful due to the reliance on original_name as an 'id' throughout env
#' packages. If not removed, quotes are sometimes escaped by the underlying
#' functions used, apparently creating duplicates among original_name.
#'
#' @param df Dataframe
#' @param cols Character. Column from which to remove quotes
#'
#' @keywords internal
#' @return Dataframe with quotes removed from `cols`
#' @export
#'
#' @examples
#' x <- data.frame(a = c("species A", "species 'A'"))
#' clean_quotes(x, cols = "a")
  clean_quotes <- function(df
                           , cols = "original_name"
                           ) {

      df %>%
        dplyr::mutate(dplyr::across(tidyselect::any_of(cols), \(x) gsub("'", "", x, fixed = TRUE))) %>%
        dplyr::mutate(dplyr::across(tidyselect::any_of(cols), \(x) gsub('"', "", x, fixed = TRUE)))

  }
