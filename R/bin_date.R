
#' Add temporal bins to a dataframe
#'
#' @param df Dataframe, with `date_col`, to be augmented with temporal bins
#' @param date_col Character. Name of column in `df` containing dates
#' @param bin Character or dataframe. If character, any lubridate package
#' method(s) such as `year` for `lubridate::year()` or `month` for
#' `lubridate::month()`. If dataframe, it must contain a `date_col` column with
#' a match for every instance in `df` `data_col` (or at least every instance
#' needed in a temporal bin).
#'
#' @return `df` with extra columns from `bin`
#' @export
#'
#' @examples
  bin_date <- function(df
                        , date_col = "date"
                        , bin = c("year", "month")
                        ) {

    dates <- df %>%
        dplyr::distinct(!!rlang::ensym(date_col))

    if("character" %in% class(bin)) {

      dates <- purrr::map(bin
                          , \(x) {

                            func <- utils::getFromNamespace(x, ns = "lubridate")

                            dates %>%
                              dplyr::mutate(!!rlang::ensym(x) := func(!!rlang::ensym(date_col)))


                          }
                          ) %>%
        purrr::reduce(dplyr::left_join)

    }

    if("data.frame" %in% class(bin)) {

      dates <- dates %>%
        dplyr::left_join(bin)

    }

    res <- df %>%
      dplyr::left_join(dates)

  }
