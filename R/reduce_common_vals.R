#' Reduce common name values
#'
#' Simplify and standardise a vector of common names. Sets title case (except for characters following a hyphen, e.g. "White-throated"),
#' and attempts to ignore differences from the use of a hyphen (e.g. "white throated" and "white-throated" reduce to the hyphenated version).
#' Also retains small words as lowercase, and all-caps strings as all-caps (e.g. "MLR", "NE" are returned the same).
#'
#' @param x Vector of common name values
#' @small_words Vector of small words to keep lowercase (not applied to the first character of x values)
#'
#' @returns Character vector
#' @export
#'
#' @examples
#'
#' reduce_common_vals(c("White throated treecreeper", "White-throated treecreeper", "a species of moth", "mallard x duck", "Chestnut rumped heathwren (MLR)"))
#'
reduce_common_vals <- function(x,
                               small_words = c("and", "or", "of", "a", "an", "except", "x")
) {

  x <- unique(x[!is.na(x)])

  x_stripped <- x |>
    stringr::str_squish() |>
    stringr::str_to_lower() |>
    stringr::str_replace_all("-", " ")

  # prefer values with hyphens: reorder x to put them first so duplicated() keeps them
  x_reord <- order(!stringr::str_detect(x, "-"))

  x_collapsed <- x[x_reord[!duplicated(x_stripped[x_reord])]]

  # Keep post-hyphen characters and small words lower case
  keep_lower <- paste0(
    "(?i)",           # ignore case for the whole pattern (as pattern is applied after str_to_title)
    "(?<=-)\\p{L}",   # any letter straight after a hyphen (eg return as "White-throated")
    "|(?<=\\s)(?:", paste(small_words, collapse = "|"), ")(?=\\s|$)"  # small word after a space (eg return as "Red and Black")
  )

  # Keep already-allcaps whole words (2+ letters) as upper case (eg "SA", "MLR", "NE")
  upper_codes <- stringr::str_extract_all(x, "\\b\\p{Lu}{2,}\\b") |>
    unlist()
  keep_upper <- if (length(upper_codes)) {
    paste0("(?i)\\b(?:", paste(upper_codes, collapse = "|"), ")\\b")
  } else "(?!)"

  # Result
  stringr::str_to_title(x_collapsed) |>
    stringr::str_replace_all(keep_lower, stringr::str_to_lower) |>
    stringr::str_replace_all(keep_upper, stringr::str_to_upper)
}
