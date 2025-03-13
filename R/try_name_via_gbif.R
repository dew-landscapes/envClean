#' Attempt to find an unmatched scientific name using GBIF Backbone Taxonomy
#'
#' Useful in cases where the supplied scientific name does not find any match
#' in the taxonomic source being queried. This function queries the GBIF
#' Backbone Taxonomy for a supplied scientific name. The scientific
#' name from any successful match is then queried against
#' `galah::search_taxa()`.
#'
#' @param name Character. Taxa name to search.
#'
#' @return If no match, NULL. If matched, a tibble ready for input to the
#' overrides argument of `make_taxonomy()`.
#' @export
#'
#' @examples
#' galah::search_taxa("Peziza vesiculosa Bull.: Fr.") # Homonym issue
#' try_name_via_gbif("Peziza vesiculosa Bull.: Fr.")
try_name_via_gbif <- function(name) {

  qry <- rgbif::name_backbone(name = name)

  # attempt 1 -------
  # Direct galah::search_taxa on any returned gbif scientific name
  if(all(qry$matchType != "NONE", qry$scientificName != name)) {

    result <- galah::search_taxa(qry$scientificName)

  } else result <- NULL

  # attempt 2 -------
  # Try searching using gbif common name
  if(all(qry$matchType != "NONE", ! all(c("scientific_name") %in% names(result)))) {

    qry <- qry |>
      dplyr::pull(usageKey) |>
      rgbif::name_usage(data = "vernacularNames")

    if(!is.null(qry$data$vernacularName)) {

      result <- qry$data |>
        dplyr::filter(language == "eng") %>%
        {if("preferred" %in% names(.)) (.) %>% dplyr::filter(! isFALSE(preferred)) else (.)} %>%
        {if("area" %in% names(.)) (.) |> dplyr::filter(area == "eng") else (.)} |>
        dplyr::pull(vernacularName) |>
        gsub("\\,.*", "", x = _)

      result <- if(length(result)) galah::search_taxa(result) else NULL

    }

    result <- if(all(c("scientific_name") %in% names(result))) {

      result |>
        dplyr::filter(match_type == "vernacularMatch") |>
        dplyr::add_count(kingdom, scientific_name) |>
        dplyr::select(-search_term) |>
        dplyr::filter(n == max(n)) |>
        dplyr::distinct()

    } else NULL

  }

  if(!exists("result", inherits = FALSE)) result <- NULL

  return(result)

}
