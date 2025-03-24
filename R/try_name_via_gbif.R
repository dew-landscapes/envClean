#' Attempt to find an unmatched scientific name using GBIF Backbone Taxonomy
#'
#' Useful in cases where the supplied scientific name does not find any match
#' in the taxonomic source being queried. This function queries the GBIF
#' Backbone Taxonomy for a supplied scientific name. The scientific
#' name from any successful match is then queried against
#' `galah::search_taxa()`.
#'
#' @param name Character. Taxa name to search.
#' @param target_rank Character. Level within `envClean::lurank$rank` to target.
#'
#' @return If no match, NULL. If matched, a tibble ready for input to the
#' overrides argument of `make_taxonomy()`.
#' @export
#'
#' @examples
#' galah::search_taxa("Peziza vesiculosa Bull.: Fr.") # Homonym issue
#' try_name_via_gbif("Peziza vesiculosa Bull.: Fr.")
try_name_via_gbif <- function(name
                              , target_rank
) {

  qry <- rgbif::name_backbone(name = name)

  # attempt 1 -------
  # Direct galah::search_taxa on any returned gbif scientific name
  if(all(qry$matchType != "NONE", qry$scientificName != name)) {

    result1 <- galah::search_taxa(qry$scientificName)

    if(all(c("scientific_name", "kingdom") %in% names(result1))) {

      result1 |>
        dplyr::add_count(kingdom, scientific_name) |>
        dplyr::select(-search_term) |>
        dplyr::filter(n == max(n)) |>
        dplyr::distinct()

    }

  }


  # attempt 2 -------
  # Try searching using gbif common name
  if(all(qry$matchType != "NONE", ! all(c("scientific_name", target_rank) %in% names(result1)))) {

    qry <- qry |>
      dplyr::pull(usageKey) |>
      rgbif::name_usage(data = "vernacularNames")

    if(!is.null(qry$data$vernacularName)) {

      result2 <- qry$data |>
        dplyr::filter(language == "eng") %>%
        {if("preferred" %in% names(.)) (.) %>% dplyr::filter(! isFALSE(preferred)) else (.)} %>%
        {if("area" %in% names(.)) (.) |> dplyr::filter(area == "eng") else (.)} |>
        dplyr::pull(vernacularName) |>
        gsub("\\,.*", "", x = _)

      if(length(result2)) {

        result2 <- galah::search_taxa(result2)

        if(all(c("scientific_name", "match_type", "kingdom") %in% names(result2))) {

          result2 |>
            dplyr::filter(match_type == "vernacularMatch") |>
            dplyr::add_count(kingdom, scientific_name) |>
            dplyr::select(-search_term) |>
            dplyr::filter(n == max(n)) |>
            dplyr::distinct()

        }

      }

    }

  }


  # best attempt ----
  if(exists("result1")) {

    result <- result1 %>%
      {if(all(exists("result2"), "scientific_name" %in% names(result2))) dplyr::bind_rows(., result2) else .}

  } else if(all(!exists("result1"), exists("result2"))) {

    result <- result2

  } else {

    result <- NULL

  }

  result <- if(all(c("scientific_name") %in% names(result))) {

    result |>
      dplyr::mutate(rank = factor(rank, levels = levels(envClean::lurank$rank), ordered = TRUE)) |>
      dplyr::filter(rank == min(rank, na.rm = TRUE)) |>
      dplyr::select(-tidyr::any_of("n")) |>
      dplyr::add_count(kingdom, scientific_name) |>
      dplyr::filter(n == max(n)) |>
      dplyr::distinct() |>
      dplyr::slice(1)

  }

  return(result)

}
