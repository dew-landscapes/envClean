

#' Make a subspecies column
#'
#' Adds a subspecies column within taxonomic results (e.g. from
#' make_galah_taxonomy or make_gbif_taxonomy). Simultaneously creates a
#' `rank_adj` column that relevels any rank below subspecies to subspecies (i.e.
#' any rank = form or variety will have rank_adj = subspecies)
#'
#' @param tax_res Dataframe. Probably from call to `galah::search_taxa()`
#' @param use_levels Ordered factor. Ideally with a match for each unique rank
#' in `tax_res`
#' @param remove_brackets Character. Vector of regex to remove
#' from scientific_name before creating the subspecies column
#'
#' @return `tax_res` with extra columns `subspecies` and `rank_adj`. `rank` will
#' be an ordered factor.
#' @export
#' @keywords internal
#'
#' @examples
#' taxa <- c("Acacia lanigera", "Acacia lanigera var. gracilipes", "Spyridium glabrisepalum", "Spyridium eriocephalum var. glabrisepalum")
#' taxonomy_result <- galah::search_taxa(taxa)
#' make_subspecies_col(taxonomy_result) |> dplyr::select(search_term, species, subspecies, rank, rank_adj)
make_subspecies_col <- function(tax_res
                                , use_levels = levels(envClean::lurank$rank)
                                , remove_strings = "\\s\\(.*\\)\\s"
                                ) {

  tax_res %>%
    dplyr::mutate(rank = factor(rank
                                , levels = use_levels
                                , ordered = TRUE
                                )
                  , subspecies = dplyr::case_when(rank <= "subspecies" ~ gsub(paste0(remove_strings
                                                                                     , collapse = "|"
                                                                                     )
                                                                              , " "
                                                                              , scientific_name
                                                                              )
                                                  , TRUE ~ NA_character_
                                                  )
                  , rank_adj = dplyr::case_when(rank < "subspecies" ~ "subspecies"
                                                , TRUE ~ rank
                                                )
                  , rank_adj = factor(rank_adj
                                      , levels = use_levels
                                      , ordered = TRUE
                                      )
                  )


}
