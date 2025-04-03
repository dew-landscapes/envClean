#' Attempt to find a taxa for names with no match in `galah::search_taxa()`
#'
#' For an unmatched 'name', try to find a match via: `rgbif::name_usage()`;
#' `rgbif::name_backbone()`; and removal of any characters in 'name' after an
#' 'x' or 'X' (i.e. treat hybrids as just the first taxa). Using
#' `rgbif::name_backbone()` allows fuzzy matching to fix spelling errors. Any
#' results are passed back to `galah::search_taxa()` to retrieve an
#' override to use for that 'name' (so long as the rgbif result is not identical
#' to 'name'). Any 'name' still completely unmatched is just given the override
#' 'name' so it will not be lost from downstream processes but will not have any
#' associated taxonomic information.
#'
#' @param df Dataframe of biological records
#' @param taxa_col Character. Name of column in `df` containing the taxonomic
#' entities for which a match is desired.
#' @param taxonomy Result of call to `make_taxonomy()`
#' @param target_rank Character. Level within `envClean::lurank$rank` to target
#'
#' @return Tibble in appropriate form to pass to the overrides argument of
#' `make_taxonomy()`
#' @export
#'
#' @example inst/examples/make_galah_taxonomy_ex.R
make_unmatched_overrides <- function(df
                                     , taxa_col = "original_name"
                                     , taxonomy
                                     , target_rank = "species"
) {

  # catch taxa_col == "taxa"
  if(taxa_col == "taxa") {

    df <- df |>
      dplyr::rename(original_name = taxa)

    taxa_col <- "original_name"

  }

  # find unmatched -------
  unmatched <- df |>
    dplyr::distinct(!!rlang::ensym(taxa_col)) |>
    envClean::clean_quotes() |>
    dplyr::left_join(taxonomy[[target_rank]]$lutaxa) |>
    dplyr::filter(is.na(taxa)
                  |((original_is_bi|original_is_tri) & returned_rank > "species" & target_rank %in% c("species", "subspecies"))
                  |(original_is_tri & returned_rank > "subspecies" & target_rank == "subspecies")
    ) |>
    dplyr::filter(!!rlang::ensym(taxa_col) != ""
                  , !grepl("sp\\.$|another\\s|unverified|\\?", original_name)
                  , grepl(".*\\s.*", original_name)
    ) |>
    # dplyr::sample_n(200) |> # TESTING
    dplyr::select(!!rlang::ensym(taxa_col))

  if(nrow(unmatched)) {

    # non in ALA --------
    # try gbif to fix taxa just not found in ALA
    unmatched_via_gbif <- unmatched |>
      dplyr::select(!!rlang::ensym(taxa_col)) |>
      dplyr::mutate(res = purrr::map(!!rlang::ensym(taxa_col)
                                     , \(x) try_name_via_gbif(x
                                                              , target_rank = target_rank
                                     )
                                     , .progress = TRUE
      )
      ) |>
      tidyr::unnest(cols = c(res))

    if(any((target_rank != "subspecies" & ! target_rank %in% names(unmatched_via_gbif)), ! "scientific_name" %in% names(unmatched_via_gbif))) {

      unmatched_via_gbif <- unmatched_via_gbif |>
        dplyr::slice(0)

    } else {

      unmatched_via_gbif <- unmatched_via_gbif |>
        dplyr::filter(! is.na(!!rlang::ensym(target_rank)))

    }

    # hybrids --------
    unmatched_hybrids <- unmatched |>
      dplyr::anti_join(unmatched_via_gbif) |>
      dplyr::mutate(searched_name = gsub("\\sX\\s.*|\\sx\\s.*", "", original_name)) %>%
      dplyr::mutate(res = purrr::map(searched_name, galah::search_taxa)) |>
      tidyr::unnest(cols = c(res))

    if(any((target_rank != "subspecies" & ! target_rank %in% names(unmatched_hybrids)), ! "scientific_name" %in% names(unmatched_hybrids))) {

      unmatched_hybrids <- unmatched_hybrids |>
        dplyr::slice(0)

    } else {

      unmatched_hybrids <- unmatched_hybrids |>
        dplyr::filter(! is.na(!!rlang::ensym(target_rank)))

    }

    # altogether ------
    if(any(exists("unmatched_hybrids"), exists("unmatched_via_gbif"))) {

      overrides_unmatched <- unmatched |>
        dplyr::left_join(dplyr::bind_rows(mget(ls(pattern = "^unmatched_"))
                                          , .id = "note"
        ) |>
          dplyr::filter(! is.na(!!rlang::ensym(taxa_col))) |>
          dplyr::select(!!rlang::ensym(taxa_col)
                        , tidyr::any_of(tidyr::matches(target_rank))
                        , scientific_name
                        , kingdom
                        , note
          ) |>
          dplyr::distinct() |>
          dplyr::mutate(note = gsub("unmatched_", "", note)
                        , note = gsub("_", " ", note)
          )
        ) |>
        dplyr::select(!!rlang::ensym(taxa_col)
                      , taxa_to_search = scientific_name
                      , use_kingdom = kingdom
                      , tidyr::any_of(tidyr::matches(target_rank))
                      , note
        ) %>%
        {if("species" %in% names(.)) dplyr::mutate(., use_species = dplyr::case_when(! is.na(species) & ! grepl("\\.$", species) ~ species
                                                                                     , ! is.na(taxa_to_search) & (is.na(species)|grepl("\\.$", species)) ~ taxa_to_search
                                                                                     , .default = !!rlang::ensym(taxa_col)
        )
        ) else dplyr::mutate(., use_species = dplyr::if_else(! is.na(taxa_to_search)
                                                             , taxa_to_search
                                                             , !!rlang::ensym(taxa_col)
        )
        )
        } |>
        dplyr::mutate(use_subspecies = dplyr::if_else(! is.na(taxa_to_search) & target_rank == "subspecies"
                                                      , taxa_to_search
                                                      , NA
        )
        , taxa_to_search = dplyr::if_else(! is.na(taxa_to_search)
                                          , taxa_to_search
                                          , !!rlang::ensym(taxa_col)
        )
        ) |>
        dplyr::select(-tidyr::any_of("species")) |>
        dplyr::relocate(use_kingdom, note, .after = dplyr::last_col())

    } else overrides_unmatched <- tibble::tibble()

  } else overrides_unmatched <- tibble::tibble()

  return(overrides_unmatched)

}
