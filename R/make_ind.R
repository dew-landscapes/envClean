#' Make indigenous status lookup
#'
#' Finds indigenous status for taxa based on highest frequency of occurrence in data using `make_attribute`.
#' Compared to a straight run of `make_attribute`, this function first attempts to find an indigenous status
#' for a taxa based on values found in a primary data source (if supplied). Taxa with no indigenous values in the primary data
#' source are then given an indigenous status based on all other data sources. In addition, flags and overrides indicating
#' non-indigenous taxa can also be provided to overcome errors in the data, and the indigenous calculation can be
#' restricted to an area of interest (aoi).
#'
#' @param df Dataframe with `taxa_col` and `ind_col`, and optionally x, y coordinate columns if `use_aoi` == TRUE.
#' @param taxa_col Character name of column in `df` that was passed to `get_taxonomy` as `taxa_col`.
#' @param ind_col Character name of column in `df` that contains the common names.
#' @param taxonomy List resulting from call to `make_taxonomy()`.
#' @param max_guess Character. If indigenous values are not available for `taxa`, try guessing from values up to `max_guess`
#' level of taxonomic hierarchy. See `lurank`. Note it does not make sense to provide a rank here that is lower than
#' the `target_rank` provided to `make_taxonomy` when `taxonomy` was made.
#' @param context Any other columns in `df` to maintain throughout summarising.
#' @param remove_strings Character. Any values in `ind_col` to exclude.
#' @param primary_data_source Character value from `data_source_col` indicating the name of the primary data source.
#' @param data_source_col Character name of the data source column with the name of the primary data source.
#' @param non_ind_terms Character vector of non-indigenous terms found in common names to use for applying non-indigenous
#' status regardless of the indigenous values in the data.
#' @param common_df Data frame containing `taxa_col` and 'common' field with common names for each taxa to use for
#' applying non-indigenous status with the `non_ind_terms`.
#' @param genus_overrides Character vector of known non-indigenous genera to apply non-indigenous status to all
#' species within those genera regardless of the indigenous values in the data.
#' @param species_overrides Character vector of known non-indigenous species to apply non-indigenous status regardless
#' of the indigenous values in the data.
#' @param use_aoi sf. Name of sf object for filtering data for indigenous status generation using envClean::filter_geo_range.
#' @param df_x Character. Name of column with x coordinate.
#' @param df_y Character. Name of column with y coordinate.
#' @param crs_df Anything that will return a legitimate crs when passed to the
#' crs attribute of st_transform or st_as_sf.
#'
#' @return Dataframe with one row for each taxa with best guess at a common name based on the values in `ind_col`.
#'
#' @export
#'
#' @examples
#'
make_ind <- function(df
                     , taxa_col = "original_name"
                     , ind_col = "ind"
                     , taxonomy
                     , max_guess = "species"
                     , context = "kingdom"
                     , remove_strings = c("n/a", "''", "NA", "^\\s*$")
                     , primary_data_source = NULL
                     , data_source_col = NULL
                     , non_ind_terms = NULL
                     , common_df = NULL
                     , genus_overrides = NULL
                     , species_overrides = NULL
                     , use_aoi = NULL
                     , df_x = "long"
                     , df_y = "lat"
                     , crs_df = 4326
) {

  if(all(!is.null(primary_data_source), !is.null(data_source_col))) {

    primary_ind <- df |>
      dplyr::filter(!!rlang::ensym(data_source_col) == primary_data_source) %>%
      {if(!is.null(use_aoi)) envClean::filter_geo_range(., use_aoi = use_aoi, x = df_x, y = df_y, crs_df = crs_df) else .} |>
      dplyr::select(!!rlang::ensym(taxa_col), !!rlang::ensym(ind_col)) |>
      dplyr::rename(ind = ind_col) |>
      envClean::make_attribute(att_col = "ind"
                               , taxonomy = taxonomy
                               , max_guess = max_guess
                               , unknown_action = "U"
                               , remove_strings = remove_strings
      ) |>
      dplyr::filter(!is.na(ind_vals)) # remove taxa not found in bdbsa being added via taxonomy in make_attribute (allows potential for correct attribution for missing taxa from other sources below)

  }


  all_ind <- df %>%
    {if(!is.null(use_aoi)) envClean::filter_geo_range(., use_aoi = use_aoi, x = df_x, y = df_y, crs_df = crs_df) else .} |>
    dplyr::select(!!rlang::ensym(taxa_col), !!rlang::ensym(ind_col)) |>
    dplyr::rename(ind = ind_col) |>
    envClean::make_attribute(att_col = "ind"
                             , taxonomy = taxonomy
                             , max_guess = max_guess
                             , unknown_action = "U"
                             , remove_strings = remove_strings
    )

  if(exists("primary_ind")) {

    all_ind <- all_ind |>
      dplyr::anti_join(primary_ind, by = "taxa") |>
      dplyr::bind_rows(primary_ind)

  }

  res <- all_ind %>%
    {if(all(!is.null(common_df), !is.null(non_ind_terms))) dplyr::left_join(., common_df |>
                                                                              dplyr::select(taxa, common)
                                                                            , by = "taxa") |>
        dplyr::mutate(ind = ifelse(grepl(paste0(non_ind_terms, "\\s", collapse = "|"), common)
                                   , "N"
                                   , ind
        )
        ) else .} %>%
    {if(!is.null(genus_overrides)) dplyr::mutate(., ind = ifelse(grepl(paste0("^", genus_overrides, "\\s"
                                                                              , collapse = "|")
                                                                       , taxa
    )
    , "N"
    , ind
    )
    ) else .} %>%
    {if(!is.null(species_overrides)) dplyr::mutate(., ind = ifelse(grepl(paste0("^", species_overrides, "$"
                                                                                , collapse = "|"
    )
    , taxa
    )
    , "N"
    , ind
    )
    ) else .} |>
    dplyr::arrange(taxa) |>
    dplyr::distinct(taxa, ind, ind_vals, ind_from) |>
    dplyr::mutate(ind = dplyr::if_else(grepl("\\?", ind), "U" , ind))

  return(res)

}
