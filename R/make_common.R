#' Make common name lookup
#'
#' Finds common names for taxa based on highest frequency of occurrence in data using envClean::make_attribute.
#' Compared to a straight run of envClean::make_attribute, this function first attempts to match a taxa to a
#' common name based on only common names belonging to taxa names of the same rank (i.e. species names will only
#' be matched to species level common names if they occur in the data). Taxa that cannot be matched to a common
#' name at their taxonomic rank will be given a common name from a lower level (e.g. a species without a species
#' level match will be given the most frequently occurring subspecies common name).
#'
#' @param df Dataframe with `taxa_col` and `common_col`, and optionally x, y coordinate columns if `use_aoi` == TRUE.
#' @param taxa_col Character name of column in `df` that was passed to `get_taxonomy` as `taxa_col`.
#' @param common_col Character name of column in `df` that contains the common names.
#' @param taxonomy List resulting from call to `make_taxonomy()`.
#' @param max_guess Character. If common name values are not available for `taxa`, try guessing from values up to `max_guess`
#' level of taxonomic hierarchy. See `lurank`. Note it does not make sense to provide a rank here that is lower than
#' the `target_rank` provided to `make_taxonomy` when `taxonomy` was made.
#' @param context Any other columns in `df` to maintain throughout summarising.
#' @param remove_strings Character. Any values in `common_col` to exclude.
#' @param use_aoi sf. Name of sf object for filtering data for common name generation using envClean::filter_geo_range.
#' @param df_x Character. Name of column with x coordinate.
#' @param df_y Character. Name of column with y coordinate.
#' @param crs_df Anything that will return a legitimate crs when passed to the
#' crs attribute of st_transform or st_as_sf.
#'
#' @return Dataframe with one row for each taxa with best guess at a common name based on the values in `common_col`.
#'
#' @export
#'
#' @examples
#'
make_common <- function(df
                        , taxa_col = "original_name"
                        , common_col = "common"
                        , taxonomy = taxonomy
                        , max_guess = "species"
                        , context = "kingdom"
                        , remove_strings = c("n/a", "''", "NA", "^\\s*$")
                        , use_aoi = NULL
                        , df_x = "long"
                        , df_y = "lat"
                        , crs_df = 4326
) {

  # lowest rank ----
  # find lowest rank for use below
  lowest_rank <- envClean::lurank |>
    dplyr::filter(rank <= max_guess
                  , rank == min(rank)
    ) |>
    dplyr::mutate(rank = as.character(rank)
                  , rank = dplyr::if_else(rank %in% c("variety", "form"), "subspecies", rank)
    ) |>
    dplyr::pull(rank)

  # prep ----
  # filter by aoi, select cols and add returned rank for splitting below
  prep <- df |>
    dplyr::filter(!is.na(!!rlang::ensym(common_col))) %>%
    {if(!is.null(use_aoi)) envClean::filter_geo_range(., use_aoi = use_aoi, x = df_x, y = df_y, crs_df = crs_df) else .} |>
    dplyr::select(!!rlang::ensym(taxa_col), !!rlang::ensym(common_col)) |>
    dplyr::left_join(taxonomy[[lowest_rank]]$lutaxa |>
                       dplyr::select(!!rlang::ensym(taxa_col), returned_rank)
    ) |>
    dplyr::filter(!is.na(returned_rank)
                  , returned_rank <= max_guess
    )

  # same ranks prep ----
  # nest data by returned rank to enable finding same rank common names in a map below
  same_ranks_prep <- prep |>
    dplyr::mutate(returned_rank = as.character(returned_rank)
                  , returned_rank = dplyr::if_else(returned_rank %in% c("variety", "form"), "subspecies", returned_rank)
    ) |>
    tidyr::nest(data = -c(returned_rank))

  # same ranks ----
  # run make_attribute with data for each returned rank to only feed the same level data, i.e. only common names from species level taxa will be used to generate species common names
  same_ranks <- purrr::map2(same_ranks_prep$data
                            , same_ranks_prep$returned_rank
                            , \(x, y) {

                              envClean::make_attribute(df = x
                                                       , taxa_col = taxa_col
                                                       , att_col = common_col
                                                       , taxonomy = taxonomy[[y]]
                                                       , max_guess = y
                                                       , context = context
                                                       , remove_strings = remove_strings
                              )

                            }
                            , .progress = TRUE
  ) |>
    dplyr::bind_rows() |>
    dplyr::filter(!is.na(!!rlang::ensym(common_col)))

  # other ranks prep ----
  other_ranks_prep <- prep |>
    dplyr::select(-returned_rank)

  # other ranks ----
  # straight run of make_attribute based on all data at the highest max guess to obtain common names based on all ranks
  # then only retain taxa results with common names not found within the same rank
  other_ranks_aoi <-
    envClean::make_attribute(df = other_ranks_prep
                             , taxa_col = taxa_col
                             , att_col = common_col
                             , taxonomy = taxonomy[[max_guess]]
                             , max_guess = max_guess
                             , context = context
                             , remove_strings = remove_strings
    ) |>
    dplyr::anti_join(same_ranks, by = "taxa")

  # combine ----
  res <- same_ranks |>
    dplyr::bind_rows(other_ranks_aoi)

  return(res)

}
