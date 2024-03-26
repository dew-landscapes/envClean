

#' Clean/Tidy to one row per taxa*Visit
#'
#' Deprecated in favour of the simpler workflow involving make_taxonomy then
#' bin_taxa (along with other bins, say, geographic and temporal). Required
#' attributes are then added to each taxa; context and/or taxa + context before
#' reducing to distinct context and associated attributes.
#'
#' @param df Dataframe to clean, filter and tidy with respect to taxonomy.
#' @param taxa_col Character. Name of column with taxa.
#' @param context Character. Name of columns defining context.
#' @param extra_cols Character. Name of any extra columns to keep.
#' @param required_rank Character. Default is 'species'. What level of the
#' taxonomic hierarchy is required for results. Records above this level are
#' filtered.
#' @param do_cov Logical. Should cover (needs to be supplied in df) be appended
#' to output.
#' @param do_life Logical. Should lifeform (needs to be supplied in df) be
#' appended to output.
#' @param do_ind Logical. Should ind (needs to be supplied in df) be
#' appended to output.
#' @param lucov Dataframe lookup for cover.
#' @param lulife Dataframe lookup for lifeform.
#' @param taxonomy list with (at least) named elements `lutaxa` and `taxonomy`.
#' Usually resulting from call to `envClean::make_taxonomy()`.
#'
#' @return Dataframe with columns taxa, context and, possibly, extracols
#' , lifeform and cover
#' @export
#'
#' @examples
  filter_taxa <- function(df
                          , taxa_col = "original_name"
                          , context
                          , extra_cols = NULL
                          , required_rank = "species"
                          , do_cov = FALSE
                          , do_life = FALSE
                          , lucov = NULL
                          , lulife = NULL
                          , taxonomy
                          ) {

    df <- df %>%
      dplyr::mutate(original_name = !!rlang::ensym(taxa_col))

    bio_taxa <- df %>%
      dplyr::distinct(original_name) %>%
      dplyr::left_join(taxonomy$lutaxa) %>%
      dplyr::left_join(taxonomy$taxonomy) %>%
      dplyr::filter(!is.na(taxa)) %>%
      dplyr::filter(rank <= required_rank) %>%
      dplyr::inner_join(df) %>%
      dplyr::select(taxa
                    , tidyselect::any_of(context)
                    , tidyselect::any_of(extra_cols)
                    ) %>%
      dplyr::distinct()

    bio_taxa_cov <- if(do_cov) {

      make_cover(bio_taxa
                 , taxa_col = "taxa"
                 , context = context
                 , lucov = lucov
                 )

      }

    bio_taxa_life <- if(do_life) {

      make_lifeform(bio_taxa
                    , taxa_col = "taxa"
                    , context = context
                    , lulife = lulife
                    )

    }

    bio_taxa <- bio_taxa %>%
      dplyr::distinct(dplyr::across(tidyselect::any_of(context))
                      , dplyr::across(tidyselect::any_of(extra_cols))
                      , taxa
                      ) %>%
      {if(do_cov) (.) %>% dplyr::left_join(bio_taxa_cov) else (.)} %>%
      {if(do_life) (.) %>% dplyr::left_join(bio_taxa_life) else (.)}

    return(bio_taxa)

  }

