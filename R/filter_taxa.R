

#' Clean/Tidy to one row per taxa*Visit
#'
#' Includes running of taxa_taxonomy(), if lutaxa does not already exist.
#'
#' @param df Dataframe to reduce.
#' @param taxa_col Character. Name of column with taxa.
#' @param context Character. Name of columns defining context.
#' @param extra_cols Character. Name of any extra columns to keep.
#' @param target_rank Character. Name of level in taxonomic hierarchy that names
#' should be retrieved from, if possible.
#' @param do_cov Logical. Should cover (needs to be supplied in df) be appended
#' to output.
#' @param do_life Logical. Should lifeform (needs to be supplied in df) be
#' appended to output.
#' @param do_ind Logical. Should ind (needs to be supplied in df) be
#' appended to output.
#' @param lucov Dataframe lookup for cover.
#' @param lulife Dataframe lookup for lifeform.
#' @param taxonomy list with named elements `lutaxa` and `taxonomy`.
#' Usually resulting from call to `envClean::make_taxonomy()`.
#' @param target_rank Character. Default is 'species'. At what level of the
#' taxonomic hierarchy are results desired.
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
                          , do_cov = FALSE
                          , do_life = FALSE
                          , lucov = NULL
                          , lulife = NULL
                          , taxonomy
                          , target_rank = "species"
                          ) {

    df <- df %>%
      dplyr::mutate(original_name = !!rlang::ensym(taxa_col))

    # Use dftaxa as base df from here
    bio_taxa <- df %>%
      dplyr::distinct(original_name) %>%
      dplyr::left_join(taxonomy$lutaxa) %>%
      dplyr::left_join(taxonomy$taxonomy) %>%
      dplyr::filter(!is.na(taxa)) %>%
      dplyr::filter(rank <= target_rank) %>%
      dplyr::inner_join(df) %>%
      dplyr::select(tidyselect::any_of(context)
                    , taxa
                    , tidyselect::all_of(extra_cols)
                    , tidyselect::any_of(c("cover", "cover_code"))
                    ) %>%
      dplyr::distinct()

    bio_taxa_cov <- if(do_cov) {

      .context = context
      .lucov = lucov

      make_cover(bio_taxa
                   , context = .context
                   , lucov = .lucov
                   )

      } else bio_taxa

    bio_taxa_life <- if(do_life) {

      .context = context
      .lulife = lulife

      make_lifeform(bio_taxa
                      , context = .context
                      , lulife = .lulife
                      )

    } else bio_taxa

    bio_taxa <- bio_taxa %>%
      dplyr::distinct(dplyr::across(tidyselect::any_of(context))
                      , taxa
                      ) %>%
      {if(do_cov) (.) %>% dplyr::left_join(bio_taxa_cov) else (.)} %>%
      {if(do_life) (.) %>% dplyr::left_join(bio_taxa_life) else (.)}

    return(bio_taxa)

  }

