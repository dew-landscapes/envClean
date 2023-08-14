
#' Title
#'
#' @param df Dataframe with `taxa_col` and `att_col`
#' @param taxa_col Character name of column in `df` that was passed to `get_taxonomy` as `taxa_col`
#' @param att_col Character name of column in `df` that contains the attribute to summarise
#' @param taxonomy List resulting from call to `make_taxonomy()`
#' @param max_guess Character. If attribute is not available for `taxa`, try guessing from values up to `max_guess` level of taxonomic hierarchy. See `lurank`
#' @param agg_method Function name to use for summarising numeric `att_col`. Ignored if `att_col` is not numeric
#' @param agg_round Passed to `base::round()` round argument. Used if summarising numeric `att_col`.
#' @param att_default Default value used for `att_col` when no other value can be found/guessed.
#' @param context Any other columns in `df` to maintain throughout summarising.
#' @param ... Passed to `agg_method`
#'
#' @return Dataframe with one row for each taxa and context with best guess at a single attribute based on the values in `att_col`
#' @export
#'
#' @examples
  make_attribute <- function(df
                             , taxa_col = "original_name"
                             , att_col
                             , taxonomy
                             , max_guess = "family"
                             , agg_method = median
                             , agg_round = 2
                             , att_default = "Unknown"
                             , context = NULL
                             , ...
                             ) {

    tax_levels <- lurank %>%
      dplyr::filter(rank <= max_guess) %>%
      dplyr::pull(rank) %>%
      as.vector()

    attribute_df <- df %>%
      dplyr::filter(!is.na(!!rlang::ensym(att_col))) %>%
      dplyr::rename(original_name = !!rlang::ensym(taxa_col)
                    , att_col = !!rlang::ensym(att_col)
                    ) %>%
      dplyr::left_join(taxonomy$lutaxa) %>%
      dplyr::left_join(taxonomy$taxonomy) %>%
      dplyr::select(kingdom
                    , taxa
                    , best_key
                    , att_col
                    , tidyselect::any_of(tax_levels)
                    , tidyselect::any_of(context)
                    )

    tax_levels_list <- tax_levels %>%
      intersect(names(attribute_df)) %>%
      Map(function(x) NULL, .)

    for(i in 1:length(tax_levels_list)) {

      this_level <- names(tax_levels_list[i])

      att_class <- class(attribute_df$att_col)

      this_att_name <- paste0(this_level, "_att")

      this_att_vals <- paste0(this_level, "_vals")

      this_att_from <- paste0(this_level, "_from")

      if(att_class == "numeric") {

        tax_levels_list[[this_level]] <- attribute_df %>%
          dplyr::group_by(!!rlang::ensym(this_level)
                          , dplyr::across(tidyselect::any_of(context))
                          ) %>%
          dplyr::summarise(!!rlang::ensym(this_att_name) := agg_method(att_col
                                                                 , ...
                                                                 )
                           , !!rlang::ensym(this_att_vals) := paste0(round(min(att_col, na.rm = TRUE), agg_round)
                                                                     , " to "
                                                                     , round(max(att_col, na.rm = TRUE), agg_round)
                                                                     )
                           , !!rlang::ensym(this_att_from) := this_level
                           ) %>%
          dplyr::ungroup() %>%
          dplyr::distinct() %>%
          dplyr::filter(!is.na(!!rlang::ensym(this_level)))

      }

      if(att_class != "numeric") {

        tax_levels_list[[this_level]] <- attribute_df %>%
          dplyr::count(dplyr::across(!!rlang::ensym(this_level))
                       , att_col
                       , dplyr::across(tidyselect::any_of(context))
                       ) %>%
          dplyr::group_by(!!rlang::ensym(this_level)
                          , dplyr::across(tidyselect::any_of(context))
                          ) %>%
          dplyr::summarise(!!rlang::ensym(this_att_name) := att_col[which.max(n)]
                           , !!rlang::ensym(this_att_vals) := vec_to_sentence(unique(att_col))
                           , !!rlang::ensym(this_att_from) := this_level
                           ) %>%
          dplyr::ungroup() %>%
          dplyr::filter(!is.na(!!rlang::ensym(this_level)))

      }

    }

    att_result <-
      purrr::reduce(c(list(taxonomy$taxonomy %>%
                             dplyr::select(kingdom
                                           , taxa
                                           , tidyselect::any_of(names(tax_levels_list))
                                           )
                           )
                      , tax_levels_list
                      )
                    , dplyr::left_join
                    ) %>%
      dplyr::select(kingdom
                    , taxa
                    , tidyselect::any_of(context)
                    , tidyselect::contains(paste0(names(tax_levels_list), "_"))
                    ) %>%
      tidyr::pivot_longer(tidyselect::contains(paste0(names(tax_levels_list), "_"))
                          , names_to = c("name", ".value")
                          , names_sep = "_"
                          , values_drop_na = TRUE
                          ) %>%
      dplyr::left_join(lurank
                       , by = c("from" = "rank")
                       ) %>%
      dplyr::group_by(kingdom
                      , dplyr::across(tidyselect::any_of(context))
                      , taxa
                      ) %>%
      dplyr::filter(sort == max(sort)) %>%
      dplyr::ungroup() %>%
      dplyr::select(kingdom
                    , tidyselect::any_of(context)
                    , taxa
                    , att = att
                    , att_vals = vals
                    , att_from = from
                    ) %>%
      purrr::set_names(gsub("att", att_col, names(.)))

    return(att_result)

  }
