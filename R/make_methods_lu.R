#' Make standardised collection methods lookup
#'
#'
#' @param df Dataframe with methods variants.
#' @param methods_key Dataframe with 'method_gp' column for the standardised methods, 'terms' column for the search terms
#' used to define each standard method_gp, and 'rank' column with integers specifying which method_gp to use over others
#' if there is overlap in search terms (where higher numbers indicate those to use over lower numbers).
#' @param method_col Character. Name of column in `df` with method.
#' @param taxonomy List resulting from call to `make_taxonomy()`.
#' @param taxa_col Character. Name of column in `df` with taxa. Used for matching to `taxonomy` and generating 'tax_gp'
#' column in output, indicating which taxonomic groups each method has been recorded for.
#'
#' @return Dataframe with original 'method' column and standardised 'method_gp' column based on the supplied `methods_key`.
#' Also, 'tax_gp' column indicating the taxonomic group for which the methods were recorded, and
#' 'method_n' column indicating the number of occurrences for the original 'method' in the 'tax_gp'.
#' @export
#'
#' @examples

make_methods_lu <- function(df
                            , methods_key
                            , method_col = "method"
                            , taxonomy
                            , taxa_col = "original_name"
                            , unassigned = "observed"
)
{

  methods_df <- df |>
    dplyr::rename(tidyr::all_of(c(method = method_col
                                  , original_name = taxa_col))
    ) |>
    dplyr::distinct(original_name, method) |>
    dplyr::filter(!is.na(method)
                  , !method %in% c("", " ")
    ) |>
    dplyr::left_join(taxonomy$lutaxa |>
                       dplyr::distinct(original_name, taxa)
    ) |>
    dplyr::left_join(taxonomy$taxonomy |>
                       dplyr::distinct(taxa, kingdom, phylum)
    ) |>
    dplyr::filter(!is.na(kingdom)
                  , !is.na(phylum)
    ) |>
    dplyr::mutate(tax_gp = dplyr::case_when(kingdom != "Animalia" ~ "Non-animal"
                                            , kingdom == "Animalia" & phylum != "Chordata" ~ "Invertebrates"
                                            , kingdom == "Animalia" & phylum == "Chordata" ~ "Vertebrates"
                                            , .default = NA
    )
    ) |>
    dplyr::count(method, tax_gp, name = "method_n") |>
    dplyr::arrange(desc(method_n))

  res <- methods_df |>
    #dplyr::slice(1) |> # for testing
    dplyr::mutate(method_gp = purrr::map_chr(method, \(x) {

      purrr::pmap(list(x
                       , methods_key$terms
                       , methods_key$method_gp
      )
      , \(a, b, c) {

        tibble::tibble(meth = ifelse(grepl(paste0(b, collapse = "|"), a, ignore.case = TRUE)
                                     , c
                                     , NA
        )
        )|>
          dplyr::mutate(meth = as.character(meth))

      }
      ) |>
        dplyr::bind_rows() |>
        dplyr::filter(!is.na(meth)) %>%
        {if(nrow(.) == 0) dplyr::bind_rows(., tibble::tibble(meth = "observed")) else .} %>%
        {if(nrow(.) > 1) dplyr::left_join(., methods_key |>
                                            dplyr::select(method_gp, rank)
                                          , by = c("meth" = "method_gp")
        ) |>
            dplyr::filter(rank == max(rank, na.rm = TRUE)) |>
            dplyr::summarise(meth = stringr::str_flatten_comma(unique(meth))) else .} |>
        dplyr::pull()

    }
    )
    ) |>
    dplyr::distinct() |>
    dplyr::left_join(methods_df) |>
    dplyr::arrange(method_gp, desc(method_n), method)

  return(res)

}
