#' How many records, visits, sites and taxa in a dataframe
#'
#' @param df Dataframe with taxa records.
#' @param site_cols Name of column(s) defining sites.
#' @param visit_cols Name of column(s) defining visits.
#' @param taxa_cols Name of column(s) defining taxa.
#'
#' @return One row dataframe
#' @export
#'
#' @examples
rec_vis_sit_tax <- function(df
                            , site_cols
                            , visit_cols
                            , taxa_cols
                            ) {

  df %>%
    dplyr::summarise(taxa = n_distinct(across(any_of(taxa_cols)))
                     , records = nrow(.)
                     , visits = n_distinct(across(any_of(visit_cols)))
                     , sites = n_distinct(across(any_of(site_cols)))
                     )

}
