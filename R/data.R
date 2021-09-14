#' Dataframe of filtering steps
#'
#'
#' @format A data frame with `r nrow(lufilter)` rows and `r ncol(lufilter)`
#' variables:
#' \describe{
#'   \item{filter}{Character short name for filtering step.}
#'   \item{desc}{Phrase describing filtering step.}
#'   ...
#' }
"lufilter"

#' Dataframe of taxonomic ranks
#'
#' A dataset listing taxonomic rank as an ordered factor.
#'
#' @format A data frame with `r nrow(lurank)` rows and `r ncol(lurank)`
#' variables:
#' \describe{
#'   \item{rank}{Taxonomic rank.}
#'   \item{sort}{Numeric. Can be used directly to sort taxonomic ranks but is
#'   also used to create the ordered factor rank.}
#'   ...
#' }
"lurank"

