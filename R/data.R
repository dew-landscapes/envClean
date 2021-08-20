

#' Dataframe of taxonomic ranks
#'
#' A dataset listing taxonomic rank as an ordered factor.
#'
#' @format A data frame with 53940 rows and 10 variables:
#' \describe{
#'   \item{rank}{Taxonomic rank}
#'   \item{sort}{Numeric. Can be used directly to sort taxonomic ranks but is
#'   also used to create the ordered factor rank.}
#'   ...
#' }
"lurank"


#' Lookup for RELIABNR field from BDBSA
#'
#' `RELIABNR` field in the [Biological Databases of South Australia](https://www.environment.sa.gov.au/topics/Science/Information_data/Biological_databases_of_South_Australia)
#' describes the reliability of the spatial coordinates associated with any
#' record. In `lurelBDBSA` the codes from `RELIABNR` are mapped directly to a maximum
#' distance in metres allowing more easy filtering of desired spatial accuracy.
#'
#' @format A data frame with 26 rows and 2 variables:
#' \describe{
#'   \item{rel}{`RELIABNR` field from BDBSA}
#'   \item{max_dist}{Maximum spatial reliability distance in metres.}
#'   ...
#' }
"lurelBDBSA"
