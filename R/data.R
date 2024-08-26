
#' Simple feature to define a geographic area of interest.
#'
#' `aoi` defines an area in the northern Murray Mallee in South Australia.
#'
#' @format A simple feature with `r nrow(aoi)` rows and `r ncol(aoi)` variables:
#' \describe{
#'   \item{area}{Area of simple feature in sqare metres.}
#'   \item{geometry}{List column of geometry.}
#'   ...
#' }
"aoi"

#' Example of data combined from several data sources.
#'
#' Example GBIF occurrence data set \insertCite{GBIFRef_1,GBIFRef_6}{envClean}.
#'
#' @format A data frame with `r nrow(flor_all)` rows and `r ncol(flor_all)`
#' variables:
#' \describe{
#'   \item{lat}{Latitude in decimal degrees.}
#'   \item{long}{Longitude in decimal degrees.}
#'   \item{data_name}{Name of original data source.}
#'   \item{site}{Site name from original data source.}
#'   \item{date}{Date of observation of `original_name`.}
#'   \item{original_name}{Taxonomy retrieved from `data_name`.}
#'   \item{cover}{Cover estimate (numeric) retrieved from `data_name`.}
#'   \item{cover_code}{Cover estimate (ordinal) retrieved from `data_name`.}
#'   \item{quad_x}{Length of one quadrat edge.}
#'   \item{quad_y}{Length of other quadrat edge.}
#'   \item{rel_dist}{Reliability of `lat` and `long` in metres.}
#'   \item{month}{Month component of `date`.}
#'   \item{year}{Year component of `date`.}
#'   ...
#' }
#' @references
#'   \insertAllCited{}
"flor_all"

#' Dataframe of cleaning steps
#'
#'
#' @format A data frame with `r nrow(luclean)` rows and `r ncol(luclean)`
#' variables:
#' \describe{
#'   \item{clean}{Character short name for cleaning step.}
#'   \item{desc}{Phrase describing filtering step.}
#'   \item{order}{Suggested order}
#'   ...
#' }
"luclean"

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

#' Manual taxonomic overrides
#'
#' Can be used as argument to `envClean::make_taxa_taxonomy()`.
#'
#' @format A data frame with `r nrow(taxonomy_overrides)` rows and `r ncol(taxonomy_overrides)`
#' variables:
#' \describe{
#'   \item{original_name}{Character. Original names of taxa}
#'   \item{taxa_to_search}{Character. Try searching for this taxa (often the
#'   same as original_name)}
#'   \item{use_}{Character. Any number of `use_` columns, such as `use_species`
#'   and/or `use_subspecies`. If no match is found for `taxa_to_search` these
#'   names will be used at the rank they imply}
#'   \item{note}{Usually a rationale for the inclusion}
#'   ...
#' }
"taxonomy_overrides"
