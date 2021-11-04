
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
#' Example resulting from munging several different data sources (`data_name`)
#' into one tidy data set. This is the sort of starting point where `envClean`
#' tools can help.
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
"flor_all"

#' Dataframe of filtering steps
#'
#'
#' @format A data frame with `r nrow(lufilter)` rows and `r ncol(lufilter)`
#' variables:
#' \describe{
#'   \item{filter}{Character short name for filtering step.}
#'   \item{desc}{Phrase describing filtering step.}
#'   \item{order}{Suggested order}
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

