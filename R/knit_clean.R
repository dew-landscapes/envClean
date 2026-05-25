#' Knit child documents to report the standard envCleaned process
#'
#' Use with knitr chunk option `results="asis"`.
#' Note: some files include named R objects to output the currently-used settings, and the knit will
#' error if these are missing. For example, the 'core' clean files require `scale` to describe
#' extent, and  `settings_clean` for the fbd_dist setting. Set these in the report environment before using `report_clean`.
#'
#' @param clean_summary Tibble containing names of clean process outputs (e.g. clean_start, bio_bin,
#'   bio_clean, etc), As output from envTargets::summarise_store_object
#' @param heading_level What level of subheading to set the output sections. E.g. default 2 will use
#'   "##" to separate sections
#' @param scale List with elements `extent` and `grain` as expected by many of
#' the clean Rmd files. If `NULL` (default) an attempt will be made to generate
#' `scale` via `envFunc::extract_scale(element = basename(here::here()))`
#'
#' @returns
#' @export
#'
knit_clean <- function(clean_summary,
                         heading_level = 3
                       , scale = NULL
                       ) {

  if(is.null(scale)) scale <- envFunc::extract_scale(element = basename(here::here()))

  text <- NULL

  for(file in clean_summary$rmd) {

    text <- c(text
              , knitr::knit_child(fs::path(system.file("rmd", package = "envClean"), file)
                                  , quiet = TRUE
                                  )
              )

  }

  rep_str <- paste0("\\1", strrep("#", heading_level))

  text <- gsub("(?<![({])#", rep_str, text, perl = TRUE)

  cat(text
      , sep = "\n\n"
      )

}
