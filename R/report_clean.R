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
#'
#' @returns
#' @export
#'
report_clean <- function(clean_summary,
                         heading_level = 2) {

  for(file in clean_summary$rmd) {

    child <- knitr::knit_child(
      fs::path(system.file("rmd", package = "envClean"), file),
      quiet = TRUE
    )

    cat(
      paste0( #insert extra '#' to section headers
        strrep("headingtext", times = heading_level - 1),
        gsub("^\\n", "", x = child),
        sep = "\n\n"
      )
    )

  }

}
