#' Knit child documents to report the standard envCleaned process
#'
#' Use with knitr chunk option `results="asis"`.
#' Note: some files include named R objects to output the currently-used
#' settings, and the knit will error if these are missing. For example, the
#' 'core' clean files require the list `settings` with elements `extent` and
#' `grain`, and  `settings_clean` for the fbd_dist setting. Set these in the
#' report environment before using `report_clean`.
#'
#' @param clean_summary Tibble containing names of clean process outputs (e.g.
#' clean_start, bio_bin, bio_clean, etc), As output from
#' `envTargets::summarise_store_object()`
#' @param heading_level What level of subheading to set the output sections.
#' e.g. default 3 will result in "###" heading level at the start of each clean
#' Rmd
#'
#' @returns as for `knitr::knit_child()`
#' @export
#'
knit_clean <- function(clean_summary,
                         heading_level = 3
                       ) {

  text <- NULL

  for(file in clean_summary$rmd) {

    text <- c(text
              , knitr::knit_child(fs::path(system.file("rmd", package = "envClean"), file)
                                  , quiet = TRUE
                                  )
              )

  }

  rep_str <- paste0("\\1", strrep("#", heading_level - 1))

  text <- gsub("(?<![({])(#+)", rep_str, text, perl = TRUE)

  cat(text
      , sep = "\n\n"
      )

}
