

lufilter <- tibble::tribble(~filter, ~desc,
                    "all", "starting point",
                    "aoi", "area of interest",
                    "time", "within a specified data range",
                    "cell", "as a byproduct of converting sites to cells",
                    "rel", "spatial reliability",
                    "ann", "non-persistent taxa",
                    "taxa", "taxonomy",
                    "single", "singletons",
                    "filt_env", "as a byproduct of filtering cells with any `NA` value for an environmental variable",
                    "out", "outliers",
                    "NA", "NA values in important columns",
                    "effort", "context effort",
                    "prop","proportion of sites",
                    "life", "as a byproduct of assigning all records of a taxa a lifeform",
                    "cov", "as a byproduct of assigning all records of a taxa a cover value",
                    "recent", "the most recent visit to a cell",
                    "tidy", "last check for singleton sites"
                    ) %>%
  dplyr::mutate(order = dplyr::row_number() - 1) %>%
  dplyr::arrange(order)
