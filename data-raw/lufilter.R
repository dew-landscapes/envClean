

lufilter <- tibble::tribble(~filter, ~desc, ~order,
                    "all", "starting point", 1,
                    "ann", "'non-persistent taxa'", 7,
                    "aoi", "'area of interest'", 2,
                    "cell", "as a byproduct of converting sites to cells", 3,
                    "cov", "as a byproduct of assigning all records of a taxa a cover value", 11,
                    "life", "as a byproduct of assigning all records of a taxa a lifeform", 10,
                    "filt_env", "as a byproduct of filtering cells with any `NA` value for an environmental variable", 13,
                    "out", "'outliers'", 4,
                    "prop","'proportion of sites'", 9,
                    "recent", "the most recent visit to a cell", 12,
                    "rel", "'spatial reliability'", 6,
                    "single", "'singleton sites'", 8,
                    "effort", "'cell effort'", 9,
                    "taxa", "'taxonomy'", 5,
                    "tidy", "last check for singleton sites", 14
                    ) %>%
  dplyr::arrange(order)
