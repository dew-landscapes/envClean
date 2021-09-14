

lufilter <- tibble::tribble(~filter, ~desc,
                    "all", "starting point",
                    "ann", "'non-persistent taxa'",
                    "aoi", "'area of interest'",
                    "cell", "as a byproduct of converting sites to cells",
                    "cov", "as a byproduct of assigning all records of a taxa a cover value",
                    "life", "as a byproduct of assigning all records of a taxa a lifeform",
                    "filt_env", "as a byproduct of filtering cells with any `NA` value for an environmental variable",
                    "out", "'outliers'",
                    "prop","'proportion of sites'",
                    "recent", "the most recent visit to a cell",
                    "rel", "'spatial reliability'",
                    "single", "'singleton sites'",
                    "effort", "'cell effort'",
                    "taxa", "'taxonomy'",
                    "tidy", "last check for singleton sites"
                    )
