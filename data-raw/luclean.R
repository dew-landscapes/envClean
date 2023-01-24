

  luclean <- tibble::tribble(~clean, ~desc,
                             "all", "starting point",
                             "tem_range", "within a date range",
                             "tem_bin", "assigning dates to temporal bins",
                             "tem_rel", "temporal reliability",
                             "geo_range", "within a geographic area",
                             "geo_bin", "assigning locations to spatial bins",
                             "geo_rel", "spatial reliability",
                             "ann", "non-persistent taxa",
                             "taxa", "align taxonomy and resolve any taxonomic duplication within bins",
                             "single", "singletons",
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
