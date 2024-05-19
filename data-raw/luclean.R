

  luclean <- tibble::tribble(~clean, ~desc,
                             "all", "starting point",
                             "tem_range", "within a date range",
                             "tem_bin", "assigning dates to temporal bins",
                             "tem_rel", "temporal reliability",
                             "geo_range", "within a geographic area",
                             "geo_bin", "assigning locations to spatial bins",
                             "geo_rel", "spatial reliability",
                             "geo", "add geographic context (e.g. IBRA)",
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
                             "lists", "add list length",
                             "filt_list_df", "filter occurrence data to a set of criteria",
                             "fst", "fix spatial taxonomy",
                             "fbd", "filter by distribution",
                             "pres", "presences only",
                             "coord", "centroids of state, capital and institutions",
                             "ind", "indigenous species",
                             "rm", "geographic reliability",
                             "include", "taxa with presences, reliable distributions, and/or mcp around presences",
                             "bin", "reduce to distinct rows"
                             ) %>%
    dplyr::mutate(order = dplyr::row_number() - 1) %>%
    dplyr::arrange(order)
