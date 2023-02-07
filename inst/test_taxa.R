

  test_taxa <- tibble::tibble(original_name = c("Melithreptus gularis"
                                                , "Melithreptus gularis gularis"
                                                , "Tympanocriptis lineata"
                                                , "Tympanocryptis lineata pinguicolla"
                                                , "Melithreptus gularis laetior"
                                                , "Aedes notoscriptus"
                                                , "Aedes notoscriptus (Skuse, 1889)"
                                                , "123 456 4"
                                                , "annual herb"
                                                )
                              )

  out_file = paste0(tempfile(), ".rds")

  res01 <- get_gbif_tax(test_taxa
                       , out_file = out_file
                       , king_type = "Animalia"
                       , do_common = TRUE
                       )

  res01_size <- file.size(out_file)

  res02 <- get_gbif_tax(test_taxa
                         , out_file = out_file
                         , king_type = "Animalia"
                         , do_common = TRUE
                         )

  res02_size <- file.size(out_file)

  res03 <- make_taxa_taxonomy(test_taxa
                              , out_file = out_file
                              , king_type = "Animalia"
                              , do_common = TRUE
                              )

  res03_size <- file.size(out_file)

  res04 <- filter_taxa(test_taxa
                       , out_file = out_file
                       , king_type = "Animalia"
                       , do_common = TRUE
                       , context = NULL
                       )

  res04_size <- file.size(out_file)

  mget(ls(pattern = "res\\d{2}_size"))


