

  test_taxa <- tibble::tibble(original_name = c("Aedes notoscriptus"
                                                , "Aedes notoscriptus (Skuse, 1889)"
                                                , "123 456 4"
                                                , "annual herb"
                                                , "Actinia tenebrosa"
                                                , "Actinia tenebrosa Farquhar, 1898"
                                                , "1-2-13"
                                                , "1-May-2012"
                                                )
                              )

  out_file = paste0(tempfile(), ".rds")


  for(i in 1:100) {

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


  }

  mget(ls(pattern = "res\\d{2}_size"))


