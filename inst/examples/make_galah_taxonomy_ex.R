
  # setup
  # library("envClean")

  temp_file <- tempfile()

  taxa_df <- tibble::tibble(taxa = c("Charadrius rubricollis"
                                     , "Thinornis cucullatus"
                                     , "Melithreptus gularis laetior"
                                     , "Melithreptus gularis gularis"
                                     , "Eucalyptus viminalis"
                                     , "Eucalyptus viminalis cygnetensis"
                                     , "Eucalyptus"
                                     , "Charadrius mongolus all subspecies"
                                     , "Bettongia lesueur Barrow and Boodie Islands subspecies"
                                     , "Lagorchestes hirsutus Central Australian subspecies"
                                     , "Perameles gunnii Victorian subspecies"
                                     , "Pterostylis sp. Rock ledges (pl. 185, Bates & Weber 1990)"
                                     , "Spyridium glabrisepalum"
                                     , "Spyridium eriocephalum var. glabrisepalum"
                                     , "Petrogale lateralis (MacDonnell Ranges race)"
                                     , "Gehyra montium (revised)"
                                     , "Korthalsella japonica f. japonica"
                                     , "Galaxias sp. nov. 'Hunter'"
                                     , "Some rubbish"
                                     , "Senna artemisioides subsp x artemisioides"
                                     , "Halosarcia sp.  (NC)"
                                     , "TERMITOIDAE sp." # 'epifamily'
                                     )
                            )

  # make taxonomy (returns list and writes taxonomy_file)
  taxonomy <- make_taxonomy(df = taxa_df
                            , taxa_col = "taxa"
                            , taxonomy_file = temp_file
                            , needed_ranks = c("kingdom", "genus", "species", "subspecies")
                            )
  taxonomy$raw
  taxonomy$kingdom
  taxonomy$genus
  taxonomy$species
  taxonomy$subspecies

  # query more taxa (results are added to taxonomy_file but only the new taxa are returned (default `limit = TRUE`)
  more_taxa <- tibble::tibble(original_name = c("Amytornis whitei"
                                                , "Amytornis striatus"
                                                , "Amytornis modestus (North, 1902)"
                                                , "Amytornis modestus modestus"
                                                , "Amytornis modestus cowarie"
                                                )
                              )

  make_taxonomy(df = more_taxa
                , taxonomy_file = temp_file
                , needed_ranks = c("species")
                )

  # no dataframe supplied - all results in taxonomy_file returned
  make_taxonomy(taxonomy_file = temp_file
                , needed_ranks = c("subspecies")
                )

  # Try automatic overrides
  auto_overrides <- make_unmatched_overrides(df = taxa_df
                                             , taxa_col = "taxa"
                                             , taxonomy = taxonomy
                                             , target_rank = "species"
                                             )

  # overrrides
  overrides <- envClean::taxonomy_overrides

  # C. rubricollis binned to Phalarope lobatus at species level!
  taxonomy <- make_taxonomy(df = overrides
                            , taxonomy_file = temp_file
                            , needed_ranks = c("species", "subspecies")
                            )

  taxonomy$species$lutaxa %>%
    dplyr::filter(grepl("rubricollis", original_name))

  # add in override - C. rubricollis is binned to T. cucullatus at species level
  taxonomy <- make_taxonomy(df = overrides
                            , taxonomy_file = temp_file
                            , needed_ranks = c("species", "subspecies")
                            , overrides = overrides
                            )

  taxonomy$species$lutaxa %>%
    dplyr::filter(grepl("rubricollis", original_name))


  # tweak_species example
  make_taxonomy(df = tibble::tibble(original_name = "Acacia sp. Small Red-leaved Wattle (J.B.Williams 95033)")
                , tweak_species = FALSE
                )$raw %>%
    dplyr::select(original_name, scientific_name, species)

  make_taxonomy(df = tibble::tibble(original_name = "Acacia sp. Small Red-leaved Wattle (J.B.Williams 95033)")
                , tweak_species = TRUE
                )$raw %>%
    dplyr::select(original_name, scientific_name, species)

  # clean up
  rm(taxonomy)
  unlist(paste0(temp_file, ".parquet"))
