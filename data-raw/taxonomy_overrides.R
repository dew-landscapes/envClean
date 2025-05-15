
# these taxa appear incorrectly matched by galah taxonomy

# overrides -------

taxonomy_overrides <- tibble::tribble(
  ~original_name, ~taxa_to_search, ~use_species, ~use_subspecies, ~note,
  "Charadrius rubricollis", "Thinornis cucullatus", "Thinornis cucullatus", NA, "Hooded Plover",
  "Thinornis rubricollis rubricollis", "Thinornis cucullatus cucullatus", "Thinornis cucullatus", "Thinornis cucullatus cucullatus", "Eastern Hooded Plover", # EPBC name for Eastern Hooded Plover is Thinornis rubricollis rubricollis which reverts to species with galah::search_taxa
  "Sminthopsis fuliginosus aitkeni", "Sminthopsis fuliginosa aitkeni", "Sminthopsis fuliginosa", "Sminthopsis fuliginosa aitkeni", "KI Dunnart", # using "fuliginosus" not "fuliginosa" returns incorrect taxa
  "Sminthopsis fuliginosus", "Sminthopsis fuliginosa","Sminthopsis fuliginosa", NA, "EP/KI Dunnarts", # using "fuliginosus" not "fuliginosa" returns incorrect taxa
  "Gallirallus philippensis mellori", "Hypotaenidia philippensis mellori", "Hypotaenidia philippensis", "Hypotaenidia philippensis mellori", "Australian Buff-banded Rail", # from ALA: now regarded as Hypotaenidia philippensis. H.p.mellori is accepted, but galah::search_taxa reverts to species. original_name in
  "Grus rubicunda", "Antigone rubicunda","Antigone rubicunda",NA, "Brolga",
  "Eucalyptus X paludicola", "Eucalyptus paludicola", "Eucalyptus paludicola", NA, "Marsh Gum/Mt Compass Swamp Gum",
  "Eucalyptus x paludicola", "Eucalyptus paludicola", "Eucalyptus paludicola", NA, "Marsh Gum/Mt Compass Swamp Gum",
  "Corybas X dentatus", "Corybas dentatus", "Corybas dentatus", NA, "Finniss/Toothed Helmet-orchid",
  "Corybas x dentatus", "Corybas dentatus", "Corybas dentatus", NA, "Finniss/Toothed Helmet-orchid",
  "Melanodryas cucullata westralis", "Melanodryas cucullata westralensis", "Melanodryas cucullata", "Melanodryas cucullata westralensis", "Western Hooded Robin", # different spelling in EGIS expert distributions layer that is not matched to subspecies by galah::search_taxa
  "Strepera versicolor malanoptera", "Strepera versicolor melanoptera", "Strepera versicolor", "Strepera versicolor melanoptera", "Black-winged Currawong", # different spelling in EGIS expert distributions layer that is not matched to subspecies by galah::search_taxa
  "Rhipidura fuliginosa", "Rhipidura albiscapa", "Rhipidura albiscapa", NA, "Grey Fantail", # Rhipidura fuliginosa is New Zealand Fantail erroneously recorded as Grey Fantail in Aus mainly in last ten years via iNaturalist.
  "Rhipidura (Rhipidura) fuliginosa", "Rhipidura albiscapa", "Rhipidura albiscapa", NA, "Grey Fantail", # Rhipidura fuliginosa is New Zealand Fantail erroneously recorded as Grey Fantail in Aus mainly in last ten years via iNaturalist.
  "Acrocephalus stentoreus", "Acrocephalus australis", "Acrocephalus australis", NA, "Australian Reed Warbler", # Clamorous Reed Warbler records should be Australian
  "Porzana pusilla", "Zapornia pusilla", "Zapornia pusilla", NA, "Baillon's Crake",
  "Anas clypeata", "Spatula clypeata", "Spatula clypeata", NA, "Northern Shoveler",
  "Dacelo novaehollandiae", "Dacelo novaeguineae", "Dacelo novaeguineae", NA, "Laughing Kookaburra",
  "Puffinus griseus", "Ardenna grisea", "Ardenna grisea", NA, "Sooty Shearwater",
  "Aptenodes patagonicus", "Aptenodytes patagonicus", "Aptenodytes patagonicus", NA, "King Penguin",
  "Puffinis gravis", "Ardenna gravis", "Ardenna gravis", NA, "Great Shearwater",
  "Sericornis maculatus", "Sericornis maculatus", "Sericornis maculatus", NA, "Spotted Scrubwren", # former subspecies now accepted as a species by Birds SA and BDBSA taxonomy
  "Sericornis frontalis maculatus", "Sericornis maculatus", "Sericornis maculatus", NA, "Spotted Scrubwren",
  "Sericornis (Sericornis) frontalis maculatus", "Sericornis maculatus", "Sericornis maculatus", NA, "Spotted Scrubwren",
  "Sericornis frontalis ashbyi", "Sericornis maculatus ashbyi", "Sericornis maculatus", "Sericornis maculatus ashbyi", "Spotted Scrubwren",
  "Sericornis (Sericornis) frontalis ashbyi", "Sericornis maculatus ashbyi", "Sericornis maculatus", "Sericornis maculatus ashbyi", "Spotted Scrubwren",
  "Sericornis frontalis mellori", "Sericornis maculatus mellori", "Sericornis maculatus", "Sericornis maculatus mellori", "Spotted Scrubwren",
  "Sericornis (Sericornis) frontalis mellori", "Sericornis maculatus mellori", "Sericornis maculatus", "Sericornis maculatus mellori", "Spotted Scrubwren"
  )
