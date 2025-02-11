
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
  "Anas superciliosa X Anas platyrhynchos", "Anas superciliosa X platyrhynchos", "Anas superciliosa X platyrhynchos", NA, "Pacific Black Duck x Mallard hybrid", # duplicates of Pacific Black Duck x Mallard hybrid in galah taxonomy. Using the one that returns a sensible common name from bdbsa.
  "Anas superciliosa x anas platyrhynchos", "Anas superciliosa X platyrhynchos", "Anas superciliosa X platyrhynchos", NA, "Pacific Black Duck x Mallard hybrid", # duplicates of Pacific Black Duck x Mallard hybrid in galah taxonomy. Using the one that returns a sensible common name from bdbsa.
  "Anas superciliosa x Anas platyrhynchos", "Anas superciliosa X platyrhynchos", "Anas superciliosa X platyrhynchos", NA, "Pacific Black Duck x Mallard hybrid", # duplicates of Pacific Black Duck x Mallard hybrid in galah taxonomy. Using the one that returns a sensible common name from bdbsa.
  "Lolium perenne X Lolium rigidum", "Lolium perenne X rigidum",  "Lolium perenne X rigidum", NA, "Perennial Rye-Grass X Wimmera Rye-Grass Hybrid", # duplicates in galah taxonomy due to different case 'x'
  "Lolium perenne x Lolium rigidum", "Lolium perenne X rigidum",  "Lolium perenne X rigidum", NA, "Perennial Rye-Grass X Wimmera Rye-Grass Hybrid" # duplicates in galah taxonomy due to different case 'x'
  )
