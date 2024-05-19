
  # these taxa appeared to be duplicated after cleaning with gbif taxonomy backbone.
  # now includes other fixes...
  # swap `original_name` to `prefer` before feeding to make_taxonomy

  # fixes ------

  taxonomy_fixes <- tibble::tribble(
    ~resolved_to, ~prefer, ~note,
    "Calyptorhynchus funereus", "Zanda funerea", "Yellow-tailed Black-Cockatoo",
    "Cracticus tibicen", "Gymnorhina tibicen", "Australian Magpie",
    "Macropus rufogriseus", "Notamacropus rufogriseus", "Red-necked Wallaby",
    "Cacatua leadbeateri", "Lophochroa leadbeateri", "Pink Cockatoo",
    "Coracina tenuirostris", "Edolisoma tenuirostre", "Common Cicadabird",
    "Glossopsitta pusilla", "Parvipsitta pusilla", "Little Lorikeet",
    "Psephotellus varius", "Psephotus varius", "Mulga Parrot",
    "Glossopsitta porphyrocephala", "Parvipsitta porphyrocephala", "Purple-crowned Lorikeet",
    "Aegintha temporalis", "Neochmia temporalis", "Red-browed Finch",
    "Pandion cristatus", "Pandion haliaetus", "Osprey",
    "Anthus novaeseelandiae", "Anthus australis", "Australian Pipit",
    "Petroica multicolor","Petroica boodang","Scarlet Robin", # Norfolk Island vs Mainland
    "Ninox novaeseelandiae","Ninox boobook","Australian Boobook", # Norfolk Island vs Mainland
    #"Pachycephala occidentalis","Pachycephala fuliginosa","Western Whistler",
    "Pachycephala occidentalis","Pachycephala pectoralis","Golden Whistler",
    "Pachycephala fuliginosa","Pachycephala pectoralis","Golden Whistler",
    "Rhipidura fuliginosa","Rhipidura albiscapa","Grey Fantail", # Rhipidura fuliginosa is New Zealand Fantail erroneously recorded as Grey Fantail in Aus mainly in last ten years via iNaturalist.
    "Tyto alba", "Tyto javanica", "Eastern Barn Owl", # same species (used to be Tyto alba javanica); consensus seems to be javanica now
    "Acrocephalus stentoreus", "Acrocephalus australis", "Australian Reed Warbler", # Clamorous Reed Warbler records should be Australian
    "Sminthopsis griseoventer", "Sminthopsis fuliginosus", "EP/KI Dunnarts", # according to cited ref in BDBSA taxonomy, both species should be S.fuliginosus, just separate subspecies on KI and EP. Although, EPBC suggests both should be S.griseoventer, and redlist suggests separate species but does not have species level distribution for S.fuliginosus - only KI ssp. Resolving all to S.fuliginosus avoids reference to S.griseoventer distribution from redlist, which would remove all KI records in filter_by_distribution if all records changed to S.griseoventer.
    "Herniaria hirsuta", "Herniaria cinerea", "Rupturewort" # according to Flora of Australia (website) these are the same, but BDBSA taxonomy recognises H.cinerea and using this allows it to be attributed correctly to non-indigenous and filtered out
    )


  # overrides -------

  taxonomy_overrides <- tibble::tribble(
    ~original, ~prefer, ~note,
    "Thinornis rubricollis", "Thinornis cucullatus", "Hooded Plover",
    "Charadrius rubricollis", "Thinornis cucullatus", "Hooded Plover",
    "Thinornis rubricollis rubricollis", "Thinornis cucullatus", "Hooded Plover",
    "Gypsophila australis","Gypsophila","Chalkwort"
  )
