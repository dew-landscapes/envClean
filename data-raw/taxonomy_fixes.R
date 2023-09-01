
  # these taxa appeared to be duplicated after cleaning with gbif taxonomy backbone.
  # now includes other fixes...
  # swap `original_name` to `prefer` before feeding to make_taxonomy

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
    "Pachycephala pectoralis", "Pachycephala fuliginosa", "Western Whistler",
    "Petroica multicolor","Petroica boodang","Scarlet Robin", # Norfolk Island vs Mainland
    "Ninox novaeseelandiae","Ninox boobook","Australian Boobook", # Norfolk Island vs Mainland
    "Pachycephala occidentalis","Pachycephala fuliginosa","Western Whistler", # another variant
    "Rhipidura fuliginosa","Rhipidura albiscapa","Grey Fantail", # Rhipidura fuliginosa is New Zealand Fantail erroneously recorded as Grey Fantail in Aus mainly in last ten years via iNaturalist.
    "Tyto alba","Tyto javanica","Eastern Barn Owl", # same species (used to be Tyto alba javanica); consensus seems to be javanica now
    "Acrocephalus stentoreus","Acrocephalus australis","Australian Reed Warbler" # Clamorous Reed Warbler records should be Australian
  )
