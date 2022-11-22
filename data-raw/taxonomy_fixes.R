
  # these taxa appeared to be duplicated after cleaning with gbif taxonomy backbone.
  # swap `original_name` to `prefer` before feeding to make_taxa_taxonomy

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
    "Pandion cristatus", "Pandion haliaetus", "Osprey"
  )
