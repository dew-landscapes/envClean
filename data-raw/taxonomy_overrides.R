
# these taxa appear incorrectly matched by galah taxonomy

# overrides -------

taxonomy_overrides <- tibble::tribble(
  ~original_name, ~taxa_to_search, ~use_species, ~use_subspecies, ~note,
  "Charadrius rubricollis", "Thinornis cucullatus", "Thinornis cucullatus", NA, "Hooded Plover",
  "Sminthopsis fuliginosus aitkeni","Sminthopsis fuliginosa aitkeni","Sminthopsis fuliginosa","Sminthopsis fuliginosa aitkeni","KI Dunnart", # using "fuliginosus" not "fuliginosa" returns incorrect taxa
  "Sminthopsis fuliginosus","Sminthopsis fuliginosa","Sminthopsis fuliginosa", NA,"EP/KI Dunnarts", # using "fuliginosus" not "fuliginosa" returns incorrect taxa
)
