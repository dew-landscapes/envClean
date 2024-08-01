
# these taxa appear incorrectly matched by galah taxonomy

# overrides -------

taxonomy_overrides <- tibble::tribble(
  ~original_name, ~taxa_to_search, ~use_species, ~use_subspecies, ~note,
  "Charadrius rubricollis", "Thinornis cucullatus", "Thinornis cucullatus", NA, "Hooded Plover"
)
