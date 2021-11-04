

library(envClean)

# Example taxa
use_taxa <- "Angianthus tomentosus"

# Set context
context <- c("lat", "long", "month", "year")

# Start
flor_start <- flor_all %>%
  envFunc::add_time_stamp()

# Remove singletons
flor_single <- flor_start %>%
  filter_counts(context = context
                , thresh = 5
                ) %>%
  envFunc::add_time_stamp()

# Just keep most recent contexts
flor_recent <- flor_single %>%
  dplyr::group_by(across(any_of(context[!context %in% c("month", "year")]))) %>%
  dplyr::filter(year == max(year)
                , month == max(month)
                ) %>%
  dplyr::ungroup() %>%
  envFunc::add_time_stamp()

# Make sure required taxonomy objects are available
make_taxa_taxonomy(df = data.frame(original_name = use_taxa))

# Filter taxonomy
  # As lutaxa only contains `use_taxa`, this removes all other taxa
  # Normally, you'd have a row in lutaxa for every taxa
flor_taxa <- flor_recent %>%
  filter_taxa(context = context)

# How did records of 'taxa' change through the filtering?
find_taxa(taxa = use_taxa)
