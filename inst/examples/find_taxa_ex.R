

# library(envClean)

# Example taxa
use_taxa <- "Eucalyptus gracilis"

# Set context
context <- c("lat", "long", "month", "year")

# Start
flor_start <- flor_all %>%
  tibble::as_tibble() %>%
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

# make_taxonomy
taxa <- make_taxonomy(df = flor_recent)

# bin taxa
flor_taxa <- flor_recent %>%
  bin_taxa(taxonomy = taxa$species)

# distinct over bins
flor_bin <- flor_taxa %>%
  dplyr::distinct(taxa, dplyr::across(tidyselect::any_of(context)))

# How did records of 'taxa' change through the filtering?
# Eucalyptus gracilis
find_taxa("Eucalyptus gracilis"
          , lutaxa = taxa$species$lutaxa
          )

# Chenopodium nutans
find_taxa("Chenopodium nutans"
          , lutaxa = taxa$species$lutaxa
          )
