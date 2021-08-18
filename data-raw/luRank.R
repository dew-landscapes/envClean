
  library(magrittr)

  lurank <- tibble::tribble(
    ~rank, ~sort
    , "Kingdom", 1
    , "Phylum", 2
    , "Class", 3
    , "Order", 4
    , "Family", 5
    , "Genus", 6
    , "Species", 7
    , "Subspecies", 8
    , "Variety", 9
    , "Form", 10
    ) %>%
    dplyr::mutate(rank = forcats::fct_reorder(rank,sort)
                  , rank = factor(rank,ordered = TRUE)
                  , rank = forcats::fct_rev(rank)
                  )
