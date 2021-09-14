
  library(magrittr)

  lurank <- tibble::tribble(
    ~rank, ~sort
    , "kingdom", 1
    , "phylum", 2
    , "class", 3
    , "order", 4
    , "family", 5
    , "genus", 6
    , "species", 7
    , "subspecies", 8
    , "variety", 9
    , "form", 10
    ) %>%
    dplyr::mutate(rank = forcats::fct_reorder(rank,sort)
                  , rank = factor(rank,ordered = TRUE)
                  , rank = forcats::fct_rev(rank)
                  )
