
  luRank <- tibble::tribble(
    ~Rank, ~sort
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
    dplyr::mutate(Rank = forcats::fct_reorder(Rank,sort)
                  , Rank = factor(Rank,ordered = TRUE)
                  , Rank = forcats::fct_rev(Rank)
                  )
