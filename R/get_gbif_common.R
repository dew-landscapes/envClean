
  get_gbif_common <- function(key) {

    if(is.na(key)) {

      warning(paste0("key = ", key, ". Returning NA"))

      res <- NA_character_

    } else {

      cn_df <- rgbif::name_usage(key, data = "vernacularNames")$data %>%
        dplyr::select(tidyselect::any_of(c("vernacularName"
                                           , "language"
                                           , "preferred"
                                           )
                                         )
                      )

      has_any <- ncol(cn_df) > 0

      has_preferred <- if("preferred" %in% names(cn_df)) sum(cn_df$preferred, na.rm = TRUE) > 0 else FALSE

      has_language <- if("language" %in% names(cn_df)) sum(cn_df$preferred, na.rm = TRUE) > 0 else FALSE

      has_preferred_eng <- if(has_preferred) cn_df %>%
        dplyr::filter(preferred) %>%
        dplyr::filter(language == "eng") %>%
        nrow() %>%
        `>` (0) else FALSE

      has_eng <- if(has_language) cn_df %>%
        dplyr::filter(language == "eng") %>%
        nrow() %>%
        `>` (0) else FALSE

      res <- if(has_preferred_eng) {

        cn_df %>%
          dplyr::filter(preferred
                        , language == "eng"
                        ) %>%
          dplyr::pull(vernacularName) %>%
          unique() %>%
          sort() %>%
          paste0(collapse = ", ")

      } else if(has_eng) {

        cn_df %>%
          dplyr::filter(language == "eng") %>%
          tidytext::unnest_tokens("common"
                                  , vernacularName
                                  , token = "regex"
                                  , pattern = ",|and"
                                  , collapse = NULL
                                  ) %>%
          dplyr::mutate(common = gsub("^\\s|\\s$|etc","",common)) %>%
          dplyr::distinct(common) %>%
          dplyr::pull(common) %>%
          unique() %>%
          sort() %>%
          paste0(collapse = ", ")

      } else if(has_any) {

        cn_df %>%
          dplyr::count(vernacularName) %>%
          dplyr::arrange(desc(n)) %>%
          dplyr::slice(1) %>%
          dplyr::pull(vernacularName) %>%
          `[` (1)

      } else NA_character_

    }

    cat(paste0(key
               , ": "
               , res
               , "\n"
               )
        )

    return(res)

  }
