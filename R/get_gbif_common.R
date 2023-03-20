
  get_gbif_common <- function(key) {

    common_names <- rgbif::name_usage(key)$data %>%
      dplyr::select(contains("Key")) %>%
      dplyr::select(where(is.numeric)) %>%
      tidyr::pivot_longer(1:ncol(.),names_to = "key") %>%
      dplyr::mutate(key = purrr::map_chr(key,~gsub("Key","",.))) %>%
      dplyr::filter(key %in% lurank$rank) %>%
      dplyr::left_join(lurank, by = c("key" = "rank")) %>%
      dplyr::filter(sort == max(sort)) %>%
      dplyr::pull(value) %>%
      rgbif::name_usage() #data="vernacularNames")

    df <- common_names$data %>%
      dplyr::select(tidyselect::any_of(c("vernacularName","language","preferred")))

    has_any <- ncol(df) > 0

    has_preferred <- if("preferred" %in% names(df)) sum(df$preferred, na.rm = TRUE) > 0 else FALSE

    has_language <- if("language" %in% names(df)) sum(df$preferred, na.rm = TRUE) > 0 else FALSE

    has_preferred_eng <- if(has_preferred) df %>%
      dplyr::filter(preferred) %>%
      dplyr::filter(language == "eng") %>%
      nrow() %>%
      `>` (0) else FALSE

    has_eng <- if(has_language) df %>%
      dplyr::filter(language == "eng") %>%
      nrow() %>%
      `>` (0) else FALSE

    res <- if(has_preferred_eng) {

      df %>%
        dplyr::filter(preferred
                      , language == "eng"
                      ) %>%
        dplyr::pull(vernacularName) %>%
        unique() %>%
        sort() %>%
        paste0(collapse = ", ")

    } else if(has_eng) {

      df %>%
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

      df %>%
        dplyr::count(vernacularName) %>%
        dplyr::arrange(desc(n)) %>%
        dplyr::slice(1) %>%
        dplyr::pull(vernacularName) %>%
        `[` (1)

    } else NA

    cat(paste0(key
               , ": "
               , res
               , "\n"
               )
        )

    return(res)

  }
