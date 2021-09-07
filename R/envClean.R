

#' Get taxonomy from [GBIF Backbone Taxonomy](https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c).
#'
#' Retrieve accepted name taxonomy and retrieve taxonomic hierarchy for a df
#' with a column of taxonomic names
#'
#' @param df Dataframe with column of species names to resolve
#' @param taxa_col Character. Name of column with species names
#' @param out_file Character. Path to save results to. Saving is iterative as
#' retrieving names can take some time.
#' @param king_type Character. Kingdom type (i.e. Plantae, Animalia etc.)
#' @param do_common Logical. If TRUE, also get common names. Takes much longer.
#' @param target_rank Character. Default is 'Species'. At what level of the
#' taxonomic hierarchy are results desired.
#'
#' @return Dataframe of unique species names, other columns defining
#' taxonomic hierarchy and, optionally, common names.
#' @export
#'
#' @examples
#' get_gbif_tax(data.frame(Species = "Eucalyptus viminalis"))
#'
#'
  get_gbif_tax <- function(df
                       , taxa_col = 1
                       , out_file = tempfile()
                       , king_type = "Plantae"
                       , do_common = FALSE
                       , target_rank = "Species"
                       ){

    out_file <- paste0(gsub("\\..*","",out_file),".feather")
    tmp_file <- paste0(gsub(".feather","",out_file),"_temp.feather")

    target_sort <- lurank %>%
      dplyr::filter(rank == target_rank) %>%
      dplyr::pull(sort)

    already_done_01 <- if(file.exists(out_file)) rio::import(out_file) %>%
      dplyr::distinct(original_name) %>%
      dplyr::pull()

    already_done_02 <- if(file.exists(tmp_file)) rio::import(tmp_file) %>%
      dplyr::distinct(original_name) %>%
      dplyr::pull()

    already_done <- c(get0("already_done_01"),get0("already_done_02"))

    to_check <- df %>%
      dplyr::select(all_of(taxa_col)) %>%
      dplyr::distinct() %>%
      dplyr::pull()

    taxa <- tibble::tibble(original_name = setdiff(to_check,already_done)) %>%
      dplyr::filter(!grepl("BOLD:.*\\d{4}",original_name)
                    , !is.na(original_name)
                    ) %>%
      dplyr::mutate(searched_name = gsub("\\s*\\(.*\\).*|\\'|\\?| spp\\.| sp\\.| ssp\\.| var\\.| ex| [A-Z].*|#|\\s^"
                                        ,""
                                        ,original_name
                                        )
      , searched_name = gsub(" x .*$| X .*$","",searched_name)
      , searched_name = gsub("\\s{2,}"," ",searched_name)
      , searched_name = stringr::str_squish(searched_name)
      )

    taxas <- taxa %>%
      dplyr::distinct(searched_name) %>%
      dplyr::arrange(searched_name)

    if(length(taxas$searched_name)>0){

      for (i in taxas$searched_name){

        print(i)

        tax_gbif <- rgbif::name_backbone(i, kingdom = king_type) %>%
          dplyr::mutate(searched_name = i)

        tax_gbif <- if(sum(grepl("acceptedUsageKey",names(tax_gbif)))>0) {

          rgbif::name_usage(tax_gbif$acceptedUsageKey,return="data")$data %>%
            dplyr::mutate(matchType = "Synonym") %>%
            dplyr::rename(usage_key = key
                          , status = taxonomicStatus
                          ) %>%
            dplyr::mutate(searched_name = i)

        } else {

          tax_gbif

        }

        if(do_common) tax_gbif$common <- get_gbif_common(tax_gbif$usage_key)

        tax_gbif$taxa <- tax_gbif %>%
          tidyr::pivot_longer(where(is.numeric),names_to = "key") %>%
          dplyr::mutate(key = purrr::map_chr(key,~gsub("Key","",.))
                        , key = stringr::str_to_sentence(key)
                        ) %>%
          dplyr::filter(key %in% lurank$rank) %>%
          dplyr::left_join(lurank, by = c("key" = "rank")) %>%
          dplyr::filter(sort <= target_sort) %>%
          dplyr::filter(sort == max(sort)) %>%
          dplyr::select(tolower(lurank$rank[lurank$sort == .$sort])) %>%
          dplyr::pull()

        tax_gbif$stamp <- Sys.time()

        tax_gbif <- taxa %>%
          dplyr::inner_join(tax_gbif)

        if(file.exists(tmp_file)) {

          rio::export(tax_gbif %>%
                          dplyr::bind_rows(rio::import(tmp_file)) %>%
                          dplyr::select(1,2,taxa,everything())
                      , tmp_file
                      )

        } else {

          rio::export(tax_gbif %>%
                          dplyr::select(1,2,taxa,everything())
                      , tmp_file
                      )

        }

      }

      # Clean up results
      rio::import(tmp_file) %>%
        {if(!file.exists(out_file)) (.) else (.) %>% dplyr::bind_rows(rio::import(out_file))} %>%
        dplyr::group_by(original_name) %>%
        dplyr::filter(stamp == max(stamp)) %>%
        dplyr::ungroup() %>%
        rio::export(out_file)

      file.remove(tmp_file)

    } else {

      {warning( "No taxa supplied" )}

    }

    rio::import(out_file) %>%
      tibble::as_tibble()

  }


#' Find common name from GBIF
#'
#' @param key Numeric. gbif 'useagekey'
#'
#' @return Character. Best attempt at finding a common name for the supplied
#' useagekey
#' @export
#'
#' @examples
#' get_gbif_common(1) # 1 is "Plantae"
  get_gbif_common <- function(key) {

    print(key)

    common_names <- rgbif::name_usage(key)$data %>%
      dplyr::select(contains("Key")) %>%
      dplyr::select(where(is.numeric)) %>%
      tidyr::pivot_longer(1:ncol(.),names_to = "key") %>%
      dplyr::mutate(key = purrr::map_chr(key,~gsub("Key","",.))
                    , key = stringr::str_to_sentence(key)
                    ) %>%
      dplyr::filter(key %in% lurank$rank) %>%
      dplyr::left_join(lurank, by = c("key" = "rank")) %>%
      dplyr::filter(sort == max(sort)) %>%
      dplyr::pull(value) %>%
      rgbif::name_usage(data="vernacularNames")

    df <- common_names$data %>%
      dplyr::select(any_of(c("vernacularName","language","preferred")))

    has_any <- nrow(df) > 0

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

    if(has_preferred_eng) {

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
        tidytext::unnest_tokens("common",vernacularName,token = "regex", pattern = ",|and",collapse = FALSE) %>%
        dplyr::mutate(common = gsub("^\\s|\\s$|etc","",common)) %>%
        dplyr::distinct(common) %>%
        dplyr::pull(common) %>%
        unique() %>%
        sort() %>%
        paste0(collapse = ", ")

    } else if(has_any) {

      df %>%
        dplyr::count(language,vernacularName) %>%
        dplyr::arrange(desc(n)
                       , language
                       ) %>%
        dplyr::slice(1) %>%
        dplyr::pull(vernacularName) %>%
        `[` (1)

    } else ""

  }

  # Add common name to existing taxonomic data frame
  add_gbif_common <- function(path = "out/luGBIF.feather") {

    gbif_tax_df <- rio::import(path) %>%
      #(if(testing) {. %>% dplyr::sample_n(5)} else {.}) %>%
      dplyr::mutate(common = purrr::map_chr(key,envClean::get_gbif_common))

    rio::export(gbif_tax_df,path)

  }


#' Clean/Tidy to one row per taxa*Visit
#'
#' Includes running of taxa_taxonomy(), if lutaxa does not already exist.
#'
#' @param df Dataframe to reduce.
#' @param taxa_col Character. Name of column with species.
#' @param context Character. Name of columns defining context.
#' @param extra_cols Character. Name of any extra columns to keep.
#' @param target_rank Character. Name of level in taxonomic hierarchy that names
#' should be retrieved from, if possible.
#' @param do_cov Logical. Should cover (needs to be supplied in df) be appended
#' to output.
#' @param do_life Logical. Should lifeform (needs to be supplied in df) be
#' appended to output.
#' @param lucov Dataframe lookup for cover.
#' @param lulife Dataframe lookup for lifeform.
#' @param poor Character. Vector of words to exclude from species list.
#' @param save_gbif_file Character. Path to `luGBIF.feather`. Passed to
#' `get_gbif_taxa()` (via `make_taxa_taxonomy()`) if lutaxa does not exist.
#' @param king_for_taxa Character. Passed to `get_gbif_taxa()`
#'
#' @return Dataframe with columns taxa, visit column(s) and, if used, extracols.
#' @export
#'
#' @examples
  filter_taxa <- function(df
                          , taxa_col = "original_name"
                          , context
                          , extra_cols = NULL
                          , target_rank = "species"
                          , do_cov = FALSE
                          , do_life = FALSE
                          , lucov = NULL
                          , lulife = NULL
                          , poor = NULL
                          , save_gbif_file = tempfile()
                          , king_for_taxa = "Plantae"
                          ) {

    df <- df %>%
      dplyr::rename(original_name = !!ensym(taxa_col))

    # run taxa_taxonomy
    if(!exists("lutaxa")) {

      .taxa_col <- taxa_col

      make_taxa_taxonomy(df
                         , taxa_col = .taxa_col
                         , lifespan_col = if(do_life) lifespan_col <- "lifespan" else NULL
                         , ind_col <- "ind"
                         , poor_filt = poor
                         , save_luGBIF = save_gbif_file
                         , king = king_for_taxa
                         )

      }

    # Use dftaxa as base df from here
    flor_taxa <- df %>%
      dplyr::distinct(!!ensym(taxa_col)) %>%
      dplyr::rename(original_name = !!ensym(taxa_col)) %>%
      dplyr::left_join(lutaxa) %>%
      dplyr::filter(rank >= target_rank) %>%
      dplyr::left_join(df) %>%
      dplyr::select(all_of(context),taxa,all_of(extra_cols)) %>%
      dplyr::distinct()

    flor_taxa_cov <- if(do_cov) {

      .context = context
      .lucov = lucov

      make_cover(flor_taxa
                   , context = .context
                   , lucov = .lucov
                   )

      } else flortaxa

    flor_taxa_life <- if(do_life) {

      .context = context
      .lulife = lulife

      make_lifeform(flor_taxa
                      , context = .context
                      , lulife = .lulife
                      )

    } else flor_taxa

    flor_taxa <- flor_taxa %>%
      dplyr::distinct(across(all_of(context)),taxa) %>%
      {if(do_cov) (.) %>% dplyr::left_join(flor_taxa_cov) else (.)} %>%
      {if(do_life) (.) %>% dplyr::left_join(flor_taxa_life) else (.)}

  }



#' Make taxonomy lookups
#'
#' @param df Dataframe with species column.
#' @param taxa_col Name of column with species.
#' @param lifespan_col Character. Optional name of column containing lifespan
#' information.
#' @param ind_col Character. Optional name of columns containing indigenous
#' status of taxa in taxa_col.
#' @param poor_filt Character. Any taxa names to grep out of the species column.
#' (e.g. c("annual form", "unverified")).
#' @param save_luGBIF Character. Path to file containing desired taxonomy to use.
#' This is usually the output from gfbif_tax(). If this does not exist it will
#' be created (as tempfile) by get_gbif_tax().
#' @param king Character. Kingdom to search preferentially in GBIF Taxonomy
#' Backbone
#'
#' @return Dataframe with applied taxonomy from GBIF Taxonony Backbone. Also,
#' two dataframes are returned to the global environment. One, named
#' taxaTaxonomy, with unique taxa and associated taxonomicinformation and two,
#' named lutaxa, a lookup from unique values in taxa_col to matched taxonomy from
#' GBIF backbone.
#'
#' @export
#'
#' @examples
#'
  make_taxa_taxonomy <- function(df
                            , taxa_col = "original_name"
                            , lifespan_col = NULL
                            , ind_col = NULL
                            , poor_filt = c("dead","unverified")
                            , save_luGBIF = tempfile()
                            , king = "Plantae"
                            ) {

    # Remove dodgy taxonomy
    taxas <- df %>%
      dplyr::distinct(dplyr::across(all_of(taxa_col))) %>%
      dplyr::filter(!grepl(paste0(poor_filt,collapse = "|")
                           ,!!ensym(taxa_col)
                           ,ignore.case = TRUE
                           )
                    )

    # GBIF taxonomy
    zero <- taxas %>%
      get_gbif_tax(out_file = save_luGBIF
               , king_type = king
               ) %>%
      dplyr::inner_join(taxas %>%
                          dplyr::rename(original_name = 1)
                        ) %>%
      dplyr::mutate(rank = stringr::str_to_sentence(rank))

    return_taxa_taxonomy <- c("taxa",tolower(lurank$rank))

    one <- zero %>%
      dplyr::distinct(dplyr::across(any_of(return_taxa_taxonomy))) %>%
      tibble::as_tibble()

    dups <- one %>%
      dplyr::add_count(taxa) %>%
      dplyr::filter(n > 1) %>%
      dplyr::select(-n)

    keep_dups <- dups %>%
      dplyr::filter(kingdom == king) %>%
      dplyr::group_by(taxa) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    one <- one %>%
      dplyr::anti_join(dups) %>%
      dplyr::bind_rows(keep_dups)

    two <- zero %>%
      dplyr::distinct(original_name,taxa,rank) %>%
      tibble::as_tibble()

    # Add in lifespan
    if(isTRUE(!is.null(lifespan_col))) {

      spp_lifespan <- df %>%
        dplyr::filter(!is.na(lifespan)) %>%
        dplyr::rename(original_name = !!ensym(taxa_col)) %>%
        dplyr::left_join(two) %>%
        dplyr::count(taxa,lifespan) %>%
        dplyr::group_by(taxa) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) %>%
        dplyr::rename(spp_lifespan = lifespan) %>%
        dplyr::distinct()

      gen_lifespan <- df %>%
        dplyr::filter(!is.na(lifespan)) %>%
        dplyr::rename(original_name = !!ensym(taxa_col)) %>%
        dplyr::left_join(two) %>%
        dplyr::left_join(one) %>%
        dplyr::count(genus,lifespan) %>%
        dplyr::group_by(genus) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) %>%
        dplyr::rename(gen_lifespan = lifespan) %>%
        dplyr::distinct()

      fam_lifespan <- df %>%
        dplyr::filter(!is.na(lifespan)) %>%
        dplyr::rename(original_name = !!ensym(taxa_col)) %>%
        dplyr::left_join(two) %>%
        dplyr::left_join(one) %>%
        dplyr::count(family,lifespan) %>%
        dplyr::group_by(family) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) %>%
        dplyr::rename(fam_lifespan = lifespan) %>%
        dplyr::distinct()

      one <- one %>%
        dplyr::left_join(spp_lifespan) %>%
        dplyr::left_join(gen_lifespan) %>%
        dplyr::left_join(fam_lifespan) %>%
        dplyr::distinct() %>%
        dplyr::mutate(lifespan = if_else(!is.na(spp_lifespan)
                                     , spp_lifespan
                                     , if_else(!is.na(gen_lifespan)
                                               , gen_lifespan
                                               , fam_lifespan
                                               )
                                     )
                      ) %>%
        dplyr::select(names(one),lifespan)

    }

    if(isTRUE(!is.null(ind_col))) {

      ind_df <- df %>%
        dplyr::rename(original_name = !!ensym(taxa_col)) %>%
        dplyr::left_join(two) %>%
        make_ind_status(taxa_col = "taxa") %>%
        dplyr::add_count(taxa) %>%
        dplyr::mutate(ind = if_else(n > 1,"U",ind)) %>%
        dplyr::select(-n) %>%
        dplyr::distinct()

      one <- one %>%
        dplyr::inner_join(ind_df)

    }

    assign("taxa_taxonomy",one,envir = globalenv())
    assign("lutaxa",two,envir = globalenv())

  }

#' Make indigenous status lookup
#'
#' @param df Dataframe with species column and column indicating indigenous
#' status.
#' @param taxa_col Character. Name of column with species.
#' @param ind_col Character. Name of column with indigenous status
#'
#' @return Dataframe with unique species and their indigenous status.
#' @export
#'
#' @examples
  make_ind_status <- function(df, taxa_col = "taxa", ind_col = "ind") {

    df %>%
      dplyr::count(dplyr::across(!!ensym(taxa_col)),dplyr::across(!!ensym(ind_col))) %>%
      dplyr::filter(!is.na(!!ensym(ind_col))) %>%
      dplyr::group_by(!!ensym(taxa_col)) %>%
      dplyr::filter(n == max(n, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(!!ensym(taxa_col),!!ensym(ind_col)) %>%
      dplyr::right_join(df %>%
                          dplyr::distinct(!!ensym(taxa_col))
                        ) %>%
      dplyr::mutate(!!ensym(ind_col) := dplyr::if_else(grepl("\\?",!!ensym(ind_col)),"U",!!ensym(ind_col))
                    , !!ensym(ind_col) := dplyr::if_else(is.na(!!ensym(ind_col)),"U",!!ensym(ind_col))
                    )

  }



#' Get unique lifeform across species, perhaps including further context
#'
#' There are two tasks here:
#'
#' * find the most frequent lifeform assigned to a taxa (perhaps including
#' other context)
#' * ensure there are no duplicates across the relevant context
#'
#' @param df Dataframe with species column.
#' @param taxa_col Character. Name of species column.
#' @param lf_col Character. Name of lifeform (id) column.
#' @param context Charcter or NULL. Set of columns that define a context within
#' which to generate lifeform.
#' @param lulife Dataframe lookup for lifeform.
#'
#' @return Dataframe with columns taxa_col, visit col(s), lifeform
#' @export
#'
#' @examples
  make_lifeform <- function(df
                              , taxa_col = "taxa"
                              , lf_col = "lifeform"
                              , context = NULL
                              , lulife
                              ) {

    df %>%
      dplyr::filter(!is.na(!!ensym(lf_col))) %>%
      dplyr::count(across(!!ensym(taxa_col)),across(!!ensym(lf_col)),across(all_of(context)), name = "lifeform_records") %>%
      dplyr::group_by(across(!!ensym(taxa_col)),across(all_of(context))) %>%
      dplyr::mutate(taxa_records = sum(lifeform_records,na.rm = TRUE)
                    , per = 100*lifeform_records/taxa_records
                    ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(per > 5) %>%
      dplyr::left_join(lulife) %>%
      dplyr::mutate(ht_test = dplyr::if_else(lifeform == "J",ht + 0.01, ht)) %>%
      dplyr::group_by(across(!!ensym(taxa_col)),across(all_of(context))) %>%
      dplyr::slice(which(ht_test == max(ht_test, na.rm=TRUE))) %>%
      dplyr::slice(which(lifeform_records == max(lifeform_records, na.rm=TRUE))) %>%
      dplyr::slice(which(sort == max(sort, na.rm=TRUE))) %>%
      dplyr::ungroup() %>%
      dplyr::select(all_of(context),!!ensym(taxa_col),lifeform) %>%
      dplyr::filter(!is.na(!!ensym(taxa_col))) %>%
      dplyr::distinct()

  }


#' Make a single (numeric, proportion) cover column from different sorts of
#' input cover
#'
#' Assumes a numeric (percentage) cover column called 'cover' and character
#' column called 'cover_code' that is the modified Braun-Blanquet
#' \insertRef{RN4265}{envEcosystems} cover value using
#' [Biological Databases of South Australia](https://www.environment.sa.gov.au/topics/Science/Information_data/Biological_databases_of_South_Australia)
#' codes (`COVCODE` field in BDBSA). Example of `lucov` at
#' `envEcosystems::lucover`.
#'
#' @param df Dataframe containing columns to consolidate.
#' @param taxa_col Character. Name of column containing taxa.
#' @param context Character. Name of columns defining the context.
#' @param lucov Dataframe. Lookup from `cover_code` to numeric cover values.
#'
#' @return Dataframe with consolidated `use_cover` column.
#' @export
#'
#' @examples
  make_cover <- function(df, taxa_col = "taxa", context = NULL, lucov) {

    df %>%
      dplyr::filter(!is.na(cover) | !is.na(cover_code)) %>%
      dplyr::mutate(cover = ifelse(cover == 0,NA,cover)
                    , cover = ifelse(cover > 100, NA, cover)
                    , cover = cover/100
                    ) %>%
      dplyr::filter(!is.na(cover) | !is.na(cover_code)) %>%
      dplyr::left_join(lucov) %>%
      dplyr::mutate(use_cover = if_else(!is.na(cover),cover,!!ensym(cov_type))) %>%
      dplyr::group_by(dplyr::across(all_of(context))
                      , dplyr::across(!!ensym(taxa_col))
                      ) %>%
      dplyr::summarise(use_cover = max(use_cover,na.rm = TRUE)) %>%
      dplyr::ungroup()

  }


#' Filter data frame to specified spatial reliability
#'
#' @param df Dataframe.
#' @param dist_col Character. Name of the column containing the spatial
#' reliability.
#' @param dist Numeric. Filter spatial reliability greater than this value. In
#' the same units as `dist_col`.
#' @param context Character. column names defining the context.
#' @param over_ride Named list. List names must be the same as column names. Any
#' names in `over_ride` will be matched to column names in `df` and any values
#' in that list element will not be filtered (on spatial reliability).
#'
#' @return Dataframe with records of greater than `dist` filtered. Filtering is
#' done at `context` level.
#' @export
#'
#' @examples
  filter_spat_rel <- function(df
                              , dist_col = "rel_dist"
                              , dist = 50
                              , context
                              , over_ride = NULL
                              ){

    .df <- df

    if(isTRUE(!is.null(over_ride))) {

      for(i in seq_along(over_ride)) {

        col <- names(over_ride)[[i]]

        .df <- .df %>%
          dplyr::mutate(!!ensym(dist_col) := if_else(!!ensym(col) %in% over_ride[[i]]
                                                , dist
                                                , !!ensym(dist_col)
                                                )
                        )

      }

    }

    vis_rel <- .df %>%
      dplyr::distinct(dplyr::across(any_of(context)),!!ensym(dist_col)) %>%
      dplyr::filter(!is.na(!!ensym(dist_col))) %>%
      dplyr::filter(!!ensym(dist_col) <= dist) %>%
      dplyr::select(-!!ensym(dist_col))

    df %>%
      dplyr::inner_join(vis_rel)

  }


#' Filter a dataframe with e/n or lat/long to an area of interest polygon (sf)
#'
#' @param df Dataframe. Needs coordinate columns
#' @param use_aoi sf. Name of sf object defining the area of interest
#' @param x Character. Name of column with x coord
#' @param y Character. Name of column with y coord
#' @param crs_df Anything that will return a legitimate crs when passed to the
#' crs attribute of st_transform or st_as_sf
#' @param crs_aoi as for crsdf
#'
#' @return Dataframe filtered to area of interest
#' @export
#'
#' @examples
  filter_aoi <- function(df
                         , use_aoi
                         , x = "long"
                         , y = "lat"
                         , crs_df = 4326
                         , crs_aoi
                         ) {

    df %>%
      dplyr::distinct(!!ensym(x),!!ensym(y)) %>%
      sf::st_as_sf(coords = c(x,y)
                   , crs = crs_df
                   , remove = FALSE
                   ) %>%
      sf::st_transform(crs = crs_aoi) %>%
      sf::st_filter(use_aoi) %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::inner_join(df) %>%
      tibble::as_tibble()

  }


#' Filter a column
#'
#' @param df Dataframe with column to filter.
#' @param filt_col Character. Name of column to filter.
#' @param filt_text Character. Text(s) to filter from df.
#' @param df_join Optional dataframe. Joined to df before filter. No names from
#' dfJoin are returned.
#'
#' @return Filtered dataframe with same names as df
#' @export
#'
#' @examples
  filter_text_col <- function(df
                         , filt_col
                         , filt_text
                         , df_join = NULL
                         ) {

    joined <- if(isTRUE(!is.null(df_join))) {

      df %>%
        dplyr::left_join(df_join)

    } else df

    keep_levels <- joined %>%
      dplyr::distinct(!!ensym(filt_col)) %>%
      dplyr::filter(!grepl(paste0(filt_text,collapse = "|"),!!ensym(filt_col)))

    joined %>%
      dplyr::inner_join(keep_levels) %>%
      dplyr::select(names(df))

  }



#' Filter any context with less instances than a threshold value
#'
#' @param df Dataframe with column names defining context.
#' @param context Character. columns defining context within which to count
#' instances.
#' @param thresh Numeric. Threshold (inclusive of thresh) below which to filter.
#'
#' @return Filtered dataframe with same names as df
#' @export
#'
#' @examples
  filter_counts <- function(df, context, thresh = 1) {

    df %>%
      dplyr::add_count(across(all_of(context))) %>%
      dplyr::filter(n > thresh) %>%
      dplyr::select(names(df))

  }




#' Filter taxa recorded at less than x percent of visits
#'
#' @param df Dataframe with taxa and context
#' @param context Character. Column names that define context, usually a 'visit'
#' to a 'cell'.
#' @param min_sites Absolute minimum sites at which a taxa should be recorded.
#' @param keep Character. taxa that should not be dropped. Used to set x
#' percent of sites.
#' @param default_per If keeptaxa is NULL, what is the minimum percent of sites
#' at which a taxa should be recorded.
#'
#' @return df filtered to exclude taxa recorded at less than x percent of
#' visits.
#' @export
#'
#' @examples
  filter_prop <- function(df
                          , context = "cell"
                          , min_sites = 15
                          , keep = NULL
                          , default_per = 1
                          ) {

    thresh <- if(isTRUE(!is.null(keep))) {

      dont_drop_df <- df %>%
        dplyr::mutate(visits = n_distinct(across(all_of(context)))) %>%
        dplyr::filter(taxa %in% keep) %>%
        dplyr::count(taxa,visits,name = "records") %>%
        dplyr::filter(records > 5) %>%
        dplyr::mutate(per = round(100*records/visits,2)) %>%
        dplyr::filter(visits > min_sites/2) %>%
        dplyr::pull(per) %>%
        min()

    } else default_per

    drop_taxa <- df %>%
      dplyr::mutate(n_visits = dplyr::n_distinct(across(all_of(context)))) %>%
      dplyr::group_by(taxa,n_visits) %>%
      dplyr::summarise(n_records = n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(per = 100*n_records/n_visits) %>%
      dplyr::filter(per < thresh) %>%
      dplyr::distinct(taxa)

    df %>%
      dplyr::anti_join(drop_taxa)

  }

