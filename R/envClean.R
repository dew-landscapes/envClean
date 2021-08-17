

#' Get taxonomy from [GBIF Backbone Taxonomy](https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c).
#'
#' Retrieve accepted name taxonomy and retrieve taxonomic hierarchy for a df
#' with a column of taxonomic names
#'
#' @param df Dataframe with column of species names to resolve
#' @param sppcol Character. Name of column with species names
#' @param outFile Character. Path to save results to. Saving is iterative as
#' retrieving names can take some time.
#' @param kingType Character. Kingdom type (i.e. Plantae, Animalia etc.)
#' @param getCommon Logical. If TRUE, also get common names. Takes much longer.
#' @param targetRank Character. Default is 'Species'. At what level of the
#' taxonomic hierarchy are results desired.
#'
#' @return Dataframe of unique species names, other columns defining
#' taxonomic hierarchy and, optionally, common names.
#' @export
#'
#' @examples
#' gbif_tax(data.frame(Species = "Eucalyptus viminalis"))
#'
#'
  gbif_tax <- function(df
                       , sppcol = 1
                       , outFile = tempfile()
                       , kingType = "Plantae"
                       , getCommon = FALSE
                       , targetRank = "Species"
                       ){

    outFile <- paste0(gsub("\\..*","",outFile),".feather")
    tmpFile <- paste0(gsub(".feather","",outFile),"_temp.feather")

    targetSort <- luRank %>%
      dplyr::filter(Rank == targetRank) %>%
      dplyr::pull(sort)

    alreadyDone01 <- if(file.exists(outFile)) rio::import(outFile) %>%
      dplyr::distinct(originalName) %>%
      dplyr::pull()

    alreadyDone02 <- if(file.exists(tmpFile)) rio::import(tmpFile) %>%
      dplyr::distinct(originalName) %>%
      dplyr::pull()

    alreadyDone <- c(get0("alreadyDone01"),get0("alreadyDone02"))

    toCheck <- df %>%
      dplyr::select(all_of(sppcol)) %>%
      dplyr::distinct() %>%
      dplyr::pull()

    taxa <- tibble::tibble(originalName = setdiff(toCheck,alreadyDone)) %>%
      dplyr::filter(!grepl("BOLD:.*\\d{4}",originalName)
                    , !is.na(originalName)
                    ) %>%
      dplyr::mutate(searchedName = gsub("\\s*\\(.*\\).*|\\'|\\?| spp\\.| sp\\.| ssp\\.| var\\.| ex| [A-Z].*|#|\\s^"
                                        ,""
                                        ,originalName
                                        )
      , searchedName = gsub(" x .*$| X .*$","",searchedName)
      , searchedName = gsub("\\s{2,}"," ",searchedName)
      , searchedName = stringr::str_squish(searchedName)
      )

    taxas <- taxa %>%
      dplyr::distinct(searchedName) %>%
      dplyr::arrange(searchedName)

    if(length(taxas$searchedName)>0){

      for (i in taxas$searchedName){

        print(i)

        taxGBIF <- rgbif::name_backbone(i, kingdom = kingType) %>%
          dplyr::mutate(searchedName = i)

        taxGBIF <- if(sum(grepl("acceptedUsageKey",names(taxGBIF)))>0) {

          rgbif::name_usage(taxGBIF$acceptedUsageKey,return="data")$data %>%
            dplyr::mutate(matchType = "Synonym") %>%
            dplyr::rename(usageKey = key
                          , status = taxonomicStatus
                          ) %>%
            dplyr::mutate(searchedName = i)

        } else {

          taxGBIF

        }

        if(getCommon) taxGBIF$Common <- get_gbif_common(taxGBIF$usageKey)

        taxGBIF$Taxa <- taxGBIF %>%
          tidyr::pivot_longer(where(is.numeric),names_to = "key") %>%
          dplyr::mutate(key = purrr::map_chr(key,~gsub("Key","",.))
                        , key = stringr::str_to_sentence(key)
                        ) %>%
          dplyr::filter(key %in% luRank$Rank) %>%
          dplyr::left_join(luRank, by = c("key" = "Rank")) %>%
          dplyr::filter(sort <= targetSort) %>%
          dplyr::filter(sort == max(sort)) %>%
          dplyr::select(tolower(luRank$Rank[luRank$sort == .$sort])) %>%
          dplyr::pull()

        taxGBIF$Stamp <- Sys.time()

        taxGBIF <- taxa %>%
          dplyr::inner_join(taxGBIF)

        if(file.exists(tmpFile)) {

          rio::export(taxGBIF %>%
                          dplyr::bind_rows(rio::import(tmpFile)) %>%
                          dplyr::select(1,2,Taxa,everything())
                      , tmpFile
                      )

        } else {

          rio::export(taxGBIF %>%
                          dplyr::select(1,2,Taxa,everything())
                      , tmpFile
                      )

        }

      }

      # Clean up results
      rio::import(tmpFile) %>%
        {if(!file.exists(outFile)) (.) else (.) %>% dplyr::bind_rows(rio::import(outFile))} %>%
        dplyr::group_by(originalName) %>%
        dplyr::filter(Stamp == max(Stamp)) %>%
        dplyr::ungroup() %>%
        rio::export(outFile)

      file.remove(tmpFile)

    } else {

      {warning( "No taxa supplied" )}

    }

    rio::import(outFile)

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

    commonNames <- rgbif::name_usage(key)$data %>%
      dplyr::select(contains("Key")) %>%
      dplyr::select(where(is.numeric)) %>%
      tidyr::pivot_longer(1:ncol(.),names_to = "key") %>%
      dplyr::mutate(key = purrr::map_chr(key,~gsub("Key","",.))
                    , key = stringr::str_to_sentence(key)
                    ) %>%
      dplyr::filter(key %in% luRank$Rank) %>%
      dplyr::left_join(luRank, by = c("key" = "Rank")) %>%
      dplyr::filter(sort == max(sort)) %>%
      dplyr::pull(value) %>%
      rgbif::name_usage(data="vernacularNames")

    df <- commonNames$data %>%
      dplyr::select(any_of(c("vernacularName","language","preferred")))

    hasAny <- nrow(df) > 0

    hasPreferred <- if("preferred" %in% names(df)) sum(df$preferred, na.rm = TRUE) > 0 else FALSE

    hasLanguage <- if("language" %in% names(df)) sum(df$preferred, na.rm = TRUE) > 0 else FALSE

    hasPreferredEng <- if(hasPreferred) df %>%
      dplyr::filter(preferred) %>%
      dplyr::filter(language == "eng") %>%
      nrow() %>%
      `>` (0) else FALSE

    hasEng <- if(hasLanguage) df %>%
      dplyr::filter(language == "eng") %>%
      nrow() %>%
      `>` (0) else FALSE

    if(hasPreferredEng) {

      df %>%
        dplyr::filter(preferred
                      , language == "eng"
                      ) %>%
        dplyr::pull(vernacularName) %>%
        unique() %>%
        sort() %>%
        paste0(collapse = ", ")

    } else if(hasEng) {

      df %>%
        dplyr::filter(language == "eng") %>%
        tidytext::unnest_tokens("common",vernacularName,token = "regex", pattern = ",|and",collapse = FALSE) %>%
        dplyr::mutate(common = gsub("^\\s|\\s$|etc","",common)) %>%
        dplyr::distinct(common) %>%
        dplyr::pull(common) %>%
        unique() %>%
        sort() %>%
        paste0(collapse = ", ")

    } else if(hasAny) {

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

    gbifTaxDf <- rio::import(path) %>%
      #(if(testing) {. %>% dplyr::sample_n(5)} else {.}) %>%
      dplyr::mutate(Common = purrr::map_chr(key,envClean::get_gbif_common))

    rio::export(gbifTaxDf,path)

  }


#' Clean/Tidy to one row per Taxa*Visit
#'
#' Includes running of taxa_taxonomy()
#'
#' @param df Dataframe to reduce.
#' @param sppcol Character. Name of column with species.
#' @param visit Character. Name of columns defining a visit.
#' @param extracols Character. Name of any extra columns to keep.
#'
#' @return Dataframe with columns Taxa, visit column(s) and, if used, extracols.
#' @export
#'
#' @examples
  filter_taxa <- function(df
                          , sppcol = "SPECIES"
                          , visit = visitcols
                          , extracols = NULL
                          , targetRank = "Species"
                          , doCover = FALSE
                          , doLifeform = FALSE
                          ) {

    # run taxa_taxonomy
    if(!exists("luTaxa")) taxa_taxonomy(df, lifespancol = "lifespan")

    # Use dfTaxa as base df from here
    florTaxa <- df %>%
      dplyr::distinct(!!ensym(sppcol)) %>%
      dplyr::left_join(luTaxa %>%
                          dplyr::rename(!!ensym(sppcol) := originalName)
                        ) %>%
      dplyr::filter(Rank >= "Species") %>%
      dplyr::left_join(df) %>%
      dplyr::select(all_of(visit),Taxa,all_of(extracols)) %>%
      dplyr::distinct()

    florTaxaCov <- if(doCover) {

      create_cover(florTaxa, visit = visitcols)

      } else florTaxa

    florTaxaLife <- if(doLifeform) {

      create_lifeform(florTaxa, context = visitcols)

    } else florTaxa

    florTaxa <- florTaxa %>%
      dplyr::distinct(across(all_of(visitcols)),Taxa) %>%
      {if(doCover) (.) %>% dplyr::left_join(florTaxaCov) else (.)} %>%
      {if(doLifeform) (.) %>% dplyr::left_join(florTaxaLife) else (.)}


    }



#' Create taxonomy lookups
#'
#' @param df Dataframe with species column.
#' @param sppcol Name of column with species.
#' @param poorTaxFilt Character. Any text to grep out of the species column.
#' (e.g. c("dead", "unknown")).
#' @param luTaxFile Character. Path to file containing desired taxonomy to use.
#' This is usually the output from gfbif_tax(). If this does not exist it will
#' be created by gbif_tax().
#' @param king Character. Kingdom to search preferentially in GBIF Taxonomy
#' Backbone
#'
#' @return Dataframe with applied taxonomy from GBIF Taxonony Backbone. Also,
#' two dataframes are returned to the global environment. One, named
#' taxaTaxonomy, with unique taxa and associated taxonomicinformation and two,
#' named luTaxa, a lookup from unique values in sppcol to matched taxonomy from
#' GBIF backbone.
#'
#' @export
#'
#' @examples
#'
  taxa_taxonomy <- function(df
                            , sppcol = "SPECIES"
                            , lifespancol = NULL
                            , indcol = NULL
                            , poorTaxFilt = speciesFilter
                            , luTaxFile = "out/luGBIF.feather"
                            , king = "Plantae"
                            ) {

    # Remove dodgy taxonomy
    taxas <- df %>%
      dplyr::distinct(dplyr::across(!!ensym(sppcol))) %>%
      dplyr::filter(!grepl(paste0(speciesFilter,collapse = "|"),!!ensym(sppcol),ignore.case = TRUE))

    # GBIF taxonomy
    zero <- taxas %>%
      gbif_tax(outFile = luTaxFile
              , kingType = king
               ) %>%
      dplyr::inner_join(taxas %>%
                          dplyr::rename(originalName = 1)
                        ) %>%
      dplyr::mutate(Rank = stringr::str_to_sentence(rank))

    returnTaxaTaxonomy <- c("Taxa",tolower(luRank$Rank))

    one <- zero %>%
      dplyr::distinct(dplyr::across(any_of(returnTaxaTaxonomy))) %>%
      stats::setNames(stringr::str_to_sentence(names(.))) %>%
      tibble::as_tibble()

    dups <- one %>%
      dplyr::add_count(Taxa) %>%
      dplyr::filter(n > 1) %>%
      dplyr::select(-n)

    keepDups <- dups %>%
      dplyr::filter(Kingdom == king) %>%
      dplyr::group_by(Taxa) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    one <- one %>%
      dplyr::anti_join(dups) %>%
      dplyr::bind_rows(keepDups)

    two <- zero %>%
      dplyr::distinct(originalName,Taxa,Rank) %>%
      tibble::as_tibble()

    # Add in lifespan
    if(isTRUE(!is.null(lifespancol))) {

      sppLS <- df %>%
        dplyr::filter(!is.na(lifespan)) %>%
        dplyr::rename(originalName = !!ensym(sppcol)) %>%
        dplyr::left_join(two) %>%
        dplyr::count(Taxa,lifespan) %>%
        dplyr::group_by(Taxa) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) %>%
        dplyr::rename(sppLS = lifespan) %>%
        dplyr::distinct()

      genLS <- df %>%
        dplyr::filter(!is.na(lifespan)) %>%
        dplyr::rename(originalName = !!ensym(sppcol)) %>%
        dplyr::left_join(two) %>%
        dplyr::left_join(one) %>%
        dplyr::count(Genus,lifespan) %>%
        dplyr::group_by(Genus) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) %>%
        dplyr::rename(genLS = lifespan) %>%
        dplyr::distinct()

      famLS <- df %>%
        dplyr::filter(!is.na(lifespan)) %>%
        dplyr::rename(originalName = !!ensym(sppcol)) %>%
        dplyr::left_join(two) %>%
        dplyr::left_join(one) %>%
        dplyr::count(Family,lifespan) %>%
        dplyr::group_by(Family) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) %>%
        dplyr::rename(famLS = lifespan) %>%
        dplyr::distinct()

      one <- one %>%
        dplyr::left_join(sppLS) %>%
        dplyr::left_join(genLS) %>%
        dplyr::left_join(famLS) %>%
        dplyr::distinct() %>%
        dplyr::mutate(blah = if_else(!is.na(sppLS)
                                     , sppLS
                                     , if_else(!is.na(genLS)
                                               , genLS
                                               , famLS
                                               )
                                     )
                      , blah = if_else(is.na(blah),"unknown",blah)
                      ) %>%
        dplyr::select(names(one),lifespan = blah)

    }

    if(isTRUE(!is.null(indcol))) {

      inddf <- df %>%
        dplyr::rename(originalName = SPECIES) %>%
        dplyr::left_join(two) %>%
        create_ind_status(sppcol = "Taxa") %>%
        dplyr::add_count(Taxa) %>%
        dplyr::mutate(ind = if_else(n > 1,"U",ind)) %>%
        dplyr::select(-n) %>%
        dplyr::distinct()

      one <- one %>%
        dplyr::inner_join(inddf)

    }

    assign("taxaTaxonomy",one,envir = globalenv())
    assign("luTaxa",two,envir = globalenv())

  }

#' Create indigenous status lookup
#'
#' @param df Dataframe with species column and column indicating indigenous
#' status.
#' @param sppcol Character. Name of column with species.
#' @param indcol Character. Name of column with indigenous status
#'
#' @return Dataframe with unique species and their indigenous status.
#' @export
#'
#' @examples
  create_ind_status <- function(df, sppcol = "Taxa", indcol = "ind") {

    df %>%
      dplyr::count(dplyr::across(!!ensym(sppcol)),dplyr::across(!!ensym(indcol))) %>%
      dplyr::filter(!is.na(!!ensym(indcol))) %>%
      dplyr::group_by(!!ensym(sppcol)) %>%
      dplyr::filter(n == max(n, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(!!ensym(sppcol),!!ensym(indcol)) %>%
      dplyr::right_join(df %>%
                          dplyr::distinct(!!ensym(sppcol))
                        ) %>%
      dplyr::mutate(!!ensym(indcol) := dplyr::if_else(grepl("\\?",!!ensym(indcol)),"U",!!ensym(indcol))
                    , !!ensym(indcol) := dplyr::if_else(is.na(!!ensym(indcol)),"U",!!ensym(indcol))
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
#' @param sppcol Character. Name of species column.
#' @param lfcol Character. Name of lifeform (id) column.
#' @param context Charcter or NULL. Set of columns that define a context within
#' which to generate lifeform.
#' @param luLifeformObj Dataframe. Name of dataframe object containing columns
#' used later to deal with duplicates. e.g. if a taxa (within a context) has
#' two lifeforms with the same number of instances, say, SA and SB, information
#' in luLifeformObj helps to choose the tallest option.
#'
#' @return Dataframe with columns sppcol, visit col(s), lifeform
#' @export
#'
#' @examples
  create_lifeform <- function(df, sppcol = "Taxa", lfcol = "lifeform", context = NULL, luLifeformObj = luLifeform) {

    df %>%
      dplyr::filter(!is.na(!!ensym(lfcol))) %>%
      dplyr::count(across(!!ensym(sppcol)),across(!!ensym(lfcol)),across(all_of(context)), name = "lifeformRecords") %>%
      dplyr::group_by(across(!!ensym(sppcol)),across(all_of(context))) %>%
      dplyr::mutate(taxaRecords = sum(lifeformRecords,na.rm = TRUE)
                    , per = 100*lifeformRecords/taxaRecords
                    ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(per > 5) %>%
      dplyr::left_join(luLifeformObj, by = stats::setNames("LifeForm_Code",lfcol)) %>%
      dplyr::mutate(HeightTest = dplyr::if_else(lifeform == "J",Height + 0.01, Height)) %>%
      dplyr::group_by(across(!!ensym(sppcol)),across(all_of(context))) %>%
      dplyr::slice(which(HeightTest == max(HeightTest, na.rm=TRUE))) %>%
      dplyr::slice(which(lifeformRecords == max(lifeformRecords, na.rm=TRUE))) %>%
      dplyr::slice(which(LF_Class_Num == max(LF_Class_Num, na.rm=TRUE))) %>%
      dplyr::ungroup() %>%
      dplyr::select(all_of(context),!!ensym(sppcol),lifeform) %>%
      dplyr::filter(!is.na(!!ensym(sppcol))) %>%
      dplyr::distinct()

  }


  create_cover <- function(df, sppcol = "Taxa", visit = NULL, luCoverObj = luCover) {

    # Assumes numeric (percentage) cover column called 'cover' and character
    # column called 'covCode' that is the modified Braun-Blanquet cover value
    # from BDBSA. luCoverObj is used to map from covCode to numeric.

    df %>%
      dplyr::filter(!is.na(cover) | !is.na(covCode)) %>%
      dplyr::mutate(cover = ifelse(cover == 0,NA,cover)
                    , cover = ifelse(cover > 100, NA, cover)
                    , cover = cover/100
                    , COVCODE = covCode
                    ) %>%
      dplyr::filter(!is.na(cover) | !is.na(covCode)) %>%
      dplyr::left_join(luCoverObj) %>%
      dplyr::mutate(useCover = if_else(!is.na(cover),cover,!!ensym(covType))) %>%
      dplyr::group_by(dplyr::across(all_of(visit))
                      , dplyr::across(!!ensym(sppcol))
                      ) %>%
      dplyr::summarise(useCover = max(useCover,na.rm = TRUE)) %>%
      dplyr::ungroup()

  }


#' Filter data frame to specified spatial reliability
#'
#' @param df Dataframe.
#' @param distcol Character. Name of the column containing the spatial reliability.
#' @param dist Numeric. In units of the maxDist column.
#' @param visit Character. column names defining a 'visit'.
#' @param dfRel Dataframe. Lookup from reliability id to reliability distance.
#'
#' @return Dataframe with records of greater than dist filtered. Filtering is done
#' at 'visit' level.
#' @export
#'
#' @examples
  filter_spat_rel <- function(df
                              , distcol = "maxDist"
                              , dist = relDist
                              , visit = visitcols
                              , dfRel = luRel
                              ){

    visRel <- df %>%
      dplyr::left_join(dfRel) %>%
      dplyr::mutate(distcol := dplyr::if_else(is.na(!!ensym(distcol)),relDist,!!ensym(distcol))) %>%
      dplyr::distinct(dplyr::across(any_of(visit)),!!ensym(distcol)) %>%
      dplyr::filter(!!ensym(distcol) <= dist) %>%
      dplyr::select(-!!ensym(distcol))

    df %>%
      dplyr::inner_join(visRel) %>%
      tibble::as_tibble()

  }


#' Filter a dataframe with e/n or lat/long to an area of interest polygon (sf)
#'
#' @param df Dataframe. Needs coordinate columns
#' @param AOI sf. Name of sf object defining the area of interest
#' @param x Character. Name of column with x coord
#' @param y Character. Name of column with y coord
#' @param crsDf Anything that will return a legitimate crs when passed to the
#' crs attribute of st_transform or st_as_sf
#' @param crsAOI as for crsDf
#'
#' @return Dataframe filtered to area of interest
#' @export
#'
#' @examples
  filter_aoi <- function(df
                         , AOI = aoi
                         , x = "long"
                         , y = "lat"
                         , crsDf = 4326
                         , crsAOI = useEPSG
                         ) {

    df %>%
      dplyr::distinct(!!ensym(x),!!ensym(y)) %>%
      sf::st_as_sf(coords = c(x,y)
                   , crs = crsDf
                   , remove = FALSE
                   ) %>%
      sf::st_transform(crs = crsAOI) %>%
      sf::st_filter(AOI) %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::inner_join(df) %>%
      tibble::as_tibble()

  }


#' Filter a column annual taxa
#'
#' @param df Dataframe with column to filter.
#' @param filtcol Character. Name of column to filter.
#' @param filtText Character. Text(s) to filter from df.
#' @param dfJoin Optional dataframe. Joined to df before filter. No names from
#' dfJoin are returned.
#'
#' @return Filtered dataframe with same names as df
#' @export
#'
#' @examples
  filter_text_col <- function(df
                         , filtcol = "lifespan"
                         , filtText = "A"
                         , dfJoin = NULL
                         ) {

    joined <- if(isTRUE(!is.null(dfJoin))) {

      df %>%
        dplyr::left_join(dfJoin)

    } else df

    keepLevels <- joined %>%
      dplyr::distinct(!!ensym(filtcol)) %>%
      dplyr::filter(!grepl(paste0(filtText,collapse = "|"),!!ensym(filtcol)))

    joined %>%
      dplyr::inner_join(keepLevels) %>%
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
#' @param df Dataframe with Taxa and context
#' @param context Character. Column names that define context, usually a 'visit'
#' to a 'cell'.
#' @param minSites Absolute minimum sites at which a taxa should be recorded.
#' @param keepTaxa Character. Taxa that should not be dropped. Used to set x
#' percent of sites.
#' @param defaultPer If keepTaxa is NULL, what is the minimum percent of sites
#' at which a taxa should be recorded.
#'
#' @return df filtered to exclude taxa recorded at less than x percent of
#' visits.
#' @export
#'
#' @examples
  filter_prop <- function(df
                          , context = "cell"
                          , minSites = 15
                          , keepTaxa = NULL
                          , defaultPer = 1
                          ) {

    visitsPerFilter <- if(isTRUE(!is.null(keepTaxa))) {

      dontDropDf <- df %>%
        dplyr::mutate(visits = n_distinct(across(all_of(context)))) %>%
        dplyr::filter(Taxa %in% keepTaxa) %>%
        dplyr::count(Taxa,visits,name = "records") %>%
        dplyr::filter(records > 5) %>%
        dplyr::mutate(per = round(100*records/visits,2))

      dontDropDf %>%
        dplyr::filter(visits > minAbsSites/2) %>%
        dplyr::pull(per) %>%
        min()

    } else defaultPer

    dropTaxa <- df %>%
      dplyr::mutate(nVisits = dplyr::n_distinct(across(all_of(context)))) %>%
      dplyr::group_by(Taxa,nVisits) %>%
      dplyr::summarise(nRecords = n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(per = 100*nRecords/nVisits) %>%
      dplyr::filter(per < visitsPerFilter) %>%
      dplyr::distinct(Taxa)

    df %>%
      dplyr::anti_join(dropTaxa)

  }

