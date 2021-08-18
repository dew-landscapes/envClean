

#' Get taxonomy from [GBIF Backbone Taxonomy](https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c).
#'
#' Retrieve accepted name taxonomy and retrieve taxonomic hierarchy for a df
#' with a column of taxonomic names
#'
#' @param df Dataframe with column of species names to resolve
#' @param sppcol Character. Name of column with species names
#' @param outfile Character. Path to save results to. Saving is iterative as
#' retrieving names can take some time.
#' @param kingtype Character. Kingdom type (i.e. Plantae, Animalia etc.)
#' @param docommon Logical. If TRUE, also get common names. Takes much longer.
#' @param targetrank Character. Default is 'Species'. At what level of the
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
                       , outfile = tempfile()
                       , kingtype = "Plantae"
                       , getcommon = FALSE
                       , targetrank = "Species"
                       ){

    outfile <- paste0(gsub("\\..*","",outfile),".feather")
    tmpfile <- paste0(gsub(".feather","",outfile),"_temp.feather")

    targetSort <- lurank %>%
      dplyr::filter(rank == targetrank) %>%
      dplyr::pull(sort)

    alreadydone01 <- if(file.exists(outfile)) rio::import(outfile) %>%
      dplyr::distinct(originalname) %>%
      dplyr::pull()

    alreadydone02 <- if(file.exists(tmpfile)) rio::import(tmpfile) %>%
      dplyr::distinct(originalname) %>%
      dplyr::pull()

    alreadydone <- c(get0("alreadydone01"),get0("alreadydone02"))

    tocheck <- df %>%
      dplyr::select(all_of(sppcol)) %>%
      dplyr::distinct() %>%
      dplyr::pull()

    taxa <- tibble::tibble(originalname = setdiff(tocheck,alreadydone)) %>%
      dplyr::filter(!grepl("BOLD:.*\\d{4}",originalname)
                    , !is.na(originalname)
                    ) %>%
      dplyr::mutate(searchedname = gsub("\\s*\\(.*\\).*|\\'|\\?| spp\\.| sp\\.| ssp\\.| var\\.| ex| [A-Z].*|#|\\s^"
                                        ,""
                                        ,originalname
                                        )
      , searchedname = gsub(" x .*$| X .*$","",searchedname)
      , searchedname = gsub("\\s{2,}"," ",searchedname)
      , searchedname = stringr::str_squish(searchedname)
      )

    taxas <- taxa %>%
      dplyr::distinct(searchedname) %>%
      dplyr::arrange(searchedname)

    if(length(taxas$searchedname)>0){

      for (i in taxas$searchedname){

        print(i)

        taxgbif <- rgbif::name_backbone(i, kingdom = kingtype) %>%
          dplyr::mutate(searchedname = i)

        taxgbif <- if(sum(grepl("acceptedUsageKey",names(taxgbif)))>0) {

          rgbif::name_usage(taxgbif$acceptedUsageKey,return="data")$data %>%
            dplyr::mutate(matchType = "Synonym") %>%
            dplyr::rename(usageKey = key
                          , status = taxonomicStatus
                          ) %>%
            dplyr::mutate(searchedname = i)

        } else {

          taxgbif

        }

        if(getcommon) taxgbif$common <- get_gbif_common(taxgbif$usageKey)

        taxgbif$taxa <- taxgbif %>%
          tidyr::pivot_longer(where(is.numeric),names_to = "key") %>%
          dplyr::mutate(key = purrr::map_chr(key,~gsub("Key","",.))
                        , key = stringr::str_to_sentence(key)
                        ) %>%
          dplyr::filter(key %in% lurank$rank) %>%
          dplyr::left_join(lurank, by = c("key" = "rank")) %>%
          dplyr::filter(sort <= targetSort) %>%
          dplyr::filter(sort == max(sort)) %>%
          dplyr::select(tolower(lurank$rank[lurank$sort == .$sort])) %>%
          dplyr::pull()

        taxgbif$stamp <- Sys.time()

        taxgbif <- taxa %>%
          dplyr::inner_join(taxgbif)

        if(file.exists(tmpfile)) {

          rio::export(taxgbif %>%
                          dplyr::bind_rows(rio::import(tmpfile)) %>%
                          dplyr::select(1,2,taxa,everything())
                      , tmpfile
                      )

        } else {

          rio::export(taxgbif %>%
                          dplyr::select(1,2,taxa,everything())
                      , tmpfile
                      )

        }

      }

      # Clean up results
      rio::import(tmpfile) %>%
        {if(!file.exists(outfile)) (.) else (.) %>% dplyr::bind_rows(rio::import(outfile))} %>%
        dplyr::group_by(originalname) %>%
        dplyr::filter(stamp == max(stamp)) %>%
        dplyr::ungroup() %>%
        rio::export(outfile)

      file.remove(tmpfile)

    } else {

      {warning( "No taxa supplied" )}

    }

    rio::import(outfile)

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

    commonnames <- rgbif::name_usage(key)$data %>%
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

    df <- commonnames$data %>%
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

    gbiftaxdf <- rio::import(path) %>%
      #(if(testing) {. %>% dplyr::sample_n(5)} else {.}) %>%
      dplyr::mutate(common = purrr::map_chr(key,envClean::get_gbif_common))

    rio::export(gbifTaxdf,path)

  }


#' Clean/Tidy to one row per taxa*Visit
#'
#' Includes running of taxa_taxonomy(), if lutaxa does not already exist.
#'
#' @param df Dataframe to reduce.
#' @param sppcol Character. Name of column with species.
#' @param context Character. Name of columns defining context.
#' @param extracols Character. Name of any extra columns to keep.
#' @param targetrank Character. Name of level in taxonomic hierarchy that names
#' should be retrieved from, if possible.
#' @param docov Logical. Should cover (needs to be supplied in df) be appended
#' to output.
#' @param dolife Logical. Should lifeform (needs to be supplied in df) be
#' appended to output.
#'
#' @return Dataframe with columns taxa, visit column(s) and, if used, extracols.
#' @export
#'
#' @examples
  filter_taxa <- function(df
                          , sppcol = "SPECIES"
                          , context
                          , extracols = NULL
                          , targetrank = "Species"
                          , docov = FALSE
                          , dolife = FALSE
                          ) {

    # run taxa_taxonomy
    if(!exists("lutaxa")) taxa_taxonomy(df, lifespancol = "lifespan")

    # Use dftaxa as base df from here
    flortaxa <- df %>%
      dplyr::distinct(!!ensym(sppcol)) %>%
      dplyr::left_join(lutaxa %>%
                          dplyr::rename(!!ensym(sppcol) := originalname)
                        ) %>%
      dplyr::filter(rank >= targetrank) %>%
      dplyr::left_join(df) %>%
      dplyr::select(all_of(context),taxa,all_of(extracols)) %>%
      dplyr::distinct()

    flortaxaCov <- if(docov) {

      .context = context

      create_cover(flortaxa, context = .context)

      } else flortaxa

    flortaxaLife <- if(dolife) {

      .context = context

      create_lifeform(flortaxa, context = .context)

    } else flortaxa

    flortaxa <- flortaxa %>%
      dplyr::distinct(across(all_of(context)),taxa) %>%
      {if(doCover) (.) %>% dplyr::left_join(flortaxaCov) else (.)} %>%
      {if(doLifeform) (.) %>% dplyr::left_join(flortaxaLife) else (.)}

  }



#' Create taxonomy lookups
#'
#' @param df Dataframe with species column.
#' @param sppcol Name of column with species.
#' @param lifespancol Character. Optional name of column containing lifespan
#' information.
#' @param indcol Character. Optional name of columns containing indigenous
#' status of taxa in sppcol.
#' @param poorfilt Character. Any taxa names to grep out of the species column.
#' (e.g. c("annual form", "unverified")).
#' @param savefile Character. Path to file containing desired taxonomy to use.
#' This is usually the output from gfbif_tax(). If this does not exist it will
#' be created (as tempfile) by gbif_tax().
#' @param king Character. Kingdom to search preferentially in GBIF Taxonomy
#' Backbone
#'
#' @return Dataframe with applied taxonomy from GBIF Taxonony Backbone. Also,
#' two dataframes are returned to the global environment. One, named
#' taxaTaxonomy, with unique taxa and associated taxonomicinformation and two,
#' named lutaxa, a lookup from unique values in sppcol to matched taxonomy from
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
                            , poorfilt = c("dead","unverified")
                            , savefile = tempfile()
                            , king = "Plantae"
                            ) {

    # Remove dodgy taxonomy
    taxas <- df %>%
      dplyr::distinct(dplyr::across(all_of(sppcol))) %>%
      dplyr::filter(!grepl(paste0(poorfilt,collapse = "|")
                           ,!!ensym(sppcol)
                           ,ignore.case = TRUE
                           )
                    )

    # GBIF taxonomy
    zero <- taxas %>%
      gbif_tax(outfile = savefile
               , kingtype = king
               ) %>%
      dplyr::inner_join(taxas %>%
                          dplyr::rename(originalname = 1)
                        ) %>%
      dplyr::mutate(rank = stringr::str_to_sentence(rank))

    returntaxataxonomy <- c("taxa",tolower(lurank$rank))

    one <- zero %>%
      dplyr::distinct(dplyr::across(any_of(returntaxataxonomy))) %>%
      tibble::as_tibble()

    dups <- one %>%
      dplyr::add_count(taxa) %>%
      dplyr::filter(n > 1) %>%
      dplyr::select(-n)

    keepDups <- dups %>%
      dplyr::filter(kingdom == king) %>%
      dplyr::group_by(taxa) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()

    one <- one %>%
      dplyr::anti_join(dups) %>%
      dplyr::bind_rows(keepDups)

    two <- zero %>%
      dplyr::distinct(originalname,taxa,rank) %>%
      tibble::as_tibble()

    # Add in lifespan
    if(isTRUE(!is.null(lifespancol))) {

      sppLS <- df %>%
        dplyr::filter(!is.na(lifespan)) %>%
        dplyr::rename(originalname = !!ensym(sppcol)) %>%
        dplyr::left_join(two) %>%
        dplyr::count(taxa,lifespan) %>%
        dplyr::group_by(taxa) %>%
        dplyr::filter(n == max(n)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(-n) %>%
        dplyr::rename(sppLS = lifespan) %>%
        dplyr::distinct()

      genLS <- df %>%
        dplyr::filter(!is.na(lifespan)) %>%
        dplyr::rename(originalname = !!ensym(sppcol)) %>%
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
        dplyr::rename(originalname = !!ensym(sppcol)) %>%
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
        dplyr::rename(originalname = SPECIES) %>%
        dplyr::left_join(two) %>%
        create_ind_status(sppcol = "taxa") %>%
        dplyr::add_count(taxa) %>%
        dplyr::mutate(ind = if_else(n > 1,"U",ind)) %>%
        dplyr::select(-n) %>%
        dplyr::distinct()

      one <- one %>%
        dplyr::inner_join(inddf)

    }

    assign("taxaTaxonomy",one,envir = globalenv())
    assign("lutaxa",two,envir = globalenv())

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
  create_ind_status <- function(df, sppcol = "taxa", indcol = "ind") {

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
#'
#' @return Dataframe with columns sppcol, visit col(s), lifeform
#' @export
#'
#' @examples
  create_lifeform <- function(df
                              , sppcol = "taxa"
                              , lfcol = "lifeform"
                              , context = NULL
                              ) {

    df %>%
      dplyr::filter(!is.na(!!ensym(lfcol))) %>%
      dplyr::count(across(!!ensym(sppcol)),across(!!ensym(lfcol)),across(all_of(context)), name = "lifeformrecords") %>%
      dplyr::group_by(across(!!ensym(sppcol)),across(all_of(context))) %>%
      dplyr::mutate(taxaRecords = sum(lifeformrecords,na.rm = TRUE)
                    , per = 100*lifeformrecords/taxaRecords
                    ) %>%
      dplyr::ungroup() %>%
      dplyr::filter(per > 5) %>%
      dplyr::left_join(lulifeform) %>%
      dplyr::mutate(httest = dplyr::if_else(lifeform == "J",ht + 0.01, ht)) %>%
      dplyr::group_by(across(!!ensym(sppcol)),across(all_of(context))) %>%
      dplyr::slice(which(httest == max(httest, na.rm=TRUE))) %>%
      dplyr::slice(which(lifeformrecords == max(lifeformrecords, na.rm=TRUE))) %>%
      dplyr::slice(which(sort == max(sort, na.rm=TRUE))) %>%
      dplyr::ungroup() %>%
      dplyr::select(all_of(context),!!ensym(sppcol),lifeform) %>%
      dplyr::filter(!is.na(!!ensym(sppcol))) %>%
      dplyr::distinct()

  }


  create_cover <- function(df, sppcol = "taxa", context = NULL, lucov = luCover) {

    # Assumes numeric (percentage) cover column called 'cover' and character
    # column called 'covCode' that is the modified Braun-Blanquet cover value
    # from BDBSA. lucov is used to map from covCode to numeric.

    df %>%
      dplyr::filter(!is.na(cover) | !is.na(covCode)) %>%
      dplyr::mutate(cover = ifelse(cover == 0,NA,cover)
                    , cover = ifelse(cover > 100, NA, cover)
                    , cover = cover/100
                    , COVCODE = covCode
                    ) %>%
      dplyr::filter(!is.na(cover) | !is.na(covCode)) %>%
      dplyr::left_join(lucov) %>%
      dplyr::mutate(useCover = if_else(!is.na(cover),cover,!!ensym(covType))) %>%
      dplyr::group_by(dplyr::across(all_of(context))
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
#' @param context Character. column names defining the context.
#' @param dfrel Dataframe. Lookup from reliability id to reliability distance.
#'
#' @return Dataframe with records of greater than dist filtered. Filtering is done
#' at 'visit' level.
#' @export
#'
#' @examples
  filter_spat_rel <- function(df
                              , distcol = "maxDist"
                              , dist
                              , context
                              , dfrel
                              ){

    visrel <- df %>%
      dplyr::left_join(dfrel) %>%
      dplyr::mutate(distcol := dplyr::if_else(is.na(!!ensym(distcol)),relDist,!!ensym(distcol))) %>%
      dplyr::distinct(dplyr::across(any_of(context)),!!ensym(distcol)) %>%
      dplyr::filter(!!ensym(distcol) <= dist) %>%
      dplyr::select(-!!ensym(distcol))

    df %>%
      dplyr::inner_join(visrel) %>%
      tibble::as_tibble()

  }


#' Filter a dataframe with e/n or lat/long to an area of interest polygon (sf)
#'
#' @param df Dataframe. Needs coordinate columns
#' @param aoi sf. Name of sf object defining the area of interest
#' @param x Character. Name of column with x coord
#' @param y Character. Name of column with y coord
#' @param crsdf Anything that will return a legitimate crs when passed to the
#' crs attribute of st_transform or st_as_sf
#' @param crsaoi as for crsdf
#'
#' @return Dataframe filtered to area of interest
#' @export
#'
#' @examples
  filter_aoi <- function(df
                         , aoi
                         , x = "long"
                         , y = "lat"
                         , crsdf = 4326
                         , crsaoi = useEPSG
                         ) {

    df %>%
      dplyr::distinct(!!ensym(x),!!ensym(y)) %>%
      sf::st_as_sf(coords = c(x,y)
                   , crs = crsdf
                   , remove = FALSE
                   ) %>%
      sf::st_transform(crs = crsaoi) %>%
      sf::st_filter(aoi) %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::inner_join(df) %>%
      tibble::as_tibble()

  }


#' Filter a column
#'
#' @param df Dataframe with column to filter.
#' @param filtcol Character. Name of column to filter.
#' @param filttext Character. Text(s) to filter from df.
#' @param dfjoin Optional dataframe. Joined to df before filter. No names from
#' dfJoin are returned.
#'
#' @return Filtered dataframe with same names as df
#' @export
#'
#' @examples
  filter_text_col <- function(df
                         , filtcol
                         , filttext
                         , dfjoin = NULL
                         ) {

    joined <- if(isTRUE(!is.null(dfjoin))) {

      df %>%
        dplyr::left_join(dfjoin)

    } else df

    keeplevels <- joined %>%
      dplyr::distinct(!!ensym(filtcol)) %>%
      dplyr::filter(!grepl(paste0(filttext,collapse = "|"),!!ensym(filtcol)))

    joined %>%
      dplyr::inner_join(keeplevels) %>%
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
#' @param minsites Absolute minimum sites at which a taxa should be recorded.
#' @param keeptaxa Character. taxa that should not be dropped. Used to set x
#' percent of sites.
#' @param defaultper If keeptaxa is NULL, what is the minimum percent of sites
#' at which a taxa should be recorded.
#'
#' @return df filtered to exclude taxa recorded at less than x percent of
#' visits.
#' @export
#'
#' @examples
  filter_prop <- function(df
                          , context = "cell"
                          , minsites = 15
                          , keeptaxa = NULL
                          , defaultper = 1
                          ) {

    visitsPerFilter <- if(isTRUE(!is.null(keeptaxa))) {

      dontdropdf <- df %>%
        dplyr::mutate(visits = n_distinct(across(all_of(context)))) %>%
        dplyr::filter(taxa %in% keeptaxa) %>%
        dplyr::count(taxa,visits,name = "records") %>%
        dplyr::filter(records > 5) %>%
        dplyr::mutate(per = round(100*records/visits,2))

      dontdropdf %>%
        dplyr::filter(visits > minsites/2) %>%
        dplyr::pull(per) %>%
        min()

    } else defaultper

    droptaxa <- df %>%
      dplyr::mutate(nVisits = dplyr::n_distinct(across(all_of(context)))) %>%
      dplyr::group_by(taxa,nvisits) %>%
      dplyr::summarise(nrecords = n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(per = 100*nrecords/nvisits) %>%
      dplyr::filter(per < defaultper) %>%
      dplyr::distinct(taxa)

    df %>%
      dplyr::anti_join(droptaxa)

  }

