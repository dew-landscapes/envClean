
  library(magrittr)
  library(envClean)

  path_to_save <- "data-raw/flor_all.rds"

  path_to_flor <- "../../data/point/bio_all.rds"

  path_to_area_of_interest <- "data-raw/shp/Bakara_smaller.shp"

  if(file.exists(path_to_flor) & file.exists(path_to_area_of_interest) & make_new_flor) {

    keep_cols <- c("lat", "long", "data_name", "site", "date", "original_name"
                   , "rel_metres", "month", "year"
                   )

    flor_all <- rio::import(path_to_flor
                            , setclass = "tibble"
                            ) %>%
      dplyr::filter(kingdom == "Plantae") %>%
      dplyr::filter(data_name %in% c("TERN", "GBIF"))

    aoi <- sf::st_read(path_to_area_of_interest)

    aoi_crs <- sf::st_crs(aoi)

    aoi_buf <- aoi %>%
      sf::st_transform(crs = 3577) %>%
      sf::st_buffer(10000) %>%
      sf::st_transform(crs = aoi_crs)

    flor_aoi <- flor_all %>%
      dplyr::filter(kingdom == "Plantae") %>%
      envClean::filter_aoi(aoi_buf
                           , crs_aoi = sf::st_crs(aoi_buf)
                           ) %>%
      dplyr::select(any_of(c("nsx", "survey_nr", keep_cols)))

    flor_aoi_sens <- envImport::flag_sens_records(flor_aoi, nsx_col = "nsx", surv_col = "survey_nr")

    flor_all <- flor_aoi_sens %>%
      dplyr::filter(!grepl(TRUE, sens_surv)) %>%
      dplyr::filter(!grepl(TRUE, sens_taxa)) %>%
      dplyr::select(any_of(keep_cols))

    rio::export(flor_all
                , path_to_save
                )

  } else flor_all <- rio::import(path_to_save)


