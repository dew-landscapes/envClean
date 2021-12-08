
  library(magrittr)
  library(envClean)

  path_to_save <- "data-raw/flor_all.csv"

  path_to_flor <- "../../projects/envEco/out/MDD02_50_current/runs/2021-10-28-1024/flor_all.csv"

  path_to_area_of_interest <- "data-raw/shp/Bakara_smaller.shp"

  if(file.exists(path_to_flor) & file.exists(path_to_area_of_interest) & make_new_flor) {

    keep_cols <- c("lat", "long", "data_name", "site", "date", "original_name"
                   , "cover", "cover_code", "quad_x", "quad_y", "rel_dist", "month", "year"
                   )

    flor_all <- data.table::fread(path_to_flor) %>%
      tibble::as_tibble()

    aoi <- sf::st_read(path_to_area_of_interest)

    aoi_crs <- sf::st_crs(aoi)

    aoi_buf <- aoi %>%
      sf::st_transform(crs = 3577) %>%
      sf::st_buffer(10000) %>%
      sf::st_transform(crs = aoi_crs)

    flor_aoi <- flor_all %>%
      envClean::filter_aoi(aoi_buf
                           , crs_aoi = sf::st_crs(aoi_buf)
                           ) %>%
      dplyr::select(any_of(c("nsx", "survey_nr", keep_cols)))

    flor_aoi_sens <- envImport::flag_sens_records(flor_aoi, nsx_col = "nsx", surv_col = "survey_nr")

    flor_all <- flor_aoi_sens %>%
      dplyr::filter(data_name != "BCM") %>%
      dplyr::filter(!grepl(TRUE, sens_surv)) %>%
      dplyr::filter(!grepl(TRUE, sens_taxa)) %>%
      dplyr::select(any_of(keep_cols))

    data.table::fwrite(flor_all
                       , path_to_save
                       )

  } else flor_all <- data.table::fread(path_to_save) %>% tibble::as.tibble()


