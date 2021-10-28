
  library(magrittr)
  library(envClean)

  path_to_save <- "data-raw/flor_all.feather"

  path_to_flor <- "../../projects/envEco/out/MDD02_50_current/runs/2021-10-28-1024/flor_all.feather"

  path_to_area_of_interest <- "data-raw/shp/Bakara_smaller.shp"

  flor_all <- if(file.exists(path_to_flor) & file.exists(path_to_area_of_interest) & make_new_flor) {

    keep_cols <- c("lat", "long", "data_name", "site", "date", "original_name"
                   , "cover", "cover_code", "quad_x", "quad_y", "rel_dist", "month", "year"
                   )

    flor_all <- rio::import(path_to_flor)

    aoi <- sf::st_read(path_to_area_of_interest) %>%
      sf::st_buffer(10000)

    flor_keep <- filter_aoi(flor_all
                            , aoi
                            , crs_aoi = sf::st_crs(aoi)
                            ) %>%
      dplyr::select(any_of(keep_cols))

    rio::export(flor_keep
                , path_to_save
                )

    return(flor_keep)

  } else rio::import(path_to_save)


