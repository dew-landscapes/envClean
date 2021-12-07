
  library(magrittr)
  library(envClean)

  path_to_save <- "data-raw/flor_all.feather"

  path_to_flor <- "../../projects/envEco/out/MDD02_50_current/runs/2021-10-28-1024/flor_all.feather"

  path_to_area_of_interest <- "data-raw/shp/Bakara_smaller.shp"

  if(file.exists(path_to_flor) & file.exists(path_to_area_of_interest) & make_new_flor) {

    sens_spp <- rio::import("https://data.environment.sa.gov.au/Content/Publications/DEW_SAEnvironmentallySensitiveDataREGISTER.xls")

    sens_spp_taxa <- sens_spp %>%
      dplyr::filter(SPECIESTYPE == "Plants") %>%
      envClean::get_gbif_tax("SPECIES"
                             , out_file = "data-raw/luGBIF.feather"
                             )

    keep_cols <- c("lat", "long", "data_name", "site", "date", "original_name"
                   , "cover", "cover_code", "quad_x", "quad_y", "rel_dist", "month", "year"
                   )

    flor_all <- rio::import(path_to_flor) %>%
      tibble::as_tibble()

    aoi <- sf::st_read(path_to_area_of_interest) %>%
      sf::st_buffer(10000)

    flor_aoi <- flor_all %>%
      envClean::filter_aoi(aoi
                           , crs_aoi = sf::st_crs(aoi)
                           ) %>%
      dplyr::select(any_of(keep_cols))

    flor_all <- flor_aoi %>%
      dplyr::anti_join(sens_spp_taxa)

    rio::export(flor_all
                , path_to_save
                )

  } else flor_all <- rio::import(path_to_save)


