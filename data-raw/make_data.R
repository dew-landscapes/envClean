
  rm(list = ls())

  make_new_flor <- FALSE

  codes <- grep("make_data"
                , list.files(path = "data-raw",pattern = "\\.R$", full.names = TRUE)
                , value = TRUE
                , invert = TRUE
                )

  codes <- codes[-grep("_gbif",codes)]

  lapply(codes,source)

  datas <- ls(pattern = "lu|flor_all|aoi|overrides$")

  do.call(save, c(lapply(datas,as.name), file = "data/data.rda"))


