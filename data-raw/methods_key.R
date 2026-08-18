methods_key <-
  tibble::tribble(~ method_gp, ~ terms, ~rank
                  , "call playback", c("call playback", "^playback$"), 0
                  , "nocturnal playback", c("nocturnal playback"), 0
                  , "camera trap", c("camera trap", "camera - surveillance", "remote camera", "^camera$"), 0
                  , "structured survey", c("transect", "area search", "fixed.*(search|survey)", "point.*(count|search)", "(search|survey).*(min|ha|hectare|km|kilometre)", "(min|ha|hectare|km|kilometre).*(search|survey)"), 0
                  , "2 ha, 20 min", c("(2 ha|2ha|2 hectare).*\\s20.*min"), 1 # ranked over general structured survey
                  , "2 ha, non-20 min", c("(2 ha|2ha|2 hectare).*non\\-20.*min"), 1 # ranked over general structured survey
                  , "2 ha, unknown time", c("Birds Australia 2ha search", "bird survey - 2ha$"), 1 # ranked over general structured survey
                  , "5 ha, 30 min", c("(5 ha|5ha|5 hectare).*30 min"), 1 # ranked over general structured survey
                  , "5 km area search", c("(5 km|5km) area search"), 1 # ranked over general structured survey
                  , "500 m transect", c("(500 m|500m).*transect"), 1 # ranked over general structured survey
                  , "200 m transect", c("(200 m|200m).*transect"), 1 # ranked over general structured survey
                  , "5 minute point search", c("5 min.*point search"), 1 # ranked over general structured survey
                  , "quadrat", c("quadrat"), 0
                  , "percent cover", c("percent cover"), 0
                  , "braun-blanquet", c("braun-blanquet"), 1 # ranked over general percent cover
                  , "remote sensing", c("remote sensing"), 0
                  , "thermal imaging", c("thermal imag"), 0
                  , "audio recorder", c("audio", "call record", "acoustic"), 0
                  , "mist net", c("mist"), 0
                  , "harp trap", c("harp", "bat trap", "bat tripline"), 0
                  , "bat detector", c("bat detector", "bat ultrasound", "anabat"), 1 # ranked over general audio recorder
                  , "pitfall", c("pit"), 0
                  , "elliott", c("elliot"), 0
                  , "cage", c("cage trap"), 0
                  , "thomas trap", c("thomas trap"), 0
                  , "hair tube", c("hair tube"), 0
                  , "spotlighting", c("spotlight"), 0
                  , "aerial survey", c("aerial.*(survey|obs|transect)", "-air:", "helicopter"), 1 # ranked over general structured survey
                  , "noose", c("noose"), 1 # ranked over hook & line (as noose often with fishing line)
                  , "tile", c("tile"), 0
                  , "fyke net", c("fyke"), 0
                  , "seine net", c("seine"), 0
                  , "opera net", c("opera net"), 0
                  , "electrofishing" , c("electro", "e fishing"), 0
                  , "dip net", c("dip"), 0
                  , "funnel trap", c("funnel", "berlese"), 1 # ranked over more general litter sample
                  , "litter sample", c("litter"), 0
                  , "BRUV", c("bruv"), 0
                  , "trawl", c("trawl"), 0
                  , "dredge", c("dredge"), 0
                  , "gill net", c("gill net|gillnet"), 0
                  , "drum net", c("drum net"), 0
                  , "bait trap", c("bait trap"), 0
                  , "plankton net", c("bongo net", "plankton net", "channel net", "//stow", "tow$", "^tow"), 0
                  , "benthic sled", c("sled"), 0
                  , "sediment grab", c("Smith.*McIntyre", "Van Veen", "Shipek", "Eckman", "(sediment|benthic) grab", "eck grab", "O'Gower"), 0
                  , "diving", c("scuba", "diving", "dive"), 0
                  , "snorkel", c("snorkel"), 0
                  , "spear", c("spear"), 0
                  , "remote operated vehicle", c("remote operated vehicle", "ROV"), 0
                  , "washing/seiving", c("washing", "seiv"), 0
                  , "hook & line", c("(hook|hand|set|long|fishing).*line", "line.*hook", "angling"), 0
                  , "light trap", c("light.*trap", "at light", "night.*light"), 0
                  , "UV light", c("uv light"), 1 # ranked over general light trap
                  , "MV lamp", c("(mv|mercury).*(lamp|light)"), 1 # mercury vapour light trap for inverts; ranked over general light trap
                  , "Bucket light trap", c("bucket.*light.*trap"), 1 # ranked over general light trap
                  , "sweep net", c("sweep"), 0
                  , "beating", c("beating"), 0
                  , "panel trap", c("panel trap"), 0
                  , "vane trap", c("vane trap"), 0
                  , "rotenone", c("rotenone"), 0 # insecticide/piscicide used for sampling insects and fish
                  , "hand net", c("hand net", "handnet", "hand-net"), 0
                  , "hand", c("by hand", "hand collect", "^hand$"), 0
                  , "sticky trap", c("sticky trap"), 0
                  , "kick net", c("kick"), 0
                  , "emergence trap", c("emergence"), 0
                  , "scraping", c("scrap"), 0
                  , "core", c("core"), 1 # ranked over hand
                  , "cathedral net", c("cathedral net"), 0
                  , "pan trap", c("pan trap"), 0
                  , "malaise trap", c("malaise"), 2 # ranked over general flight intercept trap
                  , "flight intercept trap", c("flight intercept"), 1 # ranked the same as micro pitfall to capture when both are used simultaneously
                  , "micro pitfall", c("flight intercept.*pitfall", "pitfall.*flight intercept", "(invert|micro|ant|glycol|ethylene).*pit", "pit.*(ml|1L|dung)"), 1 # ranked over general pitfall
                  , "roadkill", c("roadkill", "road kill"), 0
                  , "skeleton/feathers", c("skelet", "feather"), 0
                  , "scat", c("scat"), 0
                  , "tracks", c("tracks"), 0
                  , "diggings", c("diggings"), 0
  )
