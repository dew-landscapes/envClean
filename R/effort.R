
  filter_effort <- function(df) {

    datPCAEnv <- florAllAOIRelTaxOutAnn %>%
      dplyr::distinct(cell) %>%
      dplyr::inner_join(florCellEnvSelect) %>%
      janitor::remove_constant()


  pcaEnv <- prcomp(datPCAEnv[,-1]
                   , center = TRUE
                   , scale. = TRUE
  )


  pcaEnvDf <- tibble(cell = datPCAEnv %>%
                       dplyr::pull(cell)
  ) %>%
    dplyr::bind_cols(get_pca_ind(pcaEnv)$coord[,1:pcaAxes] %>%
                       as_tibble() %>%
                       setNames(paste0("PC",1:ncol(.)))
    )


  pcaVarDf <- get_pca_var(pcaEnv)$coord[,1:pcaAxes] %>%
    as_tibble(rownames = "name") %>%
    setNames(gsub("Dim.","PC",names(.)))


  pcaEnvDfLong <- pcaEnvDf %>%
    tidyr::pivot_longer(contains("PC"),names_to = "PC", values_to = "value")


  # breakpoints for classes in first x PCs
  brks <- pcaEnvDfLong %>%
    tidyr::nest(data = -PC) %>%
    dplyr::mutate(id = row_number()
                  , brks = purrr::map2(data
                                       ,id
                                       ,~unique(c(-Inf
                                                  ,classIntervals(.x$value, pcaCuts/.y, style = findIntervalStyle)$brks
                                                  ,Inf
                                       )
                                       )
                  )
                  , brks = purrr::map(brks,~tibble::enframe(.,name = NULL, value = "brks"))
                  , brks = purrr::map(brks,. %>% dplyr::distinct(brks))
                  , mids = purrr::map(brks,. %>% dplyr::mutate(mid = (brks+lead(brks))/2))
                  , mids = purrr::map(mids,. %>%
                                        dplyr::mutate(mid = if_else(mid == -Inf
                                                                    ,min(.$brks[is.finite(.$brks)])+min(.$mid[is.finite(.$mid)])
                                                                    ,mid
                                        )
                                        , mid = if_else(mid == Inf
                                                        ,max(.$brks[is.finite(.$brks)])+max(.$mid[is.finite(.$mid)])
                                                        , mid
                                        )
                                        ) %>%
                                        dplyr::filter(!is.na(mid) & mid != Inf & mid!= -Inf) %>%
                                        dplyr::pull(mid)
                  )
    )


  # Put breaks back into pcaEnvRes
  pcaEnvRes <- pcaEnvDfLong %>%
    dplyr::left_join(brks[,c("PC","brks")]) %>%
    dplyr::mutate(cutPC = purrr::map2(value,brks,~cut(.x,breaks=unique(unlist(.y))))) %>%
    dplyr::select(-brks) %>%
    tidyr::pivot_wider(names_from = "PC", values_from = c(value,"cutPC")) %>%
    setNames(gsub("value_|PC_","",names(.))) %>%
    tidyr::unnest(cols = contains("PC"))


  # Generate colours for PCAs
  cellPCADf <- pcaEnvDf %>%
    dplyr::left_join(pcaEnvRes) %>%
    dplyr::mutate(across(where(is.factor),factor)) %>%
    dplyr::mutate(across(where(is.factor)
                         , ~as.numeric(.)/length(levels(.))
                         , .names = "rgb{col}"
    )
    ) %>%
    setNames(gsub("rgbcutPC","",names(.))) %>%
    dplyr::mutate(colour = rgb(`1`,`2`,`3`)
                  , pcGroup = paste0(cutPC1,cutPC2,cutPC3)
    )

  pcaPalette <- cellPCADf %>%
    dplyr::distinct(pcGroup,colour) %>%
    dplyr::pull(colour,name = pcGroup)


  ## ----SRAnalysis---------------
  ##-----SRData-----
  Y <- "adjSR"

  datExp <- florAllAOIRelTaxOutAnn %>%
    dplyr::filter(!is.na(qsize)
                  , qsize >= 3*3
    ) %>%
    dplyr::distinct(Taxa,across(all_of(alwaysGroup))) %>%
    dplyr::count(across(all_of(alwaysGroup)),name = "sr") %>%
    dplyr::filter(sr > 3) %>% # Ensure tree health RRG, BB and Cooba sites not included
    dplyr::group_by(cell) %>%
    dplyr::filter(year == max(year)) %>%
    dplyr::filter(month == max(month)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(pcaEnvDf) %>%
    dplyr::mutate(adjSR = sr - 1
                  #, sinMonth = sin(2*pi*month/length(month.name))
                  #, cosMonth = cos(2*pi*month/length(month.name))
    ) %>%
    dplyr::select(!!ensym(Y),everything()) %>%
    tidyr::pivot_longer(contains("PC"),names_to = "PC") %>%
    dplyr::left_join(brks[,c("PC","brks")]) %>%
    dplyr::mutate(cutPC = purrr::map2(value,brks,~cut(.x,breaks=unique(unlist(.y))))) %>%
    dplyr::select(-brks) %>%
    tidyr::pivot_wider(names_from = "PC", values_from = c(value,"cutPC")) %>%
    setNames(gsub("value_|PC_","",names(.))) %>%
    tidyr::unnest(cols = 1:ncol(.))  %>%
    tidyr::unnest(cols = grep("cutPC",names(.),value = TRUE))


  ## ----SRModel---------------------------------------------------------------------------------------------------------------------------------------------------------------------

  # If the model has not already been run, run it and save the result to the R directory
  mod <- stan_glm(data = datExp

                  # This formula includes circular variable taking into account the month in which the data were collected
                  #, formula = as.formula(paste0(Y, " ~ PC1*sinMonth + PC1*cosMonth + PC2*sinMonth + PC2*cosMonth + PC3*sinMonth + PC3*cosMonth"))

                  # This formula does not include month
                  , formula = as.formula(paste0(Y, " ~ PC1 + PC2 + PC3"))

                  # Poisson
                  #, family = poisson()

                  # Negative binomial
                  , family = neg_binomial_2()

                  # Options
                  , iter = doIter
                  , chains = doChains
                  #, adapt_delta = 0.99
  )

  write_rds(mod, path(outDir,"mod.rds"))

  #------SRPrediction------

  preds <- brks %>%
    dplyr::pull(mids, name = PC) %>%
    purrr::cross_df() %>%
    tidyr::pivot_longer(1:ncol(.),names_to = "PC") %>%
    dplyr::left_join(brks[,c("PC","brks")]) %>%
    dplyr::mutate(cutPC = purrr::map2(value,brks,~cut(.x,breaks=unique(unlist(.y))))) %>%
    dplyr::select(-brks) %>%
    tidyr::pivot_wider(names_from = "PC", values_from = c(value,"cutPC")) %>%
    setNames(gsub("value_|PC_","",names(.))) %>%
    tidyr::unnest(cols = 1:ncol(.))  %>%
    tidyr::unnest(cols = grep("cutPC",names(.),value = TRUE)) %>%
    dplyr::inner_join(datExp %>%
                        dplyr::distinct(across(grep("cut|Month",names(.),value = TRUE)))
    )


  modPred <- preds %>%
    dplyr::mutate(col = row.names(.)) %>%
    dplyr::left_join(as_tibble(posterior_predict(mod
                                                 , newdata = .
                                                 , re.form = NA
    )
    ) %>%
      tibble::rownames_to_column(var = "row") %>%
      tidyr::gather(col,value,2:ncol(.))
    ) %>%
    (function(x) dplyr::bind_cols(x %>% dplyr::select(-value),adjSR = as.numeric(x$value))) %>%
    dplyr::mutate(sr = adjSR + 1) %>%
    dplyr::filter(sr < 10*max(datExp$sr))

  write_feather(modPred,path(outDir,"modPred.feather"))


  #------SRResiduals--------

  if(nrow(mod$data) == nrow(datExp)) {

    modResid <- tibble(fitted = fitted(mod)
                       , residual = residuals(mod)) %>%
      dplyr::mutate(standResid = residual/sd(.$residual)) %>%
      dplyr::bind_cols(datExp)

    ggplot(modResid,aes(fitted,standResid)) +
      geom_point() +
      geom_smooth()

    doCont <- sum(unlist(lapply(modResid,function(x) is.numeric(x)))) > 0
    doDisc <- sum(unlist(lapply(modResid,function(x) is.factor(x)|is.character(x)))) > 0

  }


  ##-----SRResults-------------------------------------------------------------------------------------------------------------------------------------------------------------------

  modRes <- modPred %>%
    #dplyr::filter(sr < 10*max(datExp$sr)) %>%
    dplyr::group_by_at(vars(grep("cutPC|Month|month",names(.),value = TRUE))) %>%
    dplyr::summarise(runs = n()
                     , nCheck = nrow(as_tibble(mod))
                     , modMed = quantile(sr,0.5,na.rm=TRUE)
                     , modMean = mean(sr,na.rm=TRUE)
                     , modci90lo = quantile(sr, 0.05,na.rm=TRUE)
                     , modci90up = quantile(sr, 0.95,na.rm=TRUE)
                     , extremeSRlo = quantile(sr, probs = 0+extremeSRThresh/2, na.rm=TRUE)
                     , extremeSRhi = quantile(sr, probs = 1-extremeSRThresh/2, na.rm=TRUE)
                     , text = paste0(round(modMed,2)," (",round(modci90lo,2)," to ",round(modci90up,2),")")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate_if(is.numeric,round,2)

  ggplot(modRes,aes(cutPC1,modMean,colour = cutPC3)) +
    geom_point() +
    facet_wrap(~cutPC2, scales = "free_y") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_colour_viridis_d()

  ggplot(modRes,aes(cutPC1,modMean,colour = cutPC2)) +

    geom_point() +
    #facet_wrap(~month) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    scale_colour_viridis_d() +
    coord_cartesian(y = c(0,quantile(datExp$sr, probs = 0.99)))


  cellqSize <- florAllAOI %>%
    dplyr::inner_join(datExp) %>%
    dplyr::distinct(cell,year,month,qsize)

  cellSRResult <- florAllAOIRelTaxOutAnn %>%
    dplyr::left_join(cellqSize) %>%
    dplyr::count(cell,qsize,name = "sr") %>%
    dplyr::inner_join(pcaEnvRes) %>%
    dplyr::inner_join(modRes) %>%
    dplyr::mutate(keepHi = sr < extremeSRhi
                  , keepLo = sr > extremeSRlo
                  , keepqSize = !(qsize == 0 | is.na(qsize))
                  , keep = as.logical(keepHi*keepLo)
                  , keep = if_else(!keep,keepqSize,keep)
    ) %>%
    dplyr::inner_join(cellPCADf) %>%
    dplyr::mutate(colour = if_else(keep,"black",colour))

  srCellPlot <- ggplot(cellSRResult,aes(cutPC1,sr,colour=colour
                                        #,shape=keep
  )
  ) +
    geom_jitter() +
    facet_grid(cutPC2~cutPC3) +
    coord_cartesian(y = c(0,max(cellSRResult$sr[cellSRResult$keepHi == TRUE]))) +
    scale_colour_identity() +
    theme_dark() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


  srCellTab <- cellSRResult %>%
    dplyr::count(keepHi,keepLo,keepqSize,keep)

  florAllAOIRelTaxOutAnnSR <- florAllAOIRelTaxOutAnn %>%
    dplyr::inner_join(cellSRResult %>%
                        dplyr::filter(keep)
    ) %>%
    dplyr::select(names(florAllAOIRelTaxOutAnn),grep("cutPC",names(.),value = TRUE)) %>%
    dplyr::distinct()





    }


