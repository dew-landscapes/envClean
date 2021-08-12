


#' Principal components analysis and various outputs from environmental data
#'
#' @param envdf Dataframe containing 'cell' and environmental data
#' @param axes Numeric. Number of axes to return
#' @param cuts Numeric. Number of cuts along PC1. PCn gets cuts/n cuts.
#' @param intstyle Character. Method passed to classInt::classIntervals.
#'
#' @return List of pca outputs.
#' @export
#'
#' @examples
  env_pca <- function(envdf, axes = 3, cuts = 20, intstyle = "quantile") {

    # assumes each row has a unique 'cell' id (cell number from raster)

    pcaEnv <- envdf %>%
      janitor::remove_constant()

    pca <- stats::prcomp(pcaEnv[,-1]
                   , center = TRUE
                   , scale. = TRUE
                   )

    pcaEnvDf <- tibble::tibble(cell = pcaEnv %>%
                       dplyr::pull(cell)
                       ) %>%
      dplyr::bind_cols(factoextra::get_pca_ind(pca)$coord[,1:axes] %>%
                         tibble::as_tibble() %>%
                         stats::setNames(paste0("PC",1:ncol(.)))
                       )

    pcaVarDf <- factoextra::get_pca_var(pca)$coord[,1:axes] %>%
      tibble::as_tibble(rownames = "name") %>%
      stats::setNames(gsub("Dim.","PC",names(.)))

    pcaEnvDfLong <- pcaEnvDf %>%
      tidyr::pivot_longer(contains("PC"),names_to = "PC", values_to = "value")

    # breakpoints for classes in first x PCs
    pcabrks <- pcaEnvDfLong %>%
      tidyr::nest(data = -PC) %>%
      dplyr::mutate(id = row_number()
                    , brks = purrr::map2(data
                                         ,id
                                         ,~unique(c(-Inf
                                                    ,classInt::classIntervals(.x$value, cuts/.y, style = intstyle)$brks
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
      dplyr::left_join(pcabrks[,c("PC","brks")]) %>%
      dplyr::mutate(cutPC = purrr::map2(value,brks,~cut(.x,breaks=unique(unlist(.y))))) %>%
      dplyr::select(-brks) %>%
      tidyr::pivot_wider(names_from = "PC", values_from = c(value,"cutPC")) %>%
      stats::setNames(gsub("value_|PC_","",names(.))) %>%
      tidyr::unnest(cols = contains("PC"))

    # Generate colours for PCAs
    pcaCellDf <- pcaEnvDf %>%
      dplyr::left_join(pcaEnvRes) %>%
      dplyr::mutate(across(where(is.factor),factor)) %>%
      dplyr::mutate(across(where(is.factor)
                           , ~as.numeric(.)/length(levels(.))
                           , .names = "rgb{col}"
                           )
                    ) %>%
      stats::setNames(gsub("rgbcutPC","",names(.))) %>%
      dplyr::mutate(colour = grDevices::rgb(`1`,`2`,`3`)
                    , pcGroup = paste0(cutPC1,cutPC2,cutPC3)
                    )

    pcaPalette <- pcaCellDf %>%
      dplyr::distinct(pcGroup,colour) %>%
      dplyr::pull(colour,name = pcGroup)

    endResult <- lapply(ls(pattern = "pca"), get)

    names(endResult) <- ls(pattern = "pca")

    return(endResult)

  }

  effort_model <- function(df
                           , envprcomp
                           , visit = "cell"
                           , doIter = 1000
                           , doChains = 3
                           , threshold = 0.05
                           ) {

    modY <- "sr"

    modExp <- df %>%
      dplyr::filter(!is.na(qsize)
                    , qsize >= 3*3
                    ) %>%
      dplyr::distinct(Taxa,across(all_of(visit))) %>%
      dplyr::count(across(all_of(visit)),name = "sr") %>%
      dplyr::inner_join(envprcomp$pcaEnvDf) %>%
      dplyr::mutate(adjSR = sr - 1
                    #, sinMonth = sin(2*pi*month/length(month.name))
                    #, cosMonth = cos(2*pi*month/length(month.name))
                    ) %>%
      dplyr::select(!!ensym(modY),everything()) %>%
      tidyr::pivot_longer(contains("PC"),names_to = "PC") %>%
      dplyr::left_join(envprcomp$pcabrks[,c("PC","brks")]) %>%
      dplyr::mutate(cutPC = purrr::map2(value,brks,~cut(.x,breaks=unique(unlist(.y))))) %>%
      dplyr::select(-brks) %>%
      tidyr::pivot_wider(names_from = "PC", values_from = c(value,"cutPC")) %>%
      setNames(gsub("value_|PC_","",names(.))) %>%
      tidyr::unnest(cols = 1:ncol(.))  %>%
      tidyr::unnest(cols = grep("cutPC",names(.),value = TRUE))

    #--------model-------

    mod <- rstanarm::stan_glm(data = modExp

                  , formula = as.formula(paste0(modY, " ~ PC1 + PC2 + PC3"))

                  # Negative binomial
                  , family = neg_binomial_2()

                  # Options
                  , iter = doIter
                  , chains = doChains
                  )

    preds <- envprcomp$pcabrks %>%
      dplyr::pull(mids, name = PC) %>%
      purrr::cross_df() %>%
      tidyr::pivot_longer(1:ncol(.),names_to = "PC") %>%
      dplyr::left_join(envprcomp$pcabrks[,c("PC","brks")]) %>%
      dplyr::mutate(cutPC = purrr::map2(value,brks,~cut(.x,breaks=unique(unlist(.y))))) %>%
      dplyr::select(-brks) %>%
      tidyr::pivot_wider(names_from = "PC", values_from = c(value,"cutPC")) %>%
      setNames(gsub("value_|PC_","",names(.))) %>%
      tidyr::unnest(cols = 1:ncol(.))  %>%
      tidyr::unnest(cols = grep("cutPC",names(.),value = TRUE)) %>%
      dplyr::inner_join(modExp %>%
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
      (function(x) dplyr::bind_cols(x %>% dplyr::select(-value),sr = as.numeric(x$value)))


     #------residuals--------

    modResid <- tibble::tibble(fitted = fitted(mod)
                               , residual = residuals(mod)
                               ) %>%
      dplyr::mutate(standResid = residual/sd(.$residual)) %>%
      dplyr::bind_cols(modExp)

    modResidPlot <- ggplot(modResid,aes(fitted,standResid)) +
      geom_point() +
      geom_smooth()


    #--------result---------

    modRes <- modPred %>%
      dplyr::group_by(across(contains("PC"))) %>%
      dplyr::summarise(runs = n()
                       , nCheck = nrow(as_tibble(mod))
                       , modMed = quantile(sr,0.5,na.rm=TRUE)
                       , modMean = mean(sr,na.rm=TRUE)
                       , modci90lo = quantile(sr, 0.05,na.rm=TRUE)
                       , modci90up = quantile(sr, 0.95,na.rm=TRUE)
                       , extremeSRlo = quantile(sr, probs = 0 + threshold/2, na.rm=TRUE)
                       , extremeSRhi = quantile(sr, probs = 1 - threshold/2, na.rm=TRUE)
                       , text = paste0(round(modMed,2)," (",round(modci90lo,2)," to ",round(modci90up,2),")")
                       ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_if(is.numeric,round,2) %>%
      dplyr::mutate(pcGroup = paste0(cutPC1,cutPC2,cutPC3))


    #--------explore---------

    modMedPlot <- ggplot(modRes,aes(cutPC1,modMed,colour = cutPC3)) +
      geom_point() +
      facet_wrap(~cutPC2, scales = "free_y") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_colour_viridis_d()

    modMeanPlot <- ggplot(modRes,aes(cutPC1,modMean,colour = cutPC3)) +
      geom_point() +
      facet_wrap(~cutPC2, scales = "free_y") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_colour_viridis_d()


    #-------cell results-------

    modCellQSize <- df %>%
      dplyr::inner_join(modExp) %>%
      dplyr::distinct(cell,year,month,qsize)

    modCellResult <- df %>%
      dplyr::count(cell,qsize,name = "sr") %>%
      dplyr::inner_join(envprcomp$pcaCellDf %>%
                          dplyr::select(cell,pcGroup,colour)
                        ) %>%
      dplyr::inner_join(modRes) %>%
      dplyr::mutate(keepHi = sr < extremeSRhi
                    , keepLo = sr > extremeSRlo
                    , keepqSize = !(qsize == 0 | is.na(qsize))
                    , keep = as.logical(keepHi*keepLo)
                    , keep = if_else(!keep,keepqSize,keep)
                    ) %>%
      dplyr::mutate(colour = if_else(keep,"black",colour))

    modCellPlot <- ggplot(modCellResult,aes(cutPC1,sr,colour=colour)) +
      geom_jitter() +
      facet_grid(cutPC2~cutPC3) +
      coord_cartesian(y = c(0,max(modCellResult$sr[modCellResult$keepHi == TRUE]))) +
      scale_colour_identity() +
      theme_dark() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    modCellTab <- modCellResult %>%
      dplyr::count(keepHi,keepLo,keepqSize,keep)

    endResult <- lapply(ls(pattern = "mod"), get)

    names(endResult) <- ls(pattern = "mod")

    return(endResult)

  }


  effort_filter <- function(argsenv
                            , argsenvpca
                            , argseffortmodel
                            ) {

    florEnv <- do.call(create_env,args = argsenv) %>%
      na.omit()

    argsenvpca <- c(list(envdf = florEnv),argsenvpca)

    envpca <- do.call(env_pca,args = argsenvpca)

    argseffortmodel <- c(list(df = argsenv$df, envprcomp = envpca),argseffortmodel)

    effortmod <- do.call(effort_model,args = argseffortmodel)

    toAssign <- c("florEnv","envpca","effortmod")

    walk(toAssign,~assign(.,get(.),envir = globalenv()))

    argsenv$df %>%
      dplyr::inner_join(effortmod$modCellResult %>%
                          dplyr::filter(keep)
                        ) %>%
      dplyr::select(names(argsenv$df),grep("cutPC",names(.),value = TRUE)) %>%
      dplyr::distinct()

  }

