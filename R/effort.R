


#' Principal components analysis and various outputs from environmental data
#'
#' @param envdf Dataframe containing 'cell' and environmental data.
#' @param axes Numeric. Number of axes to return.
#' @param cuts Numeric. Number of cuts along pc1. pcn gets cuts/n cuts.
#' @param intstyle Character. Method passed to classInt::classIntervals.
#'
#' @return List of pca outputs.
#' @export
#'
#' @examples
  env_pca <- function(envdf, axes = 3, cuts = 20, intstyle = "quantile") {

    # assumes each row has a unique 'cell' id (cell number from raster)

    envpca <- list()

    envpca$pcadata <- envdf %>%
      janitor::remove_constant() %>%
      na.omit()

    envpca$pcapca <- stats::prcomp(envpca$pcaData[,-1]
                   , center = TRUE
                   , scale. = TRUE
                   )

    envpca$pcarescell <- envpca$pcaData %>%
      dplyr::select(cell) %>%
      dplyr::bind_cols(factoextra::get_pca_ind(envpca$pcapca)$coord[,1:axes] %>%
                         tibble::as_tibble() %>%
                         stats::setNames(paste0("pc",1:ncol(.)))
                       )

    envpca$pcaresvar <- factoextra::get_pca_var(envpca$pcapca)$coord[,1:axes] %>%
      tibble::as_tibble(rownames = "name") %>%
      stats::setNames(gsub("Dim.","pc",names(.)))

    envpca$pcarescelllong <- envpca$pcarescell %>%
      tidyr::pivot_longer(contains("pc"),names_to = "pc", values_to = "value")

    # breakpoints for classes in first x pcs
    envpca$pcabrks <- envpca$pcarescelllong %>%
      tidyr::nest(data = -pc) %>%
      dplyr::mutate(id = dplyr::row_number()
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
                                          dplyr::mutate(mid = dplyr::if_else(mid == -Inf
                                                                             , min(.$brks[is.finite(.$brks)])+min(.$mid[is.finite(.$mid)])
                                                                             , mid
                                                                             )
                                          , mid = dplyr::if_else(mid == Inf
                                                                 , max(.$brks[is.finite(.$brks)])+max(.$mid[is.finite(.$mid)])
                                                                 , mid
                                                          )
                                          ) %>%
                                          dplyr::filter(!is.na(mid) & mid != Inf & mid!= -Inf) %>%
                                          dplyr::pull(mid)
                                        )
                    )

    print(envpca$pcabrks)

    # Put breaks back into pcaEnvres
    envpca$pcarescellCut <- envpca$pcarescelllong %>%
      dplyr::left_join(envpca$pcabrks[,c("pc","brks")]) %>%
      dplyr::mutate(cutpc = purrr::map2(value,brks,~cut(.x,breaks=unique(unlist(.y))))) %>%
      dplyr::select(-brks) %>%
      tidyr::pivot_wider(names_from = "pc", values_from = c(value,"cutpc")) %>%
      stats::setNames(gsub("value_|pc_","",names(.))) %>%
      tidyr::unnest(cols = contains("pc"))

    # Generate colours for pcas
    envpca$pcarescellCutCol <- envpca$pcarescellCut %>%
      dplyr::left_join(envpca$pcarescell) %>%
      dplyr::mutate(across(where(is.factor),factor)) %>%
      dplyr::mutate(across(where(is.factor)
                           , ~as.numeric(.)/length(levels(.))
                           , .names = "rgb{col}"
                           )
                    ) %>%
      stats::setNames(gsub("rgbcutpc","",names(.))) %>%
      dplyr::mutate(colour = grDevices::rgb(`1`,`2`,`3`)
                    , pcgroup = paste0(cutpc1,cutpc2,cutpc3)
                    )

    envpca$pcapalette <- envpca$pcarescellCutCol %>%
      dplyr::distinct(pcgroup,colour) %>%
      dplyr::pull(colour,name = pcgroup)

    invisible(envpca)

  }

#' Model the effect of principal components axes on taxa richness.
#'
#' @param df Dataframe. Cleaned data specifying context.
#' @param envprcomp Output from env_pca.
#' @param context Character. Column names that define context, usually a 'visit'
#' to a 'cell'.
#' @param doIter Numeric specifying the number of iterations to run for each
#' chain in rstan analysis.
#' @param doChains Numeric specifying the number of chains to run in rstan
#' analysis
#' @param threshold Numeric between 0 and 1 specifying the two-tailed threshold
#' above/below which richness is excessively above or below 'normal' for
#'
#' @return
#' @export
#'
#' @examples
  effort_model <- function(df
                           , envprcomp
                           , context = "cell"
                           , doiter = 1000
                           , dochains = 3
                           , threshold = 0.05
                           ) {

    effortmod <- list()

    modY <- "sr"

    effortmod$modExp <- df %>%
      dplyr::filter(!is.na(qsize)
                    , qsize >= 3*3
                    ) %>%
      dplyr::distinct(taxa,across(all_of(context))) %>%
      dplyr::count(across(all_of(context)),name = "sr") %>%
      dplyr::inner_join(envprcomp$pcarescell) %>%
      dplyr::select(!!ensym(modY),everything()) %>%
      tidyr::pivot_longer(contains("pc"),names_to = "pc") %>%
      dplyr::left_join(envprcomp$pcabrks[,c("pc","brks")]) %>%
      dplyr::mutate(cutpc = purrr::map2(value,brks,~cut(.x,breaks=unique(unlist(.y))))) %>%
      dplyr::select(-brks) %>%
      tidyr::pivot_wider(names_from = "pc", values_from = c(value,"cutpc")) %>%
      stats::setNames(gsub("value_|pc_","",names(.))) %>%
      tidyr::unnest(cols = 1:ncol(.))  %>%
      tidyr::unnest(cols = grep("cutpc",names(.),value = TRUE))

    #--------model-------

    effortmod$mod <- rstanarm::stan_glm(data = effortmod$modExp

                  , formula = as.formula(paste0(modY, " ~ pc1 + pc2 + pc3"))

                  # Negative binomial
                  , family = neg_binomial_2()

                  # Options
                  , iter = doiter
                  , chains = dochains
                  )

    effortmod$preds <- envprcomp$pcabrks %>%
      dplyr::pull(mids, name = pc) %>%
      purrr::cross_df() %>%
      tidyr::pivot_longer(1:ncol(.),names_to = "pc") %>%
      dplyr::left_join(envprcomp$pcabrks[,c("pc","brks")]) %>%
      dplyr::mutate(cutpc = purrr::map2(value,brks,~cut(.x,breaks=unique(unlist(.y))))) %>%
      dplyr::select(-brks) %>%
      tidyr::pivot_wider(names_from = "pc", values_from = c(value,"cutpc")) %>%
      stats::setNames(gsub("value_|pc_","",names(.))) %>%
      tidyr::unnest(cols = 1:ncol(.))  %>%
      tidyr::unnest(cols = grep("cutpc",names(.),value = TRUE)) %>%
      dplyr::inner_join(effortmod$modExp %>%
                          dplyr::distinct(across(grep("cut|month",names(.),value = TRUE)))
                        )

    effortmod$modPred <- effortmod$preds %>%
      dplyr::mutate(col = row.names(.)) %>%
      dplyr::left_join(as_tibble(posterior_predict(effortmod$mod
                                                   , newdata = .
                                                   , re.form = NA
                                                   )
                                 ) %>%
                         tibble::rownames_to_column(var = "row") %>%
                         tidyr::gather(col,value,2:ncol(.))
                       ) %>%
      (function(x) dplyr::bind_cols(x %>% dplyr::select(-value),sr = as.numeric(x$value)))


     #------residuals--------

    effortmod$modresid <- tibble::tibble(fitted = fitted(effortmod$mod)
                               , residual = residuals(effortmod$mod)
                               ) %>%
      dplyr::mutate(standresid = residual/sd(.$residual)) %>%
      dplyr::bind_cols(effortmod$modExp)

    effortmod$modresidplot <- ggplot(effortmod$modresid,aes(fitted,standresid)) +
      geom_point() +
      geom_smooth()


    #--------result---------

    effortmod$modres <- effortmod$modPred %>%
      dplyr::group_by(across(contains("pc"))) %>%
      dplyr::summarise(runs = n()
                       , nCheck = nrow(as_tibble(effortmod$mod))
                       , modmed = quantile(sr,0.5,na.rm=TRUE)
                       , modmean = mean(sr,na.rm=TRUE)
                       , modci90lo = quantile(sr, 0.05,na.rm=TRUE)
                       , modci90up = quantile(sr, 0.95,na.rm=TRUE)
                       , extremeSRlo = quantile(sr, probs = 0 + threshold/2, na.rm=TRUE)
                       , extremeSRhi = quantile(sr, probs = 1 - threshold/2, na.rm=TRUE)
                       , text = paste0(round(modmed,2)," (",round(modci90lo,2)," to ",round(modci90up,2),")")
                       ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_if(is.numeric,round,2) %>%
      dplyr::mutate(pcgroup = paste0(cutpc1,cutpc2,cutpc3))


    #--------explore---------

    effortmod$modmedplot <- ggplot(effortmod$modres,aes(cutpc1,modmed,colour = cutpc3)) +
      geom_point() +
      facet_wrap(~cutpc2, scales = "free_y") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_colour_viridis_d()

    effortmod$modmeanplot <- ggplot(effortmod$modres,aes(cutpc1,modmean,colour = cutpc3)) +
      geom_point() +
      facet_wrap(~cutpc2, scales = "free_y") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      scale_colour_viridis_d()


    #-------cell results-------

    effortmod$modcellQSize <- df %>%
      dplyr::inner_join(effortmod$modExp) %>%
      dplyr::distinct(cell,year,month,qsize)

    effortmod$modcellresult <- df %>%
      dplyr::count(cell,qsize,name = "sr") %>%
      dplyr::inner_join(envprcomp$pcarescellCutCol %>%
                          dplyr::select(cell,pcgroup,colour)
                        ) %>%
      dplyr::inner_join(effortmod$modres) %>%
      dplyr::mutate(keepHi = sr < extremeSRhi
                    , keepLo = sr > extremeSRlo
                    , keepqSize = !(qsize == 0 | is.na(qsize))
                    , keep = as.logical(keepHi*keepLo)
                    , keep = if_else(!keep,keepqSize,keep)
                    ) %>%
      dplyr::mutate(colour = if_else(keep,"black",colour))

    effortmod$modcellplot <- ggplot(effortmod$modcellresult,aes(cutpc1,sr,colour=colour)) +
      geom_jitter() +
      facet_grid(cutpc2~cutpc3) +
      coord_cartesian(y = c(0,max(effortmod$modcellresult$sr[effortmod$modcellresult$keepHi == TRUE]))) +
      scale_colour_identity() +
      theme_dark() +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    effortmod$modcelltab <- effortmod$modcellresult %>%
      dplyr::count(keepHi,keepLo,keepqSize,keep)

    invisible(effortmod)

  }


#' Run all functions required to complete effort filter
#'
#' @param argscreateenv Named list of arguments passed to create_env.
#' @param argsenvpca Named list of arguments passed to env_pca. The envdf
#' argument is generated within effort_filter so is not needed.
#' @param argseffortmodel Named list of arguments passed to effort_model. The
#' df argument is generated within effort_filter so is not needed.
#'
#' @return
#' @export
#'
#' @examples
  filter_effort <- function(argscreateenv
                            , argsenvpca
                            , argseffortmodel
                            ) {

    florEnv <- do.call(create_env,args = argscreateenv) %>%
      na.omit()

    argsenvpca <- c(list(envdf = florEnv),argsenvpca)

    envpca <- do.call(env_pca,args = argsenvpca)

    argseffortmodel <- c(list(df = argscreateenv$df, envprcomp = envpca),argseffortmodel)

    effortmod <- do.call(effort_model,args = argseffortmodel)

    toAssign <- c("florEnv","envpca","effortmod")

    walk(toAssign,~assign(.,get(.),envir = globalenv()))

    argscreateenv$df %>%
      dplyr::inner_join(effortmod$modcellresult %>%
                          dplyr::filter(keep)
                        ) %>%
      dplyr::select(names(argscreateenv$df),grep("cutpc",names(.),value = TRUE)) %>%
      dplyr::distinct()

  }

