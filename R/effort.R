


#' Principal components analysis and various outputs from environmental data
#'
#' @param env_df Dataframe containing 'cell' and environmental data.
#' @param context Character. Name of columns defining context.
#' @param axes Numeric. Number of axes to return.
#' @param cuts Numeric. Number of cuts along pc1. pcn gets cuts/n cuts.
#' @param int_style Character. Method passed to classInt::classIntervals.
#' @param out_file Character. Optional path to save output.
#'
#' @return List of pca outputs.
#' @export
#'
#' @examples
  make_env_pca <- function(env_df
                           , context = "cell"
                           , axes = 3
                           , cuts = 20
                           , int_style = "quantile"
                           , out_file = NULL
                           ) {

    env_pca <- list()

    env_pca$pca_data <- env_df %>%
      janitor::remove_constant() %>%
      stats::na.omit()

    exclude_cols <- grep(paste0(context
                                , collapse = "|"
                                )
                         , names(env_df)
                         )

    env_pca$pca_pca <- stats::prcomp(env_pca$pca_data[,-exclude_cols]
                   , center = TRUE
                   , scale. = TRUE
                   )

    env_pca$pca_res_cell <- env_pca$pca_data %>%
      dplyr::select(tidyselect::any_of(context)) %>%
      dplyr::bind_cols(factoextra::get_pca_ind(env_pca$pca_pca)$coord[,1:axes] %>%
                         tibble::as_tibble() %>%
                         stats::setNames(paste0("pc",1:ncol(.)))
                       )

    env_pca$pca_res_var <- factoextra::get_pca_var(env_pca$pca_pca)$coord[,1:axes] %>%
      tibble::as_tibble(rownames = "name") %>%
      stats::setNames(gsub("Dim.","pc",names(.)))

    env_pca$pca_res_cell_long <- env_pca$pca_res_cell %>%
      tidyr::pivot_longer(contains("pc"),names_to = "pc", values_to = "value")

    # breakpoints for classes in first x pcs
    env_pca$pca_brks <- env_pca$pca_res_cell_long %>%
      tidyr::nest(data = -pc) %>%
      dplyr::mutate(id = dplyr::row_number()
                    , brks = purrr::map2(data
                                         ,id
                                         ,~unique(c(-Inf
                                                    ,classInt::classIntervals(.x$value, cuts/.y, style = int_style)$brks
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

    # Put breaks back into pcaEnvres
    env_pca$pca_res_cell_cut <- env_pca$pca_res_cell_long %>%
      dplyr::left_join(env_pca$pca_brks[,c("pc","brks")]) %>%
      dplyr::mutate(cut_pc = purrr::map2(value,brks,~cut(.x,breaks=unique(unlist(.y))))) %>%
      dplyr::select(-brks) %>%
      tidyr::pivot_wider(names_from = "pc", values_from = c(value,"cut_pc")) %>%
      stats::setNames(gsub("value_|pc_","",names(.))) %>%
      tidyr::unnest(cols = contains("pc"))

    # Generate colours for pcas
    env_pca$pca_res_col <- env_pca$pca_res_cell_cut %>%
      dplyr::left_join(env_pca$pca_res_cell) %>%
      dplyr::mutate(dplyr::across(where(is.factor),factor)) %>%
      dplyr::mutate(dplyr::across(where(is.factor)
                           , ~as.numeric(.)/length(levels(.))
                           , .names = "rgb_{col}"
                           )
                    ) %>%
      stats::setNames(gsub("rgb_cut_pc","",names(.))) %>%
      dplyr::mutate(colour = grDevices::rgb(`1`,`2`,`3`)
                    , pc_group = paste0(cut_pc1,cut_pc2,cut_pc3)
                    )

    env_pca$pca_palette <- env_pca$pca_res_col %>%
      dplyr::distinct(pc_group,colour) %>%
      dplyr::pull(colour,name = pc_group)

    if(!is.null(out_file)) {

      fs::dir_create(dirname(out_file))

      rio::export(env_pca
                    , out_file
                    )

    }

    invisible(env_pca)

  }

#' Model the effect of principal components axes on taxa richness.
#'
#' @param df Dataframe. Cleaned data specifying context.
#' @param env_prcomp Output from env_pca.
#' @param context Character. Column names that define context, usually a 'visit'
#' to a 'cell'.
#' @param threshold_lo,threshold_hi Numeric between 0 and 1 specifying the
#' threshold above/below which richness is excessively above or below 'normal'
#' and should be filtered.
#' @param effort_col Character (or `NULL`). Name of column with some measure of
#' effort. If `NULL`, all contexts will be used.
#' @param effort_thresh Numeric. `effort_col` (if used) will be filtered below
#' this threshold.
#' @param out_file Character. Optional path to save output.
#' @param ... Passed to [rstanarm::stan_glm()].
#'
#'
#' @return List of model outputs.
#' @export
#'
#' @examples
  make_effort_mod_pca <- function(df
                           , env_prcomp
                           , context = "cell"
                           , threshold_lo = 0.05/2
                           , threshold_hi = 0.05/2
                           , effort_col = "qsize"
                           , effort_thresh = 3*3
                           , out_file = NULL
                           , ...
                           ) {

    df <- if(!is.null(effort_col)) {

      df %>%
        dplyr::filter(!is.na(!!rlang::ensym(effort_col))
                      , !!ensym(effort_col) >= effort_thresh
                      )

    } else df

    effort_mod <- list()

    response <- "sr"

    effort_mod$dat_exp <- df %>%
        dplyr::distinct(taxa
                        , dplyr::across(tidyselect::any_of(context))
                        ) %>%
        dplyr::count(dplyr::across(tidyselect::any_of(context))
                     , name = "sr"
                     ) %>%
        dplyr::inner_join(env_prcomp$pca_res_cell) %>%
        dplyr::select(!!rlang::ensym(response)
                      , everything()
                      ) %>%
        tidyr::pivot_longer(contains("pc")
                            , names_to = "pc"
                            ) %>%
        dplyr::left_join(env_prcomp$pca_brks[,c("pc","brks")]) %>%
        dplyr::mutate(cut_pc = purrr::map2(value
                                           , brks
                                           , ~cut(.x,breaks=unique(unlist(.y)))
                                           )
                      ) %>%
        dplyr::select(-brks) %>%
        tidyr::pivot_wider(names_from = "pc"
                           , values_from = c(value,"cut_pc")
                           ) %>%
        stats::setNames(gsub("value_|pc_"
                             , ""
                             , names(.)
                             )
                        ) %>%
        tidyr::unnest(cols = 1:ncol(.))

    #--------model-------

    pcs <- grep("^pc\\d{1}", names(effort_mod$dat_exp), value = TRUE)

    effort_mod$mod <- rstanarm::stan_glm(data = effort_mod$dat_exp

                  , formula = stats::as.formula(paste0(response
                                                       , " ~ "
                                                       , paste0(pcs, collapse = "+")
                                                       )
                                                )

                  # Negative binomial
                  , family = rstanarm::neg_binomial_2()

                  # Options
                  , ...
                  )

    effort_mod$preds <- env_prcomp$pca_brks %>%
      dplyr::pull(mids, name = pc) %>%
      purrr::cross_df() %>%
      tidyr::pivot_longer(1:ncol(.),names_to = "pc") %>%
      dplyr::left_join(env_prcomp$pca_brks[,c("pc","brks")]) %>%
      dplyr::mutate(cut_pc = purrr::map2(value
                                         , brks
                                         , ~cut(.x,breaks=unique(unlist(.y)))
                                         )
                    ) %>%
      dplyr::select(-brks) %>%
      tidyr::pivot_wider(names_from = "pc", values_from = c(value,"cut_pc")) %>%
      stats::setNames(gsub("value_|pc_","",names(.))) %>%
      tidyr::unnest(cols = 1:ncol(.))  %>%
      tidyr::unnest(cols = grep("cut_pc",names(.),value = TRUE))

    effort_mod$mod_pred <- effort_mod$preds %>%
      dplyr::mutate(col = row.names(.)) %>%
      dplyr::left_join(tibble::as_tibble(rstanarm::posterior_predict(effort_mod$mod
                                                   , newdata = .
                                                   , re.form = NA
                                                   )
                                 ) %>%
                         tibble::rownames_to_column(var = "row") %>%
                         tidyr::gather(col,value,2:ncol(.))
                       ) %>%
      (function(x) dplyr::bind_cols(x %>% dplyr::select(-value),sr = as.numeric(x$value)))


     #------residuals--------

    effort_mod$mod_resid <- tibble::tibble(fitted = stats::fitted(effort_mod$mod)
                               , residual = stats::residuals(effort_mod$mod)
                               ) %>%
      dplyr::mutate(stand_resid = residual/stats::sd(.$residual)) %>%
      dplyr::bind_cols(effort_mod$dat_exp)

    effort_mod$mod_resid_plot <- ggplot2::ggplot(effort_mod$mod_resid
                                                 ,aes(fitted,stand_resid)
                                                 ) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth()


    #--------result---------

    effort_mod$mod_res <- effort_mod$mod_pred %>%
      dplyr::group_by(dplyr::across(contains("pc"))) %>%
      dplyr::summarise(runs = n()
                       , n_check = nrow(tibble::as_tibble(effort_mod$mod))
                       , mod_med = stats::quantile(sr,0.5,na.rm=TRUE)
                       , mod_mean = mean(sr,na.rm=TRUE)
                       , mod_ci90_lo = stats::quantile(sr, 0.05,na.rm=TRUE)
                       , mod_ci90_up = stats::quantile(sr, 0.95,na.rm=TRUE)
                       , extreme_sr_lo = stats::quantile(sr, probs = 0 + threshold_lo, na.rm=TRUE)
                       , extreme_sr_hi = stats::quantile(sr, probs = 1 - threshold_hi, na.rm=TRUE)
                       , text = paste0(round(mod_med,2)," (",round(mod_ci90_lo,2)," to ",round(mod_ci90_up,2),")")
                       ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_if(is.numeric,round,2) %>%
      dplyr::mutate(pc_group = paste0(cut_pc1,cut_pc2,cut_pc3))


    #--------explore---------

    effort_mod$mod_med_plot <- ggplot2::ggplot(effort_mod$mod_res
                                               ,aes(cut_pc1
                                                    , mod_med
                                                    , colour = cut_pc3
                                                    )
                                               ) +
      ggplot2::geom_jitter() +
      ggplot2::facet_wrap(~cut_pc2, scales = "free_y") +
      ggplot2::theme(axis.text.x = element_text(angle = 90
                                                , vjust = 0.5
                                                , hjust=1
                                                )
                     ) +
      ggplot2::scale_colour_viridis_d()

    effort_mod$mod_mean_plot <- ggplot2::ggplot(effort_mod$mod_res
                                                ,aes(cut_pc1
                                                     , mod_mean
                                                     , colour = cut_pc3
                                                     )
                                                ) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~cut_pc2, scales = "free_y") +
      ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
      ggplot2::scale_colour_viridis_d()


    #-------cell results-------

    qsize <- df %>%
      dplyr::distinct(across(any_of(context))
                      , across(any_of(effort_col))
                      )

    context_sr <- df %>%
      dplyr::count(across(any_of(context))
                   , name = "sr"
                   )

    effort_mod$mod_cell_result <- env_prcomp$pca_res_col %>%
      dplyr::left_join(qsize) %>%
      dplyr::inner_join(context_sr) %>%
      dplyr::select(!matches("^pc")) %>%
      dplyr::inner_join(effort_mod$mod_res) %>%
      dplyr::mutate(keep_hi = sr < extreme_sr_hi
                    , keep_lo = sr > extreme_sr_lo
                    , keep_qsize = !(!!rlang::ensym(effort_col) == 0 | is.na(!!rlang::ensym(effort_col)))
                    , keep = as.logical(keep_hi*keep_lo)
                    , keep = if_else(!keep,keep_qsize,keep)
                    ) %>%
      dplyr::mutate(colour = if_else(keep,"black",colour))

    max_y <- max(effort_mod$mod_cell_result$sr[effort_mod$mod_cell_result$keep_hi == TRUE])

    effort_mod$mod_cell_plot <- ggplot2::ggplot(effort_mod$mod_cell_result
                                                ,aes(cut_pc1
                                                     , sr
                                                     , colour = colour
                                                     )
                                                ) +
      ggplot2::geom_jitter() +
      ggplot2::facet_grid(cut_pc2~cut_pc3) +
      ggplot2::coord_cartesian(y = c(0,max_y)) +
      ggplot2::scale_colour_identity() +
      ggplot2::theme_dark() +
      ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    effort_mod$mod_cell_tab <- effort_mod$mod_cell_result %>%
      dplyr::count(keep_hi,keep_lo,keep_qsize,keep)

    if(!is.null(out_file)) {

      fs::dir_create(dirname(out_file))

      rio::export(effort_mod
                  , out_file
                  )

    }

    invisible(effort_mod)

  }

