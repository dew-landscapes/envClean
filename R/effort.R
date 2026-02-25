


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
      stats::na.omit() |>
      dplyr::distinct()

    exclude_cols <- grep(paste0(context
                                , collapse = "|"
                                )
                         , names(env_df)
                         )

    env_pca$pca_pca <- stats::prcomp(env_pca$pca_data[,-exclude_cols]
                                     , center = TRUE
                                     , scale. = TRUE
                                     , rank. = axes
                                     )

    env_pca$pca_res_cell <- env_pca$pca_data %>%
      dplyr::select(tidyselect::any_of(context)) %>%
      dplyr::bind_cols(factoextra::get_pca_ind(env_pca$pca_pca)$coord[,1:axes] %>%
                         tibble::as_tibble() %>%
                         stats::setNames(paste0("pc",1:ncol(.)))
                       )

    env_pca$pca_res_var <- suppressWarnings(factoextra::get_pca_var(env_pca$pca_pca)$coord[,1:axes]) %>% # this generates a warning if axes is less than the number of env variables
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
                    , mids = purrr::map(brks,. %>% dplyr::mutate(mid = (brks + dplyr::lead(brks)) / 2))
                    , mids = purrr::map(mids,. %>%
                                          dplyr::mutate(mid = dplyr::if_else(mid == -Inf
                                                                             , min(.$brks[is.finite(.$brks)]) + min(.$mid[is.finite(.$mid)])
                                                                             , mid
                                                                             )
                                          , mid = dplyr::if_else(mid == Inf
                                                                 , max(.$brks[is.finite(.$brks)]) + max(.$mid[is.finite(.$mid)])
                                                                 , mid
                                                          )
                                          ) %>%
                                          dplyr::filter(!is.na(mid) & mid != Inf & mid!= -Inf) %>%
                                          dplyr::pull(mid)
                                        )
                    )

    # Put breaks back into pcaEnvres
    env_pca$pca_res_cell_cut <- env_pca$pca_res_cell_long %>%
      tidyr::nest(data = -pc) |>
      dplyr::left_join(env_pca$pca_brks[,c("pc","brks")]) %>%
      dplyr::mutate(cut_pc = purrr::map2(data
                                         , brks
                                         , \(x, y) cut(x$value
                                                       , breaks = unique(unlist(y))
                                                       )
                                         )
                    ) %>%
      dplyr::select(-brks) %>%
      tidyr::unnest(cols = c(data, cut_pc)) |>
      tidyr::pivot_wider(names_from = "pc", values_from = c(value, "cut_pc")) %>%
      stats::setNames(gsub("value_|pc_", "", names(.))) %>%
      tidyr::unnest(cols = matches("pc"))

    # Generate colours for pcas
    env_pca$pca_res_col <- env_pca$pca_res_cell_cut %>%
      dplyr::mutate(dplyr::across(where(is.factor),factor)) %>%
      dplyr::mutate(dplyr::across(where(is.factor)
                           , \(x) as.numeric(x)/length(levels(x))
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

#' Model the effect of principal components on taxa richness.
#'
#' @param df Dataframe. Cleaned data specifying context.
#' @param env_prcomp Output from env_pca.
#' @param context Character. Column names that define context, usually a 'visit'
#' to a 'cell'.
#' @param cores Numeric. Number of cores available for running chains in
#' rstanarm model.
#' @param response Character. Name to give the 'response' variable column.
#' Default is 'sr' for 'species richness'.
#' @param response_min_thresh Numeric. Threshold below which to filter `df`
#' before running the model. Default '2' excludes singleton sites.
#' @param threshold_lo,threshold_hi Numeric between 0 and 1 specifying the
#' threshold above/below which richness is excessively above or below 'normal'
#' and should be filtered.
#' @param effort_col Character (or `NULL`). Name of column with some measure of
#' effort. If `NULL`, all contexts will be used.
#' @param effort_col_thresh Numeric. `effort_col` (if used) will be filtered
#' below this threshold. This could be used to, say, filter very small quadrats.
#' @param out_file Character. Optional path to save output.
#' @param ... Passed to `rstanarm::stan_glm()`.
#'
#'
#' @return List of model outputs.
#' @export
#'
#' @examples
  make_effort_mod_pca <- function(df
                                  , env_prcomp
                                  , context = "cell"
                                  , cores = 4
                                  , response = "sr"
                                  , response_min_thresh = 2
                                  , threshold_lo = 0.05 / 2
                                  , threshold_hi = 0.05 / 2
                                  , effort_col = "qsize"
                                  , effort_col_thresh = 3 * 3
                                  , out_file = NULL
                                  , ...
                                  ) {

    # stan options
    old_cores <- options()$mc.cores
    options(mc.cores = cores)
    rstan::rstan_options(auto_write = TRUE)

    df_for_mod <- if(!is.null(effort_col)) {

      df %>%
        dplyr::filter(!is.na(!!rlang::ensym(effort_col))
                      , !!rlang::ensym(effort_col) >= effort_col_thresh
                      )

    } else df

    effort_mod <- list()

    # all the data. used later to determine which sites are in/out
    effort_mod$dat_all <- df %>%
      dplyr::distinct(taxa
                      , dplyr::across(tidyselect::any_of(context))
                      ) %>%
      dplyr::count(dplyr::across(tidyselect::any_of(context))
                   , name = response
                   )

    # only the data for the model (taking into account effort_col/effort_col_thresh)
    effort_mod$dat_exp <- df_for_mod |>
      dplyr::distinct(taxa
                      , dplyr::across(tidyselect::any_of(context))
                      ) %>%
      dplyr::count(dplyr::across(tidyselect::any_of(context))
                   , name = response
                   ) |>
      dplyr::filter(!!rlang::ensym(response) >= response_min_thresh) %>%
      dplyr::inner_join(env_prcomp$pca_res_cell_cut |> dplyr::distinct()) %>%
      dplyr::select(!!rlang::ensym(response)
                    , everything()
                    )

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

    blah <- env_prcomp$pca_brks %>%
      dplyr::pull(mids, name = pc)

    effort_mod$preds <- expand.grid(blah) |>
      tibble::as_tibble() %>%
      tidyr::pivot_longer(1:ncol(.), names_to = "pc") %>%
      tidyr::nest(value = value) |>
      dplyr::left_join(env_prcomp$pca_brks[, c("pc", "brks")]) %>%
      dplyr::mutate(cut_pc = purrr::map2(value
                                         , brks
                                         , \(a, b) cut(a$value, breaks = unique(unlist(b)))
                                         )
                    ) %>%
      dplyr::select(-brks) %>%
      tidyr::unnest(cols = c(value, cut_pc)) |>
      tidyr::pivot_wider(names_from = "pc", values_from = c(value, "cut_pc")) %>%
      stats::setNames(gsub("value_|pc_", "", names(.))) %>%
      tidyr::unnest(cols = 1:ncol(.))

    effort_mod$mod_pred <- effort_mod$preds %>%
      tidybayes::add_predicted_draws(effort_mod$mod
                                     , re_formula = NA
                                     , value = response
                                     ) %>%
      dplyr::ungroup()


     #------residuals--------

    effort_mod$mod_resid <- tibble::tibble(fitted = stats::fitted(effort_mod$mod)
                                           , residual = stats::residuals(effort_mod$mod)
                                           ) %>%
      dplyr::mutate(stand_resid = residual / stats::sd(.$residual)) %>%
      dplyr::bind_cols(effort_mod$dat_exp)

    effort_mod$mod_resid_plot <- ggplot2::ggplot(effort_mod$mod_resid
                                                 , ggplot2::aes(fitted, stand_resid)
                                                 ) +
      ggplot2::geom_point() +
      ggplot2::geom_smooth()


    #--------result---------

    effort_mod$mod_res <- effort_mod$mod_pred %>%
      dplyr::group_by(dplyr::across(contains("pc"))) %>%
      dplyr::summarise(runs = dplyr::n()
                       , n_check = nrow(tibble::as_tibble(effort_mod$mod))
                       , mod_med = stats::quantile(!!rlang::ensym(response), 0.5, na.rm = TRUE)
                       , mod_mean = mean(!!rlang::ensym(response), na.rm = TRUE)
                       , mod_ci90_lo = stats::quantile(!!rlang::ensym(response), 0.05, na.rm = TRUE)
                       , mod_ci90_up = stats::quantile(!!rlang::ensym(response), 0.95, na.rm = TRUE)
                       , extreme_sr_lo = stats::quantile(!!rlang::ensym(response), probs = 0 + threshold_lo, na.rm=TRUE)
                       , extreme_sr_hi = stats::quantile(!!rlang::ensym(response), probs = 1 - threshold_hi, na.rm=TRUE)
                       ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate(pc_group = paste0(cut_pc1, cut_pc2, cut_pc3))


    #--------explore---------

    effort_mod$mod_med_plot <- ggplot2::ggplot(effort_mod$mod_res
                                               , ggplot2::aes(cut_pc1
                                                              , mod_med
                                                              , colour = cut_pc3
                                                              )
                                               ) +
      ggplot2::geom_jitter() +
      ggplot2::facet_wrap(~cut_pc2, scales = "free_y") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90
                                                         , vjust = 0.5
                                                         , hjust = 1
                                                         )
                     ) +
      ggplot2::scale_colour_viridis_d()

    effort_mod$mod_mean_plot <- ggplot2::ggplot(effort_mod$mod_res
                                                , ggplot2::aes(cut_pc1
                                                               , mod_mean
                                                               , colour = cut_pc3
                                                               )
                                                ) +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(~ cut_pc2, scales = "free_y") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      ggplot2::scale_colour_viridis_d()


    #-------cell results-------

    if(!is.null(effort_col)) {

      eff <- df %>%
        dplyr::distinct(across(any_of(context))
                        , across(any_of(effort_col))
                        )

    }

    effort_mod$mod_cell_result <- env_prcomp$pca_res_col %>%
      {if(!is.null(effort_col)) (.) %>% dplyr::left_join(eff) else (.)} %>%
      dplyr::inner_join(effort_mod$dat_all) %>%
      dplyr::select(! tidyselect::matches("^pc")) %>%
      dplyr::left_join(effort_mod$mod_res) %>%
      dplyr::mutate(keep_hi = !!rlang::ensym(response) <= extreme_sr_hi
                    , keep_lo = !!rlang::ensym(response) >= extreme_sr_lo
                    , keep_qsize = FALSE
                    ) %>%
      {if(!is.null(effort_col)) (.) %>% dplyr::mutate(keep_qsize = !(!!rlang::ensym(effort_col) == 0 | is.na(!!rlang::ensym(effort_col)))) else (.)} %>%
      dplyr::mutate(keep = as.logical(keep_hi * keep_lo)
                    , keep = dplyr::if_else(!keep, keep_qsize, keep)
                    ) %>%
      dplyr::mutate(colour = dplyr::if_else(keep, "black", colour)) |>
      dplyr::distinct()

    max_y <- max(effort_mod$mod_cell_result[[response]][effort_mod$mod_cell_result$keep_hi == TRUE])

    effort_mod$mod_cell_plot <- ggplot2::ggplot(effort_mod$mod_cell_result
                                                , ggplot2::aes(cut_pc1
                                                               , !!rlang::ensym(response)
                                                               , colour = colour
                                                               )
                                                ) +
      ggplot2::geom_jitter() +
      ggplot2::facet_grid(cut_pc2 ~ cut_pc3) +
      ggplot2::coord_cartesian(y = c(0, max_y)) +
      ggplot2::scale_colour_identity() +
      ggplot2::theme_dark() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1))

    effort_mod$mod_cell_tab <- effort_mod$mod_cell_result %>%
      dplyr::count(keep_hi, keep_lo, keep_qsize, keep)

    if(!is.null(out_file)) {

      fs::dir_create(dirname(out_file))

      rio::export(effort_mod
                  , out_file
                  )

    }

    options(mc.cores = old_cores)

    return(invisible(effort_mod))

  }

