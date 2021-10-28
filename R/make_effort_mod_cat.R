

#' Model the effect of categorical variables on taxa richness.
#'
#' @param df Dataframe. Cleaned data specifying context.
#' @param context Character. Column names that define context, usually a 'visit'
#' to a 'cell'.
#' @param cat_cols Columns (2) specifying the categorical variables to model.
#' Usually a taxonomic level (say, class) and a geographic level (say, IBRA
#' Region).
#' @param threshold_lo,threshold_hi Numeric between 0 and 1 specifying the
#' threshold above/below which richness is excessively above or below 'normal'
#' and should be filtered.
#' @param ... Passed to `rstanarm::stan_glm` (e.g. chains, iter).
#'
#' @return List of model outputs.
#' @export
#'
#' @examples
make_effort_mod_cat <- function(df
                                , context = "cell"
                                , cat_cols
                                , threshold_lo = 0.05/2
                                , threshold_hi = 0.05/2
                                , ...
                                ) {

  effort_mod <- list()

  y <- "sr"

  effort_mod$dat_exp <- df %>%
    dplyr::distinct(taxa,dplyr::across(tidyselect::any_of(context))) %>%
    dplyr::count(dplyr::across(tidyselect::all_of(context)),name = "sr") %>%
    dplyr::select(!!rlang::ensym(y),everything())


  #--------model-------

  effort_mod$mod <- rstanarm::stan_glm(data = effort_mod$dat_exp

                                       , formula = stats::as.formula(paste0(y
                                                                            , " ~ "
                                                                            , paste0(cat_cols, collapse = "+")
                                                                            )
                                                                     )

                                       # Negative binomial
                                       , family = rstanarm::neg_binomial_2()

                                       # Options
                                       , ...
                                       )

  effort_mod$mod_pred <- effort_mod$dat_exp %>%
    dplyr::distinct(dplyr::across(tidyselect::any_of(cat_cols))) %>%
    dplyr::mutate(col = row.names(.)) %>%
    dplyr::left_join(as_tibble(rstanarm::posterior_predict(effort_mod$mod
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
    dplyr::group_by(dplyr::across(tidyselect::any_of(cat_cols))) %>%
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
    dplyr::mutate_if(is.numeric,round,2)


  #--------explore---------

  gg_x <- cat_cols[1]
  gg_fac <- cat_cols[2]

  effort_mod$mod_plot <- ggplot2::ggplot(effort_mod$mod_pred %>%
                                           dplyr::group_by(dplyr::across(tidyselect::any_of(cat_cols))) %>%
                                           dplyr::filter(!!rlang::ensym(y) < quantile(!!rlang::ensym(y), probs = 0.975))
                                         ,aes(!!rlang::ensym(y)
                                              , !!rlang::ensym(gg_x)
                                              )
                                         ) +
    ggridges::geom_density_ridges() +
    ggplot2::facet_wrap(eval(expr(~!!rlang::ensym(gg_fac)))
                        , scales = "free"
                        ) +
    ggplot2::theme(axis.text.x = element_text(angle = 90
                                              , vjust = 0.5
                                              , hjust=1
                                              )
                   ) +
    ggplot2::scale_colour_viridis_d()


  #-------cell results-------

  effort_mod$mod_cell_result <- effort_mod$dat_exp %>%
    dplyr::inner_join(effort_mod$mod_res) %>%
    dplyr::mutate(colour = "black"
                  , keep_hi = sr < extreme_sr_hi
                  , keep_lo = sr > extreme_sr_lo
                  , keep = as.logical(keep_hi*keep_lo)
                  , colour = if_else(sr >= extreme_sr_hi, "yellow", colour)
                  , colour = if_else(sr <= extreme_sr_lo, "blue", colour)
                  )

  max_y <- max(effort_mod$mod_cell_result$sr[effort_mod$mod_cell_result$keep_hi == TRUE])

  effort_mod$mod_cell_plot <- ggplot2::ggplot(effort_mod$mod_cell_result
                                              ,aes(!!rlang::ensym(gg_x)
                                                   , !!rlang::ensym(y)
                                                   , colour = colour
                                                   )
                                              ) +
    ggplot2::geom_jitter(alpha = 0.5
                         , shape = "."
                         ) +
    ggplot2::facet_wrap(eval(expr(~!!rlang::ensym(gg_fac)))) +
    ggplot2::coord_cartesian(y = c(0,max_y)) +
    ggplot2::scale_colour_identity() +
    #ggplot2::theme_dark() +
    ggplot2::theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

  effort_mod$mod_cell_tab <- effort_mod$mod_cell_result %>%
    dplyr::count(keep_hi,keep_lo,keep)

  invisible(effort_mod)

}
