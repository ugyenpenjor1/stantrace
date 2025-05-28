#' Plot Posterior Predictive Check with Bayesian p-value
#'
#' @param fit A CmdStanR fit object.
#' @param obs_var Name of the observed test statistic variable (string).
#' @param sim_var Name of the simulated test statistic variable (string).
#'
#' @return A ggplot2 object showing the PPC scatter plot.
#' @export
plot_ppc <- function(fit, obs_var = "T_obs_sum", sim_var = "T_sim_sum") {
  # Load draws
  draws_df <- fit$draws(variables = c(obs_var, sim_var), format = "data.frame")
  
  # Extract vectors
  fit_vals <- draws_df[[obs_var]]
  fit_rep_vals <- draws_df[[sim_var]]
  
  # Create data frame for plotting
  ppc_df <- data.frame(
    fit = fit_vals,
    fit_rep = fit_rep_vals,
    colour = ifelse(fit_rep_vals > fit_vals, "salmon", "steelblue")
  )
  
  # Axis limits
  lims <- range(c(fit_vals, fit_rep_vals))
  
  # Compute Bayesian p-value
  bpv <- mean(fit_rep_vals > fit_vals)
  bpv_label <- paste0("italic(P) == ", round(bpv, 2))
  
  # Generate plot
  ggplot2::ggplot(ppc_df, ggplot2::aes(x = fit, y = fit_rep)) +
    ggplot2::geom_point(shape = 21, size = 2, fill = ppc_df$colour, color = 'black') +
    ggplot2::geom_rug(linewidth = 0.5, length = grid::unit(0.015, "npc")) +
    ggplot2::xlim(lims) +
    ggplot2::ylim(lims) +
    ggplot2::geom_abline(slope = 1, intercept = 0) +
    ggplot2::xlab("Observed test statistic (Tobs)") +
    ggplot2::ylab("Simulated test statistic (Tsim)") +
    ggplot2::annotate(
      "text",
      x = lims[1] + 0.6 * diff(lims),
      y = lims[2] - 0.1 * diff(lims),
      label = bpv_label,
      parse = TRUE,
      size = 5
    ) +
    ggplot2::theme_bw(base_size = 16) +
    ggplot2::theme(
      legend.position = "none",
      plot.margin = grid::unit(c(0.15, 0.15, 0.15, 0.4), "inches")
    )
}
