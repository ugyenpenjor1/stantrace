#' Plot traceplots for MCMC draws array
#'
#' @param draws A 3D array of MCMC samples (iterations x chains x parameters).
#' @param mcmc_obj a cmdstanr MCMC object
#' @param parameters Optional vector of parameter names to include in the plot.
#' @param ncol Number of columns in facet layout (default is 3).
#'
#' @return A ggplot2 object showing traceplots with ESS and R-hat.
#' @export

plot_traceplot2 <- function(draws, mcmc_obj, parameters = NULL, ncol = 3) {
  # Extract metadata
  meta <- mcmc_obj$metadata()
  warmup_iters <- meta$iter_warmup
  sampling_iters <- meta$iter_sampling
  chains <- meta$num_chains
  total_iters_expected <- warmup_iters + sampling_iters

  # Get number of iterations in the draw array (dim 1 = iteration)
  iterations_in_draws <- dim(draws)[1]

  # Detect whether warm-up is present
  warmup_present <- iterations_in_draws > sampling_iters

  # Warn user if warm-up is not present but expected
  if (!warmup_present && warmup_iters > 0) {
    warning(
      "Warm-up iterations are not detected in the draws. ",
      "To include warm-up in the traceplot, call `draws(inc_warmup = TRUE)`."
    )
  }

  # Convert draws to long format
  draws_long <- as.data.frame.table(draws, responseName = "value") |>
    dplyr::rename(iteration = 1, chain = 2, variable = 3) |>
    dplyr::mutate(
      iteration = as.integer(iteration),
      chain = as.integer(chain),
      variable = as.character(variable)
    )

  # Add phase if warm-up is present
  if (warmup_present) {
    draws_long <- draws_long |>
      dplyr::mutate(
        phase = ifelse(iteration <= warmup_iters, "warmup", "sampling")
      )
  }

  # Filter parameters if needed
  if (!is.null(parameters)) {
    draws_long <- dplyr::filter(draws_long, variable %in% parameters)
  }

  # Get ESS summary
  # ess_summary <- mcmc_obj$summary(variables = unique(draws_long$variable)) |>
  #   dplyr::select(variable, ess_bulk) |>
  #   dplyr::mutate(
  #     ess_label = paste0("ESS: ", round(ess_bulk)),
  #     x = Inf,
  #     y = Inf
  #   )

  ess_summary <- mcmc_obj$summary(variables = unique(draws_long$variable)) |>
    dplyr::select(variable, ess_bulk, rhat) |>
    dplyr::mutate(
      label = paste0(
        "ESS: ", round(ess_bulk),
        "\nR-hat: ", format(round(rhat, 3), nsmall = 3)
      ),
      x = Inf,
      y = Inf
    )

  # Start ggplot
  p <- ggplot2::ggplot(
    draws_long,
    ggplot2::aes(x = iteration, y = value, color = factor(chain))
  )

  # Add alpha aesthetic only if warm-up is present
  if (warmup_present) {
    p <- p +
      ggplot2::aes(alpha = phase) +
      ggplot2::scale_alpha_manual(
        values = c("warmup" = 0.25, "sampling" = 0.7),
        guide = "none"
      )
  }

  # Build full plot
  # p +
  #   ggplot2::geom_line(linewidth = 0.85) +
  p <- if (warmup_present) {
    p + ggplot2::geom_line(linewidth = 0.7)
  } else {
    p + ggplot2::geom_line(linewidth = 0.8, alpha = 0.7)
  }
  p +
    ggplot2::scale_colour_manual(
      #values = c("chocolate3", "#0072B2", "darkorchid4", "#009E73"),
      values = c("#cc99cc", "#333333", "indianred3", "#006699"),
      name = "Chain"
    ) +
    ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = ncol) +
    ggplot2::geom_text(
      data = ess_summary,
      #mapping = ggplot2::aes(x = x, y = y, label = ess_label),
      mapping = ggplot2::aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      hjust = 1.1, vjust = 1.5,
      size = 3.5, color = "black"
    ) +
    ggplot2::labs(
      #title = "Traceplots of MCMC Samples",
      x = "Iteration",
      y = "Value"
    ) +
    #ggplot2::theme_minimal() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      axis.title = ggplot2::element_text(size = 12, vjust = 0.8),
      legend.text = ggplot2::element_text(size = 12),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}
