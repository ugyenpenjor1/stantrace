#' Plot Traceplots for MCMC Draws Array
#'
#' @param draws A 3D array of MCMC samples (iterations x chains x parameters).
#' @param parameters Optional vector of parameter names to include in the plot.
#' @param ncol Number of columns in facet layout (default is 3).
#'
#' @return A ggplot2 object showing traceplots.
#' @export

plot_traceplot <- function(draws, parameters = NULL, ncol = 3) {
  draws_long <- as.data.frame.table(draws, responseName = "value") |>
    dplyr::rename(iteration = 1, chain = 2, variable = 3) |>
    dplyr::mutate(
      iteration = as.integer(iteration),
      chain = factor(chain),
      variable = factor(variable)
    )

  if (!is.null(parameters)) {
    draws_long <- draws_long |>
      dplyr::filter(variable %in% parameters)
  }

  ggplot2::ggplot(draws_long, ggplot2::aes(x = iteration, y = value, color = chain)) +
    ggplot2::geom_line(alpha = 0.6) +
    ggplot2::scale_colour_manual(
      values = c("chocolate3", "#0072B2", "darkorchid4", "#009E73")
    ) +
    ggplot2::facet_wrap(~ variable, scales = "free_y", ncol = ncol) +
    ggplot2::labs(
      title = "Traceplots of MCMC Samples",
      x = "Iteration",
      y = "Value",
      color = "Chain"
    ) +
    ggplot2::theme_minimal()
}
