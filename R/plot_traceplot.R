#' Plot Traceplots for MCMC Draws Array
#'
#' @param draws_array A 3D array of MCMC samples (iterations x chains x parameters).
#' @param parameters Optional vector of parameter names to include in the plot.
#' @param ncol Number of columns in facet layout (default is 3).
#'
#' @return A ggplot2 object showing traceplots.
#' @export
plot_traceplot <- function(draws_array, parameters = NULL, ncol = 3) {
  library(ggplot2)
  library(dplyr)
  library(tidyr)

  draws_long <- as.data.frame.table(draws_array, responseName = "value") %>%
    rename(iteration = 1, chain = 2, variable = 3) %>%
    mutate(
      iteration = as.integer(iteration),
      chain = factor(chain),
      variable = factor(variable)
    )

  if (!is.null(parameters)) {
    draws_long <- draws_long %>%
      filter(variable %in% parameters)
  }

  ggplot(draws_long, aes(x = iteration, y = value, color = chain)) +
    geom_line(alpha = 0.6) +
    scale_colour_manual(
      values = c("chocolate3", "#0072B2", "darkorchid4", "#009E73")
    ) +
    facet_wrap(~ variable, scales = "free_y", ncol = ncol) +
    labs(
      title = "Traceplots of MCMC Samples",
      x = "Iteration",
      y = "Value",
      color = "Chain"
    ) +
    theme_minimal()
}


# Document the function
devtools::document()
# This creates the help files under man/ and updates NAMESPACE.

# Install the package locally
devtools::install()

