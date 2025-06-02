#' Load or reconstruct a CmdStanR fit object from saved files
#'
#' Loads a CmdStanR fit object from an `.rds` file if available;
#' otherwise attempts to reconstruct the fit from CSV output files.
#'
#' @param rds_path Character scalar. Path to the saved `.rds` file containing the fit object.
#' @param csv_dir Character scalar. Path to the directory containing CmdStanR CSV output files.
#' @return A CmdStanR fit object (`CmdStanMCMC`, `CmdStanGQ`, or `CmdStanVB`).
#' @export

load_cmdstanr_fit <- function(rds_path, csv_dir) {
  if (!is.character(rds_path) || length(rds_path) != 1) {
    stop("'rds_path' must be a single character string.")
  }
  if (!is.character(csv_dir) || length(csv_dir) != 1) {
    stop("'csv_dir' must be a single character string.")
  }
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stop("The 'cmdstanr' package is required but not installed.")
  }

  if (file.exists(rds_path)) {
    fit <- readRDS(rds_path)
    message("Fit object loaded from RDS.")
  } else if (dir.exists(csv_dir)) {
    csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) == 0) {
      stop("CSV directory exists but contains no CSV files.")
    }
    fit <- cmdstanr::read_cmdstan_csv(csv_files)
    message("Fit object reconstructed from CSVs.")
  } else {
    stop("Neither RDS file nor CSV directory found.")
  }
  return(fit)
}
