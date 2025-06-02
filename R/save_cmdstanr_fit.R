
#' Save cmdstanr fit object and its output CSV files
#'
#' Saves a cmdstanr fit object (`CmdStanMCMC`, `CmdStanGQ`, or `CmdStanVB`) and copies
#' its underlying Stan CSV output files to a user-specified directory.
#'
#' This is useful for archiving or transferring fit results while retaining the original
#' CmdStan output. The function includes validation to ensure correct usage.
#'
#' @param fit A CmdStanR fit object (`CmdStanMCMC`, `CmdStanGQ`, or `CmdStanVB`).
#' which all inherit from "CmdStanFit". Must implement the `output_files()` method.
#' @param rds_path A character string specifying the path to save the RDS file.
#' If missing, a temporary file is used and a message will inform the user.
#' @param csv_dir A character string specifying the directory where the CSV output
#' files should be copied. The directory will be created if it doesn't exist.
#' #' @importFrom tidyr some_function
#'
#' @return No return value. Called for its side effects: saves an RDS file and copies CSV files.
#' @export
#'
#' @examples
#' \dontrun{
#' fit <- model$sample(...)  # CmdStanR fit object
#' save_cmdstanr_fit(fit, "fit_model.rds", "output/csv_files")
#' }
save_cmdstanr_fit <- function(fit, rds_path, csv_dir) {
  # Check if fit is a cmdstanr object
  if (!inherits(fit, c("CmdStanMCMC", "CmdStanGQ", "CmdStanVB"))) {
    stop("The 'fit' object must be of class CmdStanMCMC, CmdStanGQ, or CmdStanVB from the cmdstanr package.")
  }

  # Get output CSV file paths
  csv_files <- tryCatch(
    fit$output_files(),
    error = function(e) stop("Failed to retrieve output files from 'fit': ", e$message)
  )

  # Validate output files
  if (length(csv_files) == 0 || any(!file.exists(csv_files))) {
    stop("No valid CSV files found in fit$output_files().")
  }

  # Create directory for CSVs if it doesn't exist
  if (!dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
    message("Created directory: ", csv_dir)
  }

  # Copy CSV files
  file.copy(csv_files, to = csv_dir, overwrite = TRUE)

  # If rds_path is missing, create a temporary one
  if (missing(rds_path)) {
    rds_path <- tempfile(fileext = ".rds")
    message("No rds_path provided. Saving fit object to temporary file: ", rds_path)
  }

  # Save the fit object
  saveRDS(fit, file = rds_path)

  message("CmdStanR fit object and CSV files saved successfully.")
}
