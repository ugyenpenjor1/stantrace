#' Load or reconstruct a CmdStanR fit object from saved files
#'
#' Loads a CmdStanR fit object from an `.rds` file if available;
#' otherwise attempts to reconstruct the fit from CSV output files.
#'
#' @param rds_path Character scalar. Path to the saved `.rds` file containing the fit object.
#' @param csv_dir Character scalar. Path to the directory containing CmdStanR CSV output files.
#' @return A CmdStanR fit object (`CmdStanMCMC`, `CmdStanGQ`, or `CmdStanVB`).
#' @export

#load_cmdstanr_fit <- function(rds_path, csv_dir) {
 # if (!is.character(rds_path) || length(rds_path) != 1) {
 #   stop("'rds_path' must be a single character string.")
 # }
 # if (!is.character(csv_dir) || length(csv_dir) != 1) {
 #   stop("'csv_dir' must be a single character string.")
 # }
 # if (!requireNamespace("cmdstanr", quietly = TRUE)) {
 #   stop("The 'cmdstanr' package is required but not installed.")
 # }

 # if (file.exists(rds_path)) {
 #   fit <- readRDS(rds_path)
 #   message("Fit object loaded from RDS.")
 # } else if (dir.exists(csv_dir)) {
 #   csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
 #   if (length(csv_files) == 0) {
 #     stop("CSV directory exists but contains no CSV files.")
 #   }
 #   fit <- cmdstanr::read_cmdstan_csv(csv_files)
 #   message("Fit object reconstructed from CSVs.")
 # } else {
 #   stop("Neither RDS file nor CSV directory found.")
 # }
 # return(fit)
#}

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
    
    # --- CRITICAL CHECK: Validate/Fix Paths ---
    # Check if the first CSV file actually exists where the object thinks it is
    current_files <- fit$output_files()
    if (!file.exists(current_files[1])) {
      message("Original CSV paths not found. Re-linking to: ", csv_dir)
      
      # Locate the CSVs in the new directory
      new_csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
      
      if (length(new_csv_files) == 0) {
        warning("Fit object loaded, but no CSVs found in 'csv_dir' to re-link.")
      } else {
        # This re-binds the CSVs to the object in the current session
        fit$reinit_path(new_csv_files)
      }
    }
    # ------------------------------------------
    
  } else if (dir.exists(csv_dir)) {
    csv_files <- list.files(csv_dir, pattern = "\\.csv$", full.names = TRUE)
    if (length(csv_files) == 0) {
      stop("CSV directory exists but contains no CSV files.")
    }
    # Reconstruct from scratch (Note: This returns a list/CmdStanMCMC object 
    # but lacks some of the metadata of the original fit object)
    fit <- cmdstanr::read_cmdstan_csv(csv_files)
    message("Fit object reconstructed from CSVs.")
  } else {
    stop("Neither RDS file nor CSV directory found.")
  }
  
  return(fit)
}

#fit3 <- load_cmdstanr_fit(rds_path = "fit_model.rds", csv_dir = "output_csv_files")
