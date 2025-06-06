% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_cmdstanr_fit.R
\name{save_cmdstanr_fit}
\alias{save_cmdstanr_fit}
\title{Save cmdstanr fit object and its output CSV files}
\usage{
save_cmdstanr_fit(fit, rds_path, csv_dir)
}
\arguments{
\item{fit}{A CmdStanR fit object (\code{CmdStanMCMC}, \code{CmdStanGQ}, or \code{CmdStanVB}).
which all inherit from "CmdStanFit". Must implement the \code{output_files()} method.}

\item{rds_path}{A character string specifying the path to save the RDS file.
If missing, a temporary file is used and a message will inform the user.}

\item{csv_dir}{A character string specifying the directory where the CSV output
files should be copied. The directory will be created if it doesn't exist.
#' @importFrom tidyr some_function}
}
\value{
No return value. Called for its side effects: saves an RDS file and copies CSV files.
}
\description{
Saves a cmdstanr fit object (\code{CmdStanMCMC}, \code{CmdStanGQ}, or \code{CmdStanVB}) and copies
its underlying Stan CSV output files to a user-specified directory.
}
\details{
This is useful for archiving or transferring fit results while retaining the original
CmdStan output. The function includes validation to ensure correct usage.
}
\examples{
\dontrun{
fit <- model$sample(...)  # CmdStanR fit object
save_cmdstanr_fit(fit, "fit_model.rds", "output/csv_files")
}
}
