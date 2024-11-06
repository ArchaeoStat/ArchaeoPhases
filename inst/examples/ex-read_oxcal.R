if (requireNamespace("ArchaeoData", quietly = TRUE)) {
  ## Construct the path to the data
  path <- file.path("oxcal", "ksarakil")
  path_output <- system.file(path, "MCMC_Sample.csv", package = "ArchaeoData")

  ## Import OxCal Output
  (oxcal <- read_oxcal(path_output))
}
