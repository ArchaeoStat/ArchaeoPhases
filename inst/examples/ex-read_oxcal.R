if (requireNamespace("ArchaeoData", quietly = TRUE)) {
  ## Import OxCal Output
  path <- "oxcal/ksarakil/"

  path_output <- system.file(path, "MCMC_Sample.csv", package = "ArchaeoData")
  (oxcal <- read_oxcal(path_output))
}
