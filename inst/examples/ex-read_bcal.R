if (requireNamespace("ArchaeoData", quietly = TRUE)) {
  ## Import BCal Output
  path_output <- system.file("bcal/output/rawmcmc.csv", package = "ArchaeoData")
  (bcal <- read_bcal(path_output))
}
