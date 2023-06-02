if (requireNamespace("ArchaeoData", quietly = TRUE)) {
  ## Import BCal Output
  path_output <- system.file("bcal/fishpond.csv", package = "ArchaeoData")
  (bcal <- read_bcal(path_output))
}
