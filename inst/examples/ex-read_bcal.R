if (requireNamespace("ArchaeoData", quietly = TRUE)) {
  ## Construct the path to the data
  path_output <- system.file("bcal", "fishpond.csv", package = "ArchaeoData")

  ## Import BCal Output
  (bcal <- read_bcal(path_output))
}
