\dontrun{
## Import BCal Output
path_output <- system.file("bcal/output/rawmcmc.csv", package = "fasti")
(bcal <- read_bcal(path_output))
}