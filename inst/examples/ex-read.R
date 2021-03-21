\dontrun{
## Import ChronoModel Output
path_zip <- system.file("extdata/chronomodel.zip", package = "ArchaeoPhases")
path_csv <- utils::unzip(path_zip, exdir = tempdir())
events <- read_chronomodel(path_csv[[1]], phases = FALSE)
phases <- read_chronomodel(path_csv[[2]], phases = TRUE)

## Import OxCal Output
path_zip <- system.file("extdata/oxcal.zip", package = "ArchaeoPhases")
path_csv <- utils::unzip(path_zip, exdir = tempdir())
oxcal <- read_chronomodel(path_csv)

is_original(oxcal, path_csv)
}
