\dontrun{
## Import OxCal Output
path_output <- system.file("oxcal/ksarakil/MCMC_Sample.csv", package = "fasti")
url_output <- paste0("https://raw.githubusercontent.com/tesselle/fasti/master/",
                     "inst/oxcal/ksarakil/MCMC_Sample.csv")
oxcal <- read_oxcal(path_output)

## Check md5 sum
is_original(oxcal, path_output) # Same as local file? TRUE
is_original(oxcal, url_output, download = FALSE) # Same as remote file? FALSE
is_original(oxcal, url_output, download = TRUE) # Same as remote file? TRUE
}
