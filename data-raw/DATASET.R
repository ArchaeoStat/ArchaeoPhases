## Events
mcmc_events <- read.table(file = "data-raw/events.csv", header = TRUE,
                          sep = ",", dec = ".", row.names = NULL)
usethis::use_data(mcmc_events, overwrite = TRUE)

## Phases
mcmc_phases <- read.table(file="data-raw/phases.csv", header = TRUE,
                          sep = ",", dec = ".", row.names = NULL)
usethis::use_data(mcmc_phases, overwrite = TRUE)
