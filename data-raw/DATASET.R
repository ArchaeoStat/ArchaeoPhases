## Events
events <- read.table(file = "data-raw/events.csv", header = TRUE,
                     sep = ",", dec = ".", row.names = NULL)
usethis::use_data(events, overwrite = TRUE)

## Phases
phases <- read.table(file="data-raw/phases.csv", header = TRUE,
                     sep = ",", dec = ".", row.names = NULL)
usethis::use_data(phases, overwrite = TRUE)
