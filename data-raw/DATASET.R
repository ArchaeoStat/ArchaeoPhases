# External data ================================================================
## Events
mcmc_events <- read.table(file = "data-raw/events.csv", header = TRUE,
                          sep = ",", dec = ".", row.names = NULL)
usethis::use_data(mcmc_events, overwrite = TRUE)

## Phases
mcmc_phases <- read.table(file="data-raw/phases.csv", header = TRUE,
                          sep = ",", dec = ".", row.names = NULL)
usethis::use_data(mcmc_phases, overwrite = TRUE)

# Internal data ================================================================
## Allen codes
.allen_code <- c("<", "m", "o", "fi", "di", "s", "=", "si", "d", "f", "oi", "mi", ">")
.allen_converse <- c(">", "mi", "oi", "f", "d", "si", "=", "s", "di", "fi", "o", "m", "<")
.allen_string <- c(
  `<` = "precedes",     `m` = "meets",          `o` = "overlaps",
  `fi` = "finished by", `di` = "contains",      `s` = "starts",
  `=` = "equals",       `si` = "started by",    `d` = "during",
  `f` = "finishes",     `oi` = "overlapped by", `mi` = "met by",
  `>` = "preceded by"
)

## Alspaugh codes
.alspaugh_code <- c("p", "m", "o", "F", "D", "s", "e", "S", "d", "f", "O", "M", "P")
.alspaugh_converse <- c("P", "M", "O", "f", "d", "S", "e", "s", "D", "F", "o", "m", "p")
.alspaugh_string <- c(
  `p` = "precedes",    `m` = "meets",         `o` = "overlaps",
  `F` = "finished by", `D` = "contains",      `s` = "starts",
  `e` = "equals",      `S` = "started by",    `d` = "during",
  `f` = "finishes",    `O` = "overlapped by", `M` = "met by",
  `P` = "preceded by"
)

## Table of composition of basic Allen interval relations
.composition <- read.table("data-raw/allen.csv", header = TRUE, sep = ",", row.names = 1)

usethis::use_data(
  .allen_code, .allen_string, .allen_converse, .composition,
  .alspaugh_code, .alspaugh_converse, .alspaugh_string,
  internal = TRUE,
  overwrite = TRUE
)

