## Coerce to MCMC objects
## Events
(eve <- as_events(events[1:10000, ], calendar = "CE", iteration = 1))

## Phases
(pha <- as_phases(phases[1:10000, ], start = c(1, 3), calendar = "CE",
                  iteration = 1))

## Compute phases from events/dates
## Compute min-max range for all chains
pha1 <- phase(eve)

## Compute min-max range by group
## Unless otherwise specified, the phases are assumed to be unordered
pha2 <- phase(eve, groups = list(phase_1 = c(1, 3), phase_2 = c(2, 4)))

all(pha == pha2) # TRUE
