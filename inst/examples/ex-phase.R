## Coerce to MCMC objects
## Events
(eve <- as_events(events[1:10000, ], iteration = 1))
## Phases
(pha <- as_phases(phases[1:10000, ], start = c(1, 3), iteration = 1))

## Compute phases from events/dates
## Compute min-max range for all chains
pha1 <- as_phases(eve)

## Compute min-max range by group
## Unless otherwise specified, the phases are assumed to be unordered
pha2 <- as_phases(eve, groups = list(A = c(1, 3), B = c(2, 4)))

all(pha == pha2)

## Set chronological order
## (from the oldest to the youngest phase)
set_order(pha2) <- c("B", "A")
get_order(pha2)
is_ordered(pha2)
