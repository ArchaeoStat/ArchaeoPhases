## Coerce to MCMC
events <- as_mcmc(Events, iteration = 1)
events <- events[1:10000, ]

## Compute min-max range for all chains
all <- phase(events)
all

## Compute min-max range by group
## Unless otherwise specified, the phases are assumed to be unordered
phases <- phase(events, groups = list(A = c(1, 3), B = c(2, 4)))
phases

## Set chronological order
## (from the older to the youngest phase)
set_order(phases) <- c("B", "A")
get_order(phases)

## Compute phase ranges
boundaries(phases)

## Compute phase transition
transition(phases)

## Compute phase hiatus
hiatus(phases)

## Compute phase duration
d <- duration(phases)
summary(d)
