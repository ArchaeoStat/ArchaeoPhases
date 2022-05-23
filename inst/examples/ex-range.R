## Coerce to MCMC
eve <- as_events(events, calendar = "CE", iteration = 1)
eve <- eve[1:10000, ]

## Compute min-max range by group
## Unless otherwise specified, the phases are assumed to be unordered
pha <- phase(eve, groups = list(A = c(1, 3), B = c(2, 4)))

## Set chronological order
## (from the oldest to the youngest phase)
set_order(pha) <- c("B", "A")

## Compute phase ranges
boundaries(pha)

## Compute phase transition
transition(pha)

## Compute phase hiatus
hiatus(pha)

## Compute phase duration
dur <- duration(pha)
summary(dur)
