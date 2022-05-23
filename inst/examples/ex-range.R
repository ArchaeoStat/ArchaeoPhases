## Coerce to MCMC
eve <- as_events(events, calendar = "CE", iteration = 1)
eve <- eve[1:10000, ]

## Compute min-max range by group
## Unless otherwise specified, the phases are assumed to be unordered
pha <- phase(eve, groups = list(A = c(1, 3), B = c(2, 4)))

## Compute phase ranges
boundaries(pha)

## Compute phase transition
tra <- transition(pha)
as.data.frame(tra)

## Compute phase hiatus
hia <- hiatus(pha)
as.data.frame(hia)

## Compute phase duration
dur <- duration(pha)
summary(dur)
