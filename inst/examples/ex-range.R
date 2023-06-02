## Coerce to events
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

## Compute min-max range by group
pha <- phases(eve, groups = list(A = c(1, 3), B = c(2, 4)))

## Compute phase ranges
bou <- boundaries(pha)
as.data.frame(bou)

## Compute phase transition
tra <- transition(pha)
as.data.frame(tra)

## Compute phase hiatus
hia <- hiatus(pha)
as.data.frame(hia)
