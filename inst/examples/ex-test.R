## Coerce to MCMC
eve <- as_events(events, iteration = 1)
eve <- eve[1:10000, ]

## Test for anteriority
older(eve)

## Test for hiatus
lapse(eve)
