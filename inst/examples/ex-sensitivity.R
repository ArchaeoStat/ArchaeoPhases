## Coerce to MCMC
eve <- as_events(events, iteration = 1)

## Returns 0's
sensitivity(eve, eve)
