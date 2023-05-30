## Coerce to MCMC
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

## Returns 0's
sensitivity(eve, eve)
