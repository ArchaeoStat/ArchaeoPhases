## Coerce to MCMC
eve <- as_mcmc(events, iteration = 1)

## Returns 0's
sensitivity(eve, eve)
