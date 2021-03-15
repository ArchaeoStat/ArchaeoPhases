## Coerce to MCMC
events <- as_mcmc(Events, iteration = 1)
events <- events[1:10000, ]

## Credible interval
interval_credible(events, level = 0.95)

## HPD regions
interval_hpd(events, level = 0.95)
