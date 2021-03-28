## Coerce to MCMC
eve <- as_mcmc(events, iteration = 1)
eve <- eve[1:10000, ]

## Credible interval
interval_credible(eve, level = 0.95)

## HPD regions
interval_hpd(eve, level = 0.95)
