## Coerce to MCMC
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

## Occurrence plot
occ <- occurrence(eve)
plot(occ)
