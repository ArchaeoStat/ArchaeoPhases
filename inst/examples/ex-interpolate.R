## Coerce to events
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

## Interpolate between two events
inter <- interpolate(eve, e1 = 2, e2 = 3)
plot(inter, level = 0.95, interval = "credible")
