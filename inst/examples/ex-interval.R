## Coerce to events
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

## Rata die
interval_credible(eve, level = 0.95) # Credible interval
interval_hdr(eve, level = 0.68) # HPD interval

## BP
interval_credible(eve, level = 0.95, calendar = BP()) # Credible interval
interval_hdr(eve, level = 0.95, calendar = BP()) # HPD interval
