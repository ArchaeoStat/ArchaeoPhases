## Coerce to events
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

## Elapsed origin
eve_elapse <- elapse(eve, origin = 4)
plot(eve_elapse)
