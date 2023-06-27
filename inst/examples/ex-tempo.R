## Coerce to MCMC
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)
eve <- eve[1:10000, ]

## Tempo plot
tmp <- tempo(eve)
plot(tmp)
plot(tmp, interval = "credible", panel.first = grid())
plot(tmp, interval = "gauss", panel.first = grid())

## Activity plot
act <- activity(tmp)
plot(act, panel.first = grid())
