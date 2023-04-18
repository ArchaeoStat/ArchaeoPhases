## Coerce to MCMC
eve <- as_events(events, calendar = "CE", iteration = 1)
eve <- eve[1:10000, ]

## Tempo plot
tmp <- tempo(eve)
plot(tmp)
plot(tmp, credible = TRUE, gauss = FALSE, panel.first = grid())
plot(tmp, credible = FALSE, gauss = TRUE, panel.first = grid())

## Activity plot
act <- activity(tmp)
plot(act, panel.first = grid())
