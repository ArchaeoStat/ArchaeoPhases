## Coerce to MCMC
eve <- as_events(events, calendar = "CE", iteration = 1)
eve <- eve[1:10000, ]

## Tempo plot
tmp <- tempo(eve)
plot(tmp, credible = TRUE, gauss = FALSE)
plot(tmp, credible = FALSE, gauss = TRUE)

## Activity plot
act <- activity(tmp)
plot(act)
