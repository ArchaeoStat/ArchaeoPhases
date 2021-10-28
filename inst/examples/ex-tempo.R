## Coerce to MCMC
eve <- as_mcmc(events, iteration = 1)
eve <- eve[1:10000, ]

## Tempo plot
tmp <- tempo(eve, count = FALSE)
plot(tmp)

## Activity plot
act <- activity(tmp)
plot(act)

## Rate of change
chg <- roc(act)
plot(chg)
