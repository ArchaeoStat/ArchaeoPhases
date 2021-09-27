## Coerce to MCMC
eve <- as_mcmc(events, iteration = 1)
eve <- eve[1:10000, ]

## Tempo plot
## BP
eve_BP <- CE_to_BP(eve)
tmp <- tempo(eve_BP)
plot(tmp)

## CE
eve_CE <- BP_to_CE(eve_BP)
tmp <- tempo(eve_CE, count = FALSE, gauss = TRUE)
plot(tmp)

## Activity plot
act <- activity(tmp)
plot(act)

## Rate of change
chg <- roc(act)
plot(chg)
