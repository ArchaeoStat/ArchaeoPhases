\donttest{
## Coerce to MCMC
eve <- as_mcmc(events, iteration = 1)
eve <- eve[1:10000, ]

## Tempo plot
## BP
eve_BP <- CE_to_BP(eve)
tempo_BP <- tempo(eve_BP)
plot(tempo_BP)

## CE
eve_CE <- BP_to_CE(eve_BP)
tempo_CE <- tempo(eve_CE, count = FALSE, gauss = TRUE)
plot(tempo_CE)

## Activity plot
act <- activity(eve_CE)
plot(act)

## Rate of change
chg <- roc(eve_CE, step = 50)
plot(chg)
}
