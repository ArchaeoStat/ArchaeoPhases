\donttest{
## Coerce to MCMC
eve <- as_mcmc(events, iteration = 1)
eve <- eve[1:10000, ]

## Tempo plot
tempo1 <- tempo(eve)
plot(tempo1)

tempo2 <- tempo(eve, gauss = TRUE)
plot(tempo2, calendar = "BP")

## Activity plot
act <- activity(eve)
plot(act)
}
