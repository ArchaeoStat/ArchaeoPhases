\donttest{
## Coerce to MCMC
events <- as_mcmc(Events, iteration = 1)
events <- events[1:10000, ]

## Tempo plot
tempo1 <- tempo(events)
plot(tempo1)

tempo2 <- tempo(events, gauss = TRUE)
plot(tempo2, calendar = "BP")
}
