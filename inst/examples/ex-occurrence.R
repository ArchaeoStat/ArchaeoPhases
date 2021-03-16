\donttest{
## Coerce to MCMC
events <- as_mcmc(Events, iteration = 1)
events <- events[1:10000, ]

## Occurrence plot
occ_ci <- occurrence(events, interval = "ci")
plot(occ_ci, calendar = "BCAD")

occ_hpdi <- occurrence(events, interval = "hpdi")
plot(occ_hpdi, calendar = "BP")
}
