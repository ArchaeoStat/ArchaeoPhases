## Coerce to events
eve <- as_events(mcmc_events, calendar = CE(), iteration = 1)

## Plot first event
plot(eve[, 1], interval = "hdr")

## Colorfull plot
plot(eve, col.density = c("#4477AA", "#EE6677", "#228833", "#CCBB44"))

## Plot events
plot(eve, calendar = CE(), interval = "credible", level = 0.68)
plot(eve, calendar = BP(), interval = "hdr", level = 0.68)

## Plot only 95% credible interval
plot(eve, density = FALSE, interval = "credible", lwd = 3, tcl = 0)
