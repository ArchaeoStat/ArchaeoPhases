\dontrun{
## Get NGRIP records
data("ngrip2010", package = "folio")
ngrip2010 <- subset(ngrip2010, !is.na(MCE))

## Replicate fig. 3d from Boers et al. (2017)
ngrip_error <- proxy(
  depth = ngrip2010$depth,
  proxy = ngrip2010$delta,
  proxy_error = 0.01,
  time = ngrip2010$age,
  time_error = ngrip2010$MCE,
  grid = 0.01,
  calendar = "b2k"
)

plot(ngrip_error)
}
