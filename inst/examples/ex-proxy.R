\dontrun{
## Get NGRIP records
data("ngrip2010", package = "folio")
ngrip2010 <- subset(ngrip2010, !is.na(MCE))

## Plot NGRIP record
ggplot2::ggplot(data = ngrip2010) +
  ggplot2::aes(x = age, y = delta) +
  ggplot2::geom_path() +
  ggplot2::scale_x_reverse(name = "Year b2k") +
  ggplot2::coord_cartesian(xlim = c(20000, 10000)) +
  ggplot2::theme_bw()

## Replicate fig. 3d from Boers et al. (2017)
ngrip_error <- proxy(
  depth = ngrip2010$depth,
  proxy = ngrip2010$delta,
  proxy_error = 0.01,
  time = ngrip2010$age,
  time_error = ngrip2010$MCE,
  from = 10001, to = 20000, resolution = 20,
  grid = 0.01,
  calendar = "b2k",
  density = TRUE
)

plot(ngrip_error, IQR = FALSE)
}
