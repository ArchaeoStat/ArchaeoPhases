# Vignettes that depend on internet access have been precompiled
old_wd <- setwd("./vignettes")
knitr::knit("chronos.Rmd.orig", output = "chronos.Rmd")
knitr::knit("import.Rmd.orig", output = "import.Rmd")
setwd(old_wd)
