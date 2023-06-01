# Vignettes that depend on internet access have been precompiled
old_wd <- setwd("./vignettes")
knitr::knit("ArchaeoPhases.Rmd.orig", output = "ArchaeoPhases.Rmd")
knitr::knit("import.Rmd.orig", output = "import.Rmd")
knitr::knit("allen.Rmd.orig", output = "allen.Rmd")
setwd(old_wd)
