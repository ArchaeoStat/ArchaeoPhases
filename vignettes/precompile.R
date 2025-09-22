# Vignettes that depend on internet access have been precompiled
if (!requireNamespace("ArchaeoData", quietly = TRUE)) {
  install.packages("ArchaeoData", repos = "https://archaeostat.r-universe.dev")
}

old_wd <- setwd("./vignettes")
knitr::knit("ArchaeoPhases.Rmd.orig", output = "ArchaeoPhases.Rmd")
knitr::knit("import.Rmd.orig", output = "import.Rmd")
knitr::knit("allen.Rmd.orig", output = "allen.Rmd")
setwd(old_wd)
