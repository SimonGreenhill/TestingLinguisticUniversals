.libPaths(c("rlib/", .libPaths()))

#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)

if (!suppressPackageStartupMessages(require("pacman"))) {
  install.packages("pacman")
}

pacman::p_load(
  ape,
  bayestraitr,
  brms,
  ggplot2,
  ggpubr,
  stringr,
  tidyr,
  tidyverse
)

