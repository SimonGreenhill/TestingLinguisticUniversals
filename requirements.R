.libPaths(c("rlib/", .libPaths()))

#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)

if (!suppressPackageStartupMessages(require("pacman"))) {
  install.packages("pacman")
}

pacman::p_load(
  ape,
#  bayestraitr,
  brms,
  ggplot2,
  ggpubr,
  ggridges,
  ggrepel,
  ggdist,
  patchwork,
  gridExtra,
  stringr,
  tidyr,
  patchwork,
  data.table, #faster reading in of files
  tidyverse,
  devtools
)

#the package bayestraitr needs to be installed via GitHub. Specific commit ref is given for version control
if(! "bayestraitr" %in% rownames(installed.packages())){
  devtools::install_github(repo = "SamPassmore/bayestraitr",
                           ref = "5a84d6a946e23b0e78d7b12b53299f514fcca0a7")
}
library(bayestraitr)

dir <-"output"
if(!dir.exists(dir)){
  dir.create(dir)
}

dir <-"output/plots/"
if(!dir.exists(dir)){
  dir.create(dir)
}



dir <-"output/proccessed_data/"
if(!dir.exists(dir)){
  dir.create(dir)
}
