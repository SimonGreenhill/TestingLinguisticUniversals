#!/usr/bin/Rscript

wd <- getwd()

setwd("../../")
source("requirements_for_uncontrolled_brms.R")

setwd(wd)

addTaskCallback(function(...) {
  set.seed(123)
  TRUE
})

glottolog_langs <- read.csv("Glottolog_Languages.csv.gz")
datfra <- read.table(file = "BT_data.txt")
tree <- read.nexus("pruned_tree.tree")

datfra$ID <- datfra$V1
datfra$ID2 <- datfra$ID
datfra$Longitude <-
  glottolog_langs$longitude[match(datfra$ID, glottolog_langs$glottocode)]
datfra$Latitude <-
  glottolog_langs$latitude[match(datfra$ID, glottolog_langs$glottocode)]
datfra$macroarea <-
  glottolog_langs$macroarea[match(datfra$ID, glottolog_langs$glottocode)]

datfra <- datfra[!is.na(datfra$Longitude), ]
datfra <- datfra[!is.na(datfra$Latitude), ]

#prune tree

droptips <- setdiff(tree$tip.label, datfra$V1)
tree_pruned <- drop.tip(tree, droptips)

#cheching for duplicated locations and jittering them
#otherwise the distance matrix cannot be created
duplicate_coords <-
  datfra[duplicated(datfra[, c("Longitude", "Latitude")]) |
           duplicated(datfra[, c("Longitude", "Latitude")], fromLast = TRUE), "ID"]
duplicate_rowid <- datfra$ID %in% duplicate_coords
datfra$Latitude[duplicate_rowid] <-
  jitter(datfra$Latitude[duplicate_rowid], factor = 1)
datfra$Longitude[duplicate_rowid] <-
  jitter(datfra$Longitude[duplicate_rowid], factor = 1)

prior <- c(set_prior("student_t(3, 0, 2.5)", class = "b"))
# http://srmart.in/is-the-lkj1-prior-uniform-yes/

# https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet
mod <- brm(
  formula = V2 ~ V3,
  data = list(datfra),
  prior = prior,
  family = "bernoulli",
  control = list(adapt_delta = 0.99),
  iter = 3000,
  save_pars = save_pars(all = TRUE),
  cores = 4
)
summary(mod)

# storing results

# convergence
# https://www.rensvandeschoot.com/tutorials/wambs-checklist-in-r-using-brms/
#modelposterior <- as.mcmc(mod) # with the as.mcmc() command we can use all the CODA package convergence statistics and plotting options
#gelman.diag(modelposterior[, 1:5])
#gelman.diag(modelposterior[, 1:5])$mpsrf # should be close to 1

sink("summary_uncontrolled.txt")
print(summary(mod))
#print("   ###    ")
#gelman.diag(modelposterior[, 1:5])$mpsrf
sink()

sum_clean <- as.data.frame(summary(mod)$fixed)

row.names(sum_clean) <- c("fixed_intercept", "fixed_V3")

sink("summary_clean_uncontrolled.txt")
print(sum_clean)
sink()

# all in all

save(mod, file = "mod_uncontrolled.RData")
