#!/usr/bin/Rscript

library(brms)
library(ape)
#library(coda)
source('varcov.spatial_function.R')

addTaskCallback(function(...) {set.seed(123);TRUE})

glottolog_langs <- read.csv("Glottolog_Languages.csv")
datfra <- read.table(file = "0001_or_2aKA/BT_data.txt")
tree <- read.nexus("0001_or_2aKA/pruned_tree.tree")

datfra$ID <- datfra$V1
datfra$ID2 <- datfra$ID
datfra$Longitude <- glottolog_langs$longitude[match(datfra$ID, glottolog_langs$glottocode)]
datfra$Latitude <- glottolog_langs$latitude[match(datfra$ID, glottolog_langs$glottocode)]
datfra$macroarea <- glottolog_langs$macroarea[match(datfra$ID, glottolog_langs$glottocode)]

datfra <- datfra[!is.na(datfra$Longitude),]
datfra <- datfra[!is.na(datfra$Latitude),]

#prune tree

droptips <- setdiff(tree$tip.label, datfra$V1)
tree_pruned <- drop.tip(tree, droptips)

phylo_covar_mat <- ape::vcv(tree_pruned)
phylo_covar_mat <- phylo_covar_mat / max(phylo_covar_mat)

kappa = 1 # smoothness parameter as reccomended by Dinnage et al. (2020)
phi = c(1, 1) # Sigma parameter. First value is not used.

#cheching for duplicated locations and jittering them 
#otherwise the distance matrix cannot be created
duplicate_coords <- datfra[duplicated(datfra[,c("Longitude", "Latitude")]) | duplicated(datfra[,c("Longitude", "Latitude")], fromLast = TRUE), "ID"]
duplicate_rowid <- datfra$ID %in% duplicate_coords
datfra$Latitude[duplicate_rowid] <- jitter(datfra$Latitude[duplicate_rowid], factor = 1)
datfra$Longitude[duplicate_rowid] <- jitter(datfra$Longitude[duplicate_rowid], factor = 1)

spatial_covar_mat = varcov.spatial(datfra[,c("Longitude", "Latitude")], cov.pars = phi, kappa = kappa)$varcov
dimnames(spatial_covar_mat) = list(datfra$ID2, datfra$ID2)
spatial_covar_mat <- spatial_covar_mat / max(spatial_covar_mat)

long_lat  = glottolog_langs %>% 
  filter(glottocode %in%   rownames(spatial_covar_mat)) %>% 
  column_to_rownames("glottocode") %>% 
  dplyr::select(longitude, latitude) %>% 
    as.matrix()
  
dists = fields::rdist.earth(x1 = long_lat , x2 = long_lat , miles = F)

dists = dists %>% 
  reshape2::melt() %>% 
  dplyr::rename(geo_dist = value)

spatial_covar_mat %>% 
  reshape2::melt() %>% 
  full_join(dists) %>% 
  ggplot() +
  geom_point(aes(x = geo_dist, y = value))

prior <- c(set_prior("student_t(3, 0, 2.5)", class = "b"),
           set_prior("exponential(1)", class = "sd"))
# http://srmart.in/is-the-lkj1-prior-uniform-yes/

# https://stats.stackexchange.com/questions/13166/rs-lmer-cheat-sheet
mod <- brm(formula= V2 ~ V3 + (1|gr(V1, cov = phylo_covar_mat)) + 
                          (1|gr(ID2, cov=spatial_covar_mat)) + (1 + V3 |macroarea), 
           data = list(datfra), prior=prior, family = "bernoulli", 
           control = list(adapt_delta = 0.99), iter = 3000, 
           save_pars = save_pars(all = TRUE), cores=4,  
           data2 = list(phylo_covar_mat = phylo_covar_mat, spatial_covar_mat = spatial_covar_mat))
summary(mod)

# storing results

# convergence
# https://www.rensvandeschoot.com/tutorials/wambs-checklist-in-r-using-brms/
#modelposterior <- as.mcmc(mod) # with the as.mcmc() command we can use all the CODA package convergence statistics and plotting options
#gelman.diag(modelposterior[, 1:5])
#gelman.diag(modelposterior[, 1:5])$mpsrf # should be close to 1

sink("summary.txt")
print(summary(mod))
#print("   ###    ")
#gelman.diag(modelposterior[, 1:5])$mpsrf
sink()

sum_clean <- as.data.frame(summary(mod)$fixed)
sum_clean <- rbind(sum_clean, summary(mod)$random$ID2)
sum_clean <- rbind(sum_clean, summary(mod)$random$V1)
sum_clean <- rbind(sum_clean, summary(mod)$random$macroarea)

row.names(sum_clean) <- c("fixed_intercept","fixed_V3","random_sd_Int_spatial","random_sd_Int_phylo","random_sd_Int_macro","random_sd_Int_V3_macro","random_cor_Int_V3_macro")

sink("summary_clean.txt")
print(sum_clean)
sink()

ranef_1 <- ranef(mod)
# https://www.andrewheiss.com/blog/2021/12/01/multilevel-models-panel-data-guide/

write.table(ranef_1$ID2[, , "Intercept"], file = "ranef_spatial.txt", sep = '\t', quote = F)
write.table(ranef_1$V1[, , "Intercept"], file = "ranef_phylo.txt", sep = '\t', quote = F)
write.table(ranef_1$macroarea[, , "Intercept"], file = "ranef_macroarea_intercept.txt", sep = '\t', quote = F)
write.table(ranef_1$macroarea[, , "V3"], file = "ranef_macroarea_V3.txt", sep = '\t', quote = F)

# all in all

save(mod, file = "mod.RData")

