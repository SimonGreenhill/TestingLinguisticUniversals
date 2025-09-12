#!/usr/bin/Rscript

library(brms)
library(ape)
#library(coda)
source('varcov.spatial_function.R')

addTaskCallback(function(...) {set.seed(123);TRUE})

glottolog_langs <- read.csv("Glottolog_Languages.csv")
datfra <- read.table(file = "0015aKA//BT_data.txt")
tree <- read.nexus("0015aKA/0001_or_2aKA/pruned_tree.tree")

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
  sample_n(10000) %>% 
  ggplot() +
  geom_point(aes(x = geo_dist, y = value)) +
  xlim(c(0, 700))

