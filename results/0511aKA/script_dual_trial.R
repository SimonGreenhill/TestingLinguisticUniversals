# Info about Universal. 
#Universal 34. No language has a trial number unless it has a dual. No language has a dual unless it has a plural. 
# trial - dual

#IF there is a trial, THEN there is also a dual.
#IF there is a dual, THEN there is also a plural.

# GB319: Is trial number regularly marked in the noun phrase by a dedicated phonologically free element? – # GB165: Is there productive morphological trial marking on nouns?, 
# GB165: Is there productive morphological trial marking on nouns?

# GB043: Is there productive morphological dual marking on nouns?
# GB317: Is dual number regularly marked in the noun phrase by a dedicated phonologically free element?

# GB319|GB165:1 > GB043|GB317:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

dual_trial <- subset(x = GB_wide_strict, select = c("Language_ID", "GB043", "GB165","GB317","GB319"))

dual_trial_compl <- dual_trial[complete.cases(dual_trial),]

for(i in 1:nrow(dual_trial_compl)){
  if((dual_trial_compl$GB043[i] == 1) | (dual_trial_compl$GB317[i] == 1)){dual_trial_compl$dual[i] <- 1}
  else(dual_trial_compl$dual[i] <- 0)
}

for(i in 1:nrow(dual_trial_compl)){
  if((dual_trial_compl$GB165[i] == 1) | (dual_trial_compl$GB319[i] == 1)){dual_trial_compl$trial[i] <- 1}
  else(dual_trial_compl$trial[i] <- 0)
}

table(dual_trial_compl$dual, dual_trial_compl$trial)

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(dual_trial_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

dual_trial_compl_pruned <- dual_trial_compl[ !(dual_trial_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, dual_trial_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

dual_trial_compl_pruned2 <- subset(x = dual_trial_compl_pruned, select = c("Language_ID", "trial","dual"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(dual_trial_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


