# Info about Universal. 
# Universal 34. No language has a trial number unless it has a dual. 
#No language has a dual unless it has a plural. 

# dual - plural

# GB043: Is there productive morphological dual marking on nouns?
# GB044: Is there productive morphological plural marking on nouns?

# GB317: Is dual number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB318: Is plural number regularly marked in the noun phrase by a dedicated phonologically free element?

# GB043|GB317:1 > GB044|GB318:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

dual_plural <- subset(x = GB_wide_strict, select = c("Language_ID", "GB043", "GB044","GB317","GB318"))

dual_plural_compl <- dual_plural[complete.cases(dual_plural),]

for(i in 1:nrow(dual_plural_compl)){
  if((dual_plural_compl$GB043[i] == 1) | (dual_plural_compl$GB317[i] == 1 )){dual_plural_compl$dual[i] <- 1}
  else(dual_plural_compl$dual[i] <- 0)
}

for(i in 1:nrow(dual_plural_compl)){
  if((dual_plural_compl$GB044[i] == 1) | (dual_plural_compl$GB318[i] == 1 )){dual_plural_compl$plural[i] <- 1}
  else(dual_plural_compl$plural[i] <- 0)
}

table(dual_plural_compl$dual, dual_plural_compl$plural)

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(dual_plural_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

dual_plural_compl_pruned <- dual_plural_compl[ !(dual_plural_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, dual_plural_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile
dual_plural_compl_pruned2 <- subset(x = dual_plural_compl_pruned, select = c("Language_ID", "dual","plural"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(dual_plural_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
