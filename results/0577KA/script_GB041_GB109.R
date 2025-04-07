#Universal 577 (used to be 580 in the old version). If any inflectional category 
#is expressed through suppletion in nouns, it is also expressed suppletively in 
#pronouns or verbs.

#Relevant features (can be tested for number only):
#GB041        Are there several nouns (more than three) which are suppletive for 
#             number?
#GB109        Is there verb suppletion for participant number?
# GB041:1 > GB109:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB041_GB109 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB041", "GB109"))

GB041_GB109_compl <- GB041_GB109[complete.cases(GB041_GB109),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB041_GB109_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB041_GB109_compl_pruned <- GB041_GB109_compl[ !(GB041_GB109_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB041_GB109_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

GB041_GB109_compl_pruned2 <- subset(x = GB041_GB109_compl_pruned, select = c("Language_ID", "GB041", "GB109"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB041_GB109_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

