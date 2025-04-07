#Universal 1152. For any language, preposed relative clause implies postposed relative clause.

#Relevant features
#GB327        Can the relative clause follow the noun?        
#GB328        Can the relative clause precede the noun?
#GB328:1 > GB327:1

library(ape)
# GB328:1 > GB327:1

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB327_GB328 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB327", "GB328"))

GB327_GB328_compl <- GB327_GB328[complete.cases(GB327_GB328),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB327_GB328_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB327_GB328_compl_pruned <- GB327_GB328_compl[ !(GB327_GB328_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB327_GB328_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

GB327_GB328_compl_pruned2 <- subset(x = GB327_GB328_compl_pruned, select = c("Language_ID",  "GB328", "GB327"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB327_GB328_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

