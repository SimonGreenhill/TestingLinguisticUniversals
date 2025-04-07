# Info about Universal. 
# IF a language has pronominal gender in the 2nd person singular, 
#THEN it has a greater probability of having this distinction in the 
#3rd person singular than of not having it.

# GB196	Is there a male/female distinction in 2nd person independent pronouns?
# GB030	Is there a gender distinction in independent 3rd person pronouns?

# GB196:1 > GB030:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB196_GB030 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB196", "GB030"))

GB196_GB030 <- GB196_GB030[complete.cases(GB196_GB030),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB196_GB030$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB196_GB030_pruned <- GB196_GB030[ !(GB196_GB030$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB196_GB030$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

table(GB196_GB030_pruned$GB196, GB196_GB030_pruned$GB030)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB196_GB030_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



