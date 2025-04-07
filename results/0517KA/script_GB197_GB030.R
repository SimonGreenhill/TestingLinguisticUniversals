#Universal 517 (used to be 519 in the old version). If a language has gender distinctions in the 1st person, it always has gender distinctions in the 2nd or 3rd person, or in both.

#Relevant features

#if     
#YES for GB197        Is there a male/female distinction in 1st person independent pronouns?

#then YES for at least one of the conditions below: 
#GB030        Is there a gender distinction in independent 3rd person pronouns?
#GB196        Is there a male/female distinction in 2nd person independent pronouns? 

#GB197:1 > GB030:1 | GB196:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB197_GB030 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB030", "GB196", "GB197"))

GB197_GB030_compl <- GB197_GB030[complete.cases(GB197_GB030),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB197_GB030_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB197_GB030_compl_pruned <- GB197_GB030_compl[ !(GB197_GB030_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB197_GB030_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

for(i in 1:nrow(GB197_GB030_compl_pruned)){
  if((GB197_GB030_compl_pruned$GB030[i] == '1') | (GB197_GB030_compl_pruned$GB196[i] == '1')) {GB197_GB030_compl_pruned$Gender_2_3[i] <- 1}
  else(GB197_GB030_compl_pruned$Gender_2_3[i] <- 0)
}

GB197_GB030_compl_pruned2 <- subset(x = GB197_GB030_compl_pruned, select = c("Language_ID", "GB197", "Gender_2_3"))

# checks
world_nexus_pruned
nrow(GB197_GB030_compl_pruned2)
table(GB197_GB030_compl_pruned2$GB197, GB197_GB030_compl_pruned2$Gender_2_3)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB197_GB030_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

