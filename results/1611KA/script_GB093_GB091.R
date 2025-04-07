#Universal 1611. All languages with verb-patient agreement, regardless of type, 
#also have verb agreement with the agent as well.

#Relevant features
#GB094        Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#GB093        Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?
  
#GB092        Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#GB091        Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?

#GB094:1 | GB093:1 > GB092:1 | GB091:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB093_GB091 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB091", "GB092", "GB093", "GB094"))

GB093_GB091_compl <- GB093_GB091[complete.cases(GB093_GB091),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB093_GB091_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB093_GB091_compl_pruned <- GB093_GB091_compl[ !(GB093_GB091_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB093_GB091_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

for(i in 1:nrow(GB093_GB091_compl_pruned)){
  if((GB093_GB091_compl_pruned$GB093[i] == '1') | (GB093_GB091_compl_pruned$GB094[i] == '1')) {GB093_GB091_compl_pruned$Patient_Agreement[i] <- 1}
  else(GB093_GB091_compl_pruned$Patient_Agreement[i] <- 0)
}

for(i in 1:nrow(GB093_GB091_compl_pruned)){
  if((GB093_GB091_compl_pruned$GB091[i] == '1') | (GB093_GB091_compl_pruned$GB092[i] == '1')) {GB093_GB091_compl_pruned$Agent_Agreement[i] <- 1}
  else(GB093_GB091_compl_pruned$Agent_Agreement[i] <- 0)
}

GB093_GB091_compl_pruned2 <- subset(x = GB093_GB091_compl_pruned, select = c("Language_ID", "Patient_Agreement", "Agent_Agreement"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB093_GB091_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



