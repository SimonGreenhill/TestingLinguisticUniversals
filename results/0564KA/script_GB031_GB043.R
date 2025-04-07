#Universal 564 (used to be 566 in the old version). If the Dual extends to nouns, 
# it also extends to pronouns.

#Relevant features
#Pronouns and nouns with dual can be found under following features (limitation: 
#in the case of pronouns, those are not only dual forms but also unit augemnted 
#forms)

#Pronouns
# YES for GB031 Is there a dual or unit augmented form (in addition to plural 
#or augmented) for all person categories in the pronoun system?
#Nouns
#YES for at least one of the following:
#GB043 Is there productive morphological dual marking on nouns?        
#GB317 Is dual number regularly marked in the noun phrase by a dedicated 
#phonologically free element?

# GB043|GB317:1 > GB031:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB031_GB043 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB031", "GB043", "GB317"))

GB031_GB043_compl <- GB031_GB043[complete.cases(GB031_GB043),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB031_GB043_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB031_GB043_compl_pruned <- GB031_GB043_compl[ !(GB031_GB043_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB031_GB043_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB043|GB317:1 > GB031:1
for(i in 1:nrow(GB031_GB043_compl_pruned)){
  if((GB031_GB043_compl_pruned$GB043[i] == '1') | (GB031_GB043_compl_pruned$GB317[i] == '1')) {GB031_GB043_compl_pruned$Dual_Noun[i] <- 1}
  else(GB031_GB043_compl_pruned$Dual_Noun[i] <- 0)
}

GB031_GB043_compl_pruned2 <- subset(x = GB031_GB043_compl_pruned, select = c("Language_ID", "Dual_Noun", "GB031"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB031_GB043_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

