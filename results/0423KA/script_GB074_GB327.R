# Universal 423 (used to be 424 in the old version). If a language has Prep word order, then if the verb position is 
#not SOV, the relative clause follows the noun.

#Prepositions present if YES for:
#GB074 Are there prepositions?

#Relative clauses follows the noun if YES for:
#GB327 Can the relative clause follow the noun?
  
#The verb position is not SOV (verb-final), if the verb position is either verb-initial or verb-medial: 
#NO for GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?	
#YES for GB132	Is a pragmatically unmarked constituent order verb-medial for transitive clauses?		
#or
#YES for GB133	Is a pragmatically unmarked constituent order verb-final for transitive clauses?

# GB074:1 & GB133:0 > GB327:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB074_GB327 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB133", "GB074", "GB327"))

GB074_GB327_compl <- GB074_GB327[complete.cases(GB074_GB327),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB074_GB327_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB074_GB327_compl_pruned <- GB074_GB327_compl[ !(GB074_GB327_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB074_GB327_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

for(i in 1:nrow(GB074_GB327_compl_pruned)){
  if((GB074_GB327_compl_pruned$GB133[i] == '0') &(GB074_GB327_compl_pruned$GB074[i] == '1')) {GB074_GB327_compl_pruned$Prepositions_not_SOV[i] <- 1}
  else(GB074_GB327_compl_pruned$Prepositions_not_SOV[i] <- 0)
}

GB074_GB327_compl2 <- subset(x = GB074_GB327_compl_pruned, select = c("Language_ID", "Prepositions_not_SOV", "GB327"))

# checks
world_nexus_pruned
nrow(GB074_GB327_compl2)
table(GB074_GB327_compl2$Prepositions_not_SOV, GB074_GB327_compl2$GB327)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB074_GB327_compl2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


