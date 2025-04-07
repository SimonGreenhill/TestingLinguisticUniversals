#Universal 822 (used to be 825 in the old version). Correlatives are limited to 
#verb-final languages, and in fact, are largely limited to "loose" verb-final 
#ones, namely which permit some NPs, especially "heavy" ones to occur 
#to the right of the verb without any special effect of foregrounding or 
#backgrounding.

#IF there are correlatives, THEN basic order is loosely verb-final.

#Relevant features
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?
#  GB330        Are there correlative relative clauses?

#GB330:1 > GB131:0 & GB132:0 & GB133:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB330_GB133 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB330", "GB131", "GB132", "GB133"))

GB330_GB133_compl <- GB330_GB133[complete.cases(GB330_GB133),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB330_GB133_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB330_GB133_compl_pruned <- GB330_GB133_compl[ !(GB330_GB133_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB330_GB133_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

for(i in 1:nrow(GB330_GB133_compl_pruned)){
  if((GB330_GB133_compl_pruned$GB131[i] == '0') & (GB330_GB133_compl_pruned$GB132[i] == '0') & (GB330_GB133_compl_pruned$GB133[i] == '1')) {GB330_GB133_compl_pruned$Verb_final[i] <- 1}
  else(GB330_GB133_compl_pruned$Verb_final[i] <- 0)
}

GB330_GB133_compl_pruned2 <- subset(x = GB330_GB133_compl_pruned, select = c("Language_ID", "GB330", "Verb_final"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB330_GB133_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

