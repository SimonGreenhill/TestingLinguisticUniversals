# Info about Universal. 
# If attributive adjectives have the form of relative clauses, there is a positive correlation with SVO.

#IF attributive adjectives have the form of relative clauses, THEN basic order is SVO, and vice versa.

# GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?	
# GB132	Is a pragmatically unmarked constituent order verb-medial for transitive clauses?	
# GB133	Is a pragmatically unmarked constituent order verb-final for transitive clauses?

# GB069	Do core adjectives (defined semantically as property concepts; value, shape, age, dimension) used attributively require the same morphological treatment as verbs?

# V-like Attr Adj > SVO
# GB069:1 > GB131:0 & GB132:1 & GB133:0

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

order_GB069 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB069"))

order_GB069 <- order_GB069[complete.cases(order_GB069),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(order_GB069$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

order_GB069 <- order_GB069[ !(order_GB069$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, order_GB069$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(order_GB069)){
  if((order_GB069$GB131[i] == '0') & (order_GB069$GB132[i] == '1') & (order_GB069$GB133[i] == '0')){order_GB069$VO[i] <- 1}
  else(order_GB069$VO[i] <- 0)
}

table(order_GB069$VO, order_GB069$GB069)

order_GB069_2 <- subset(x = order_GB069, select = c("Language_ID", "GB069","VO"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(order_GB069_2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

