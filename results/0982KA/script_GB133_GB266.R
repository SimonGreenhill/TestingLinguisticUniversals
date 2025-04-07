#Universal 982 (used to be 986 in the old version). If a language has a 
#locative comparative, then it is either SOV or VSO.

#GB266        Is there a comparative construction that employs a marker 
#of the standard which elsewhere has a locational meaning?

#Word order
#YES for either of the following while also NO for GB132:
#  GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses? (limitation: not only SOV but also OSV)
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?(limitation: not only VSO but also VOS)

#GB266:1 > GB131:1 | GB133:1 & GB132:0 

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB133_GB266 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB266"))

GB133_GB266_compl <- GB133_GB266[complete.cases(GB133_GB266),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB133_GB266_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB133_GB266_compl_pruned <- GB133_GB266_compl[ !(GB133_GB266_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB133_GB266_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB266:1 > GB131:1 | GB133:1 & GB132:0 

for(i in 1:nrow(GB133_GB266_compl_pruned)){
  if(((GB133_GB266_compl_pruned$GB131[i] == '1') | (GB133_GB266_compl_pruned$GB133[i] == '1')) & (GB133_GB266_compl_pruned$GB132[i] == '0')) {GB133_GB266_compl_pruned$Word_Order[i] <- 1}
  else(GB133_GB266_compl_pruned$Word_Order[i] <- 0)
}

GB133_GB266_compl_pruned2 <- subset(x = GB133_GB266_compl_pruned, select = c("Language_ID", "GB266", "Word_Order"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB133_GB266_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


