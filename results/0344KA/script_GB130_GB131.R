#Universal 344 (used to be 345 in the old version). If the order VS is dominant
# with transitive verbs, then it will be dominant with intransitive verbs as well.

#relevant features
#VS in transitive clauses if "1" for:
#GB131: Is a pragmatically unmarked constituent order verb-initial for transitive clauses? (limitation: this might include not only VSO but also VOS languages)

#VS in intransitive clauses if "2" for:
#GB130: What is the pragmatically unmarked order of S and V in intransitive clauses?

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB130_GB131 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB130", "GB131"))

GB130_GB131_compl <- GB130_GB131[complete.cases(GB130_GB131),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB130_GB131_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB130_GB131_compl_pruned <- GB130_GB131_compl[ !(GB130_GB131_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB130_GB131_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

for(i in 1:nrow(GB130_GB131_compl_pruned)){
  if(GB130_GB131_compl_pruned$GB130[i] == '2') {GB130_GB131_compl_pruned$VS_Intransitive[i] <- 1}
  else(GB130_GB131_compl_pruned$VS_Intransitive[i] <- 0)
}

GB130_GB131_compl_pruned2 <- subset(x = GB130_GB131_compl_pruned, select = c("Language_ID", "GB131", "VS_Intransitive"))

# checks

world_nexus_pruned
nrow(GB130_GB131_compl_pruned2)
table(GB130_GB131_compl_pruned2$GB131, GB130_GB131_compl_pruned2$VS_Intransitive)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB130_GB131_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

