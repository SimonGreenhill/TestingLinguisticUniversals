#Universal 71. If a language has Postp word order, and if the adjective precedes the noun, then the genitive precedes the noun.

#N-Adp & Adj-N > Gen-N

#GB075:1 & GB193:1 > GB065:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB075", "GB193", "GB065"))

data_frame_compl <- data_frame[complete.cases(data_frame),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(data_frame_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

data_frame_compl_pruned <- data_frame_compl[ !(data_frame_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, data_frame_compl_pruned$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

setdiff(world_nexus_pruned$tip.label, data_frame_compl_pruned$Language_ID)
setdiff(data_frame_compl_pruned$Language_ID, world_nexus_pruned$tip.label)

# prepare datafile

#making sure the first set of conditions is met
for(i in 1:nrow(data_frame_compl_pruned)){
  if(data_frame_compl_pruned$GB075[i] == 1 & data_frame_compl_pruned$GB193[i] == 1)
  {data_frame_compl_pruned$Condition_1[i] <- 1}
  else(data_frame_compl_pruned$Condition_1[i] <- 0)
}

# setting keeping GB065:1 and setting other options to 0
data_frame_compl_pruned$GB065[data_frame_compl_pruned$GB065 == '3'] <- '0'
data_frame_compl_pruned$GB065[data_frame_compl_pruned$GB065 == '2'] <- '0'

# write files

data_frame_compl_pruned2 <- subset(x = data_frame_compl_pruned, select = c("Language_ID", "Condition_1","GB065"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
