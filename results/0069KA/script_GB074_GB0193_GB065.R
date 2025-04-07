# Universal 69. If a language has Prep word order, then if the adjective follows the noun, the genitive follows the noun.

#Adp-N & N-Adj > N-Gen

#GB074:1 & GB193:2 > GB065:2

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB074", "GB193", "GB065"))

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

droptips <- setdiff(world_nexus$tip.label, data_frame_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

# combining GB133 and GB074 in one:
for(i in 1:nrow(data_frame_compl_pruned)){
  if(data_frame_compl_pruned$GB074[i] == 1 & 
     data_frame_compl_pruned$GB193[i] == 2)
  {data_frame_compl_pruned$GB074_GB0193[i] <- 1}
  else(data_frame_compl_pruned$GB074_GB0193[i] <- 0)
}

# setting GB065 to 2 and setting other options to 0
data_frame_compl_pruned$GB065[data_frame_compl_pruned$GB065 == '1'] <- '0'
data_frame_compl_pruned$GB065[data_frame_compl_pruned$GB065 == '3'] <- '0'
data_frame_compl_pruned$GB065[data_frame_compl_pruned$GB065 == '2'] <- '1'

# write files

data_frame_compl_pruned2 <- subset(x = data_frame_compl_pruned, select = c("Language_ID", "GB074_GB0193","GB065"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

