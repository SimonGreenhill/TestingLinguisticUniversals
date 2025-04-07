#Universal 45. If there is a construction in which the verb agrees with some member of the relational hierarchy Subject > DO > IO > Oblique > [Genitive], then there are at least some constructions in which the verb agrees with members higher on that hierarchy .

#Obj indexing > Sbj indexing

#(GB093:1 | GB094:1) > GB091:1 | GB092: 1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB093", "GB094", "GB091", "GB092"))

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

#making sure the first set of conditions is met: (GB093:1 | GB094:1)
for(i in 1:nrow(data_frame_compl_pruned)){
  if(data_frame_compl_pruned$GB093[i] == 1 | data_frame_compl_pruned$GB094[i] == 1)
  {data_frame_compl_pruned$Condition_1[i] <- 1}
  else(data_frame_compl_pruned$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met:  GB091:1 | GB092: 1
for(i in 1:nrow(data_frame_compl_pruned)){
  if(data_frame_compl_pruned$GB091[i] == 1 | data_frame_compl_pruned$GB092[i] == 1)
  {data_frame_compl_pruned$Condition_2[i] <- 1}
  else(data_frame_compl_pruned$Condition_2[i] <- 0)
}

# write files
data_frame_compl_pruned2 <- subset(x = data_frame_compl_pruned, select = c("Language_ID", "Condition_1", "Condition_2"))

table(data_frame_compl_pruned2$Condition_1, data_frame_compl_pruned2$Condition_2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
