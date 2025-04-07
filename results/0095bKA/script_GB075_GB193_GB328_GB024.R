#Universal 95b. IF there are postpositions, THEN (IF adjectives precede nouns or relatives precede nouns, THEN demonstratives preced nouns and numerals precede nouns) and (IF demonstratives precede nouns or numerals precede nouns, THEN genitives precede nouns).

#N-Adp & (Adj-N | Rel-N) > Num-N

#GB075:1 & (GB193:1 |  GB328:1) > GB024:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB075", "GB193", "GB328", "GB024"))

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

#making sure the first set of conditions is met: GB075:1 & (GB193:1 |  GB328:1)
for(i in 1:nrow(data_frame_compl_pruned)){
  if(data_frame_compl_pruned$GB075[i] == 1 & (data_frame_compl_pruned$GB193[i] == 1 | data_frame_compl_pruned$GB328[i] == 1))
  {data_frame_compl_pruned$Condition_1[i] <- 1}
  else(data_frame_compl_pruned$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: GB024:1
for(i in 1:nrow(data_frame_compl_pruned)){
  if(data_frame_compl_pruned$GB024[i] == 1)
  {data_frame_compl_pruned$Condition_2[i] <- 1}
  else(data_frame_compl_pruned$Condition_2[i] <- 0)
}

# write files

data_frame_compl_pruned2 <- subset(x = data_frame_compl_pruned, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
