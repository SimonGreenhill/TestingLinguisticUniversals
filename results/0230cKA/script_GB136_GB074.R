#Universal 230c. IF constituent order is rigid, THEN cases are absent, and vice versa; ; 
#If cases are absent, THEN the use of prepositions is extensive, and vice versa;; 
#IF order is rigid, THEN the use of prepositions is extensive, and vice versa. : 
#IF constituent order is flexible, THEN cases are present, and vice versa; ; 
#IF cases are present, THEN the use of prepositions is sparse, and vice versa; ; 
#IF order is flexible, THEN the use of prepositions is sparse, and vice versa.

#rigid order > Adp-N
# "#IF order is rigid, THEN the use of prepositions is extensive, and vice versa. : "

#GB136:1 > GB074:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB136", "GB074"))

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

#making sure the first set of conditions is met: GB136:1
for(i in 1:nrow(data_frame_compl_pruned)){
  if(data_frame_compl_pruned$GB136[i] == 1)
  {data_frame_compl_pruned$Condition_1[i] <- 1}
  else(data_frame_compl_pruned$Condition_1[i] <- 0)
}


#making sure the second set of conditions is met: GB074:1
for(i in 1:nrow(data_frame_compl_pruned)){
  if(data_frame_compl_pruned$GB074[i] == 1)
  {data_frame_compl_pruned$Condition_2[i] <- 1}
  else(data_frame_compl_pruned$Condition_2[i] <- 0)
}

# write files
data_frame_compl_pruned2 <- subset(x = data_frame_compl_pruned, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
