#Universal 507. If the verb has categories of person-number or if it has categories of gender, it always has tense-mode categories.

#(person)-number (verb) > tense-mood (verb)

#(GB089|GB90|GB91|GB92|GB93|GB94) > 0 > (GB082|GB083|GB084|GB110|GB312) > 0

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB089", "GB090", "GB091", "GB092", "GB093", "GB094", "GB082", "GB083", "GB084", "GB110", "GB312"))

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

#making sure the first set of conditions is met: (GB089|GB90|GB91|GB92|GB93|GB94) > 0
for(i in 1:nrow(data_frame_compl_pruned)){
  summ <- sum(c(as.numeric(data_frame_compl_pruned$GB089[i]), as.numeric(data_frame_compl_pruned$GB90[i]), as.numeric(data_frame_compl_pruned$GB91[i]), as.numeric(data_frame_compl_pruned$GB92[i]), as.numeric(data_frame_compl_pruned$GB93[i]), as.numeric(data_frame_compl_pruned$GB94[i])), na.rm = T)
  if(summ > 0 ){data_frame_compl_pruned$Condition_1[i] <- 1}
  else(data_frame_compl_pruned$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: (GB082|GB083|GB084|GB110|GB312) > 0 
for(i in 1:nrow(data_frame_compl_pruned)){
  summ <- sum(c(as.numeric(data_frame_compl_pruned$GB082[i]), as.numeric(data_frame_compl_pruned$GB083[i]), as.numeric(data_frame_compl_pruned$GB084[i]), as.numeric(data_frame_compl_pruned$GB110[i]), as.numeric(data_frame_compl_pruned$GB312[i])), na.rm = T)
  if(summ >= 1 ){data_frame_compl_pruned$Condition_2[i] <- 1}
  else(data_frame_compl_pruned$Condition_2[i] <- 0)
}

# write files
data_frame_compl_pruned2 <- subset(x = data_frame_compl_pruned, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
