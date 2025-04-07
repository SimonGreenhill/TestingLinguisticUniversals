#Universal 506. If a language has inflection, it always has derivation.

#inflection > word formation

#sum(GB042, GB043, GB044, GB051, GB052, GB053, GB054, GB070, GB072, GB082, GB083, GB084, GB086, GB089, GB090, GB091, GB092, GB093, GB094, GB103, GB104, GB107, GB108, GB113, GB114, GB115, GB119, GB120, GB121, GB147, GB148, GB149, GB155, GB165, GB166, GB192, GB286, GB298, GB312, GB321, GB322, GB323, GB430, GB431, GB432, GB433) >=1 > sum(GB047, GB048, GB049) >=1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

data_frame <- subset(x = GB_wide_strict, select = c("Language_ID", "GB042", "GB043", "GB044", "GB051", "GB052", "GB053", "GB054", "GB070", "GB072", "GB082", "GB083", "GB084", "GB086", "GB089", "GB090", "GB091", "GB092", "GB093", "GB094", "GB103", "GB104", "GB107", "GB108", "GB113", "GB114", "GB115", "GB119", "GB120", "GB121", "GB147", "GB148", "GB149", "GB155", "GB165", "GB166", "GB192", "GB286", "GB298", "GB312", "GB321", "GB322", "GB323", "GB430", "GB431", "GB432", "GB433", "GB047", "GB048", "GB049"))

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

#making sure the first set of conditions is met: sum(GB042, GB043, GB044, GB051, GB052, GB053, GB054, GB070, GB072, GB082, GB083, GB084, GB086, GB089, GB090, GB091, GB092, GB093, GB094, GB103, GB104, GB107, GB108, GB113, GB114, GB115, GB119, GB120, GB121, GB147, GB148, GB149, GB155, GB165, GB166, GB192, GB286, GB298, GB312, GB321, GB322, GB323, GB430, GB431, GB432, GB433) >=1
for(i in 1:nrow(data_frame_compl_pruned)){
  summ <- sum(c(as.numeric(data_frame_compl_pruned$GB042[i]), as.numeric(data_frame_compl_pruned$GB043[i]), 
                as.numeric(data_frame_compl_pruned$GB044[i]), as.numeric(data_frame_compl_pruned$GB051[i]), 
                as.numeric(data_frame_compl_pruned$GB052[i]), as.numeric(data_frame_compl_pruned$GB053[i]), 
                as.numeric(data_frame_compl_pruned$GB054[i]), as.numeric(data_frame_compl_pruned$GB070[i]), 
                as.numeric(data_frame_compl_pruned$GB072[i]), as.numeric(data_frame_compl_pruned$GB082[i]), 
                as.numeric(data_frame_compl_pruned$GB083[i]), as.numeric(data_frame_compl_pruned$GB084[i]), 
                as.numeric(data_frame_compl_pruned$GB086[i]), as.numeric(data_frame_compl_pruned$GB089[i]), 
                as.numeric(data_frame_compl_pruned$GB090[i]), as.numeric(data_frame_compl_pruned$GB091[i]), 
                as.numeric(data_frame_compl_pruned$GB092[i]), as.numeric(data_frame_compl_pruned$GB093[i]), 
                as.numeric(data_frame_compl_pruned$GB094[i]), as.numeric(data_frame_compl_pruned$GB103[i]), 
                as.numeric(data_frame_compl_pruned$GB104[i]), as.numeric(data_frame_compl_pruned$GB107[i]), 
                as.numeric(data_frame_compl_pruned$GB108[i]), as.numeric(data_frame_compl_pruned$GB113[i]),
                as.numeric(data_frame_compl_pruned$GB114[i]), as.numeric(data_frame_compl_pruned$GB115[i]), 
                as.numeric(data_frame_compl_pruned$GB119[i]), as.numeric(data_frame_compl_pruned$GB120[i]), 
                as.numeric(data_frame_compl_pruned$GB121[i]), as.numeric(data_frame_compl_pruned$GB147[i]), 
                as.numeric(data_frame_compl_pruned$GB148[i]), as.numeric(data_frame_compl_pruned$GB149[i]), 
                as.numeric(data_frame_compl_pruned$GB155[i]), as.numeric(data_frame_compl_pruned$GB165[i]),
                as.numeric(data_frame_compl_pruned$GB166[i]), as.numeric(data_frame_compl_pruned$GB192[i]), 
                as.numeric(data_frame_compl_pruned$GB286[i]), as.numeric(data_frame_compl_pruned$GB298[i]), 
                as.numeric(data_frame_compl_pruned$GB312[i]), as.numeric(data_frame_compl_pruned$GB321[i]), 
                as.numeric(data_frame_compl_pruned$GB322[i]), as.numeric(data_frame_compl_pruned$GB323[i]), 
                as.numeric(data_frame_compl_pruned$GB430[i]), as.numeric(data_frame_compl_pruned$GB431[i]),
                as.numeric(data_frame_compl_pruned$GB432[i]), as.numeric(data_frame_compl_pruned$GB433[i])
                ), na.rm = T)
  if(summ >= 1 ){data_frame_compl_pruned$Condition_1[i] <- 1}
  else(data_frame_compl_pruned$Condition_1[i] <- 0)
}

#making sure the second set of conditions is met: sum(GB047, GB048, GB049) >=1
for(i in 1:nrow(data_frame_compl_pruned)){
  summ <- sum(c(as.numeric(data_frame_compl_pruned$GB047[i]), as.numeric(data_frame_compl_pruned$GB048[i]), as.numeric(data_frame_compl_pruned$GB049[i])), na.rm = T)
  if(summ >= 1 ){data_frame_compl_pruned$Condition_2[i] <- 1}
  else(data_frame_compl_pruned$Condition_2[i] <- 0)
}

# write files
data_frame_compl_pruned2 <- subset(x = data_frame_compl_pruned, select = c("Language_ID", "Condition_1", "Condition_2"))

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(data_frame_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
