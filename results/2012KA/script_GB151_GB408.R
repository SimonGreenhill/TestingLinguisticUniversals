#Universal 2020. A system of switch-reference marking is found only in languages with an 
#accusative syntax.

#Relevant features:
#GB151 Is there an overt verb marker dedicated to signalling coreference or  noncoreference between the subject of one clause and an argument of an adjacent clause ("switch reference")?
#  GB408 Is there any accusative alignment of flagging?

#GB151:1 > GB408:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB151_GB408 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB151", "GB408"))

GB151_GB408_compl <- GB151_GB408[complete.cases(GB151_GB408),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB151_GB408_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB151_GB408_compl_pruned <- GB151_GB408_compl[ !(GB151_GB408_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB151_GB408_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

GB151_GB408_compl_pruned2 <- subset(x = GB151_GB408_compl_pruned, select = c("Language_ID", "GB151", "GB408"))

world_nexus_pruned
nrow(GB151_GB408_compl_pruned2)
table(GB151_GB408_compl_pruned2$GB151, GB151_GB408_compl_pruned2$GB408)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB151_GB408_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

