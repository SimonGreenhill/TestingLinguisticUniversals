#Universal 1163. If a language has a grammaticalized indefinite article, it is 
#likely to also have a definite article, while the reverse does not necessarily 
#hold true.

#Relevant features
#GB020        Are there definite or specific articles?
#GB021        Do indefinite nominals commonly have indefinite articles?

#  GB021:1 >  GB020:1
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB020_GB021 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB020", "GB021"))

GB020_GB021_compl <- GB020_GB021[complete.cases(GB020_GB021),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB020_GB021_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB020_GB021_compl_pruned <- GB020_GB021_compl[ !(GB020_GB021_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB020_GB021_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

GB020_GB021_compl_pruned2 <- subset(x = GB020_GB021_compl_pruned, select = c("Language_ID", "GB021", "GB020"))
table(GB020_GB021_compl_pruned2$GB020, GB020_GB021_compl_pruned2$GB021)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB020_GB021_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

