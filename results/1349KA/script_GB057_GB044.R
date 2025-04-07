#Universal 1349. Numeral classifiers occur in the languages where plural is weakly
#developed.

#Relevant features
#GB057        Are there numeral classifiers?
#GB044        Is there productive morphological plural marking on nouns?        
#GB318        Is plural number regularly marked in the noun phrase by a dedicated phonologically free element?

#GB044:0 & GB318:0 > GB057:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB057_GB044 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB057", "GB044", "GB318"))

GB057_GB044_compl <- GB057_GB044[complete.cases(GB057_GB044),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB057_GB044_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB057_GB044_compl_pruned <- GB057_GB044_compl[ !(GB057_GB044_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB057_GB044_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

for(i in 1:nrow(GB057_GB044_compl_pruned)){
  if((GB057_GB044_compl_pruned$GB044[i] == '0') & (GB057_GB044_compl_pruned$GB318[i] == '0')) {GB057_GB044_compl_pruned$No_plural[i] <- 1}
  else(GB057_GB044_compl_pruned$No_plural[i] <- 0)
}

GB057_GB044_compl_pruned2 <- subset(x = GB057_GB044_compl_pruned, select = c("Language_ID", "GB057", "No_plural"))

table(GB057_GB044_compl_pruned2$GB057, GB057_GB044_compl_pruned2$No_plural)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB057_GB044_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

