# Info about Universal. 
# Universal 24. If the relative expression precedes the noun either as the only 
#construction or as an alternate construction, either the language is postpositional, 
#or the adjective precedes the noun or both. 
# GB328, ‘Can the relative clause precede the noun’, GB075 on postpositions, GB193 on adjectives

# Variety A
# GB328:1 > GB075:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB328_GB075 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB328", "GB075"))

GB328_GB075_compl <- GB328_GB075[complete.cases(GB328_GB075),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB328_GB075_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB328_GB075_compl_pruned <- GB328_GB075_compl[ !(GB328_GB075_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB328_GB075_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

table(GB328_GB075_compl_pruned$GB328)
table(GB328_GB075_compl_pruned$GB075)

# write files


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB328_GB075_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

