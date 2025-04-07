# Info about Universal. 
# If a language has noun before numeral, then it has noun before relative clause.

# GB024	What is the order of numeral and noun in the NP?
# 	1	Num-N	486
# 2	N-Num	661
# 3	both.	144
# ?	Not known	136

# GB327	Can the relative clause follow the noun?	
# GB328	Can the relative clause precede the noun?

# A N-Num > N-Rel
# GB024:2 > GB327:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB327_GB024 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB024","GB327"))

GB327_GB024_compl <- GB327_GB024[complete.cases(GB327_GB024),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB327_GB024_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB327_GB024_compl_pruned <- GB327_GB024_compl[ !(GB327_GB024_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB327_GB024_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

GB327_GB024_compl_pruned$GB024[GB327_GB024_compl_pruned$GB024 == '1'] <- '0'
GB327_GB024_compl_pruned$GB024[GB327_GB024_compl_pruned$GB024 == '2'] <- '1'
GB327_GB024_compl_pruned$GB024[GB327_GB024_compl_pruned$GB024 == '3'] <- '0'

table(GB327_GB024_compl_pruned$GB024,GB327_GB024_compl_pruned$GB327)


# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB327_GB024_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

