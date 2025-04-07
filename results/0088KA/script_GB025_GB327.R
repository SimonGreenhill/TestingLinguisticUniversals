# Info about Universal. 

# If a language has noun before demonstrative, then it has noun before relative clause.

# GB025	What is the order of adnominal demonstrative and noun?
# 	1	Dem-N	503
# 2	N-Dem	647
# 3	both.	181
# ?	Not known	98

# GB327	Can the relative clause follow the noun?	
# GB328	Can the relative clause precede the noun?

#GB025:2 > GB327:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB25_GB327 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB025", "GB327"))

GB25_GB327 <- GB25_GB327[complete.cases(GB25_GB327),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB25_GB327$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB25_GB327_pruned <- GB25_GB327[ !(GB25_GB327$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB25_GB327$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

table(GB25_GB327_pruned$GB025, GB25_GB327_pruned$GB327)

GB25_GB327_pruned$GB025[GB25_GB327_pruned$GB025 == '1'] <- '0'
GB25_GB327_pruned$GB025[GB25_GB327_pruned$GB025 == '3'] <- '0'
GB25_GB327_pruned$GB025[GB25_GB327_pruned$GB025 == '2'] <- '1'

table(GB25_GB327_pruned$GB025, GB25_GB327_pruned$GB327)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB25_GB327_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

