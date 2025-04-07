# Info about Universal. 
# If the genitive follows the noun, then the relative clause follows the noun.

#GB065:
#1	Possessor-Possessed	581
#2	Possessed-Possessor	510
#3	both	287
#?	Not known	50

# GB327	Can the relative clause follow the noun?	
# GB328	Can the relative clause precede the noun?

#N-Gen > N-Rel
# GB065:2 > GB327:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB065_GB327 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB065", "GB327"))

GB065_GB327 <- GB065_GB327[complete.cases(GB065_GB327),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB065_GB327$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB065_GB327_pruned <- GB065_GB327[ !(GB065_GB327$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB065_GB327$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

GB065_GB327_pruned$GB065[GB065_GB327_pruned$GB065 == '1'] <- '0'
GB065_GB327_pruned$GB065[GB065_GB327_pruned$GB065 == '3'] <- '0'
GB065_GB327_pruned$GB065[GB065_GB327_pruned$GB065 == '2'] <- '1'

table(GB065_GB327_pruned$GB065, GB065_GB327_pruned$GB327)


# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB065_GB327_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

