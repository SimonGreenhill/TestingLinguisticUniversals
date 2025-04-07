# Info about Universal. 
# Universal 2. In languages with prepositions, the genitive almost always follows the governing noun, 
#while in languages with postpositions it almost always precedes. 

# GB074	Are there prepositions?
# GB075	Are there postpositions?

# GB065	What is the pragmatically unmarked order of adnominal possessor noun and possessed noun?
# 	1	Possessor-Possessed	581
# 2	Possessed-Possessor	510
# 3	both	287
# ?	Not known	50

#Variety A: Adp-N > N-Gen
# GB074:1 > GB065:2 

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB074_GB065 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB074", "GB065"))

GB074_GB065 <- GB074_GB065[complete.cases(GB074_GB065),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB074_GB065$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB074_GB065_pruned <- GB074_GB065[ !(GB074_GB065$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB074_GB065$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# checks 
nrow(GB074_GB065_pruned)
setdiff(GB074_GB065_pruned$Language_ID, world_nexus_pruned$tip.label)
setdiff(world_nexus_pruned$tip.label, GB074_GB065_pruned$Language_ID)
length(world_nexus_pruned$tip.label)


# prepare datafile

table(GB074_GB065_pruned$GB074, GB074_GB065_pruned$GB065)

GB074_GB065_pruned$GB065[GB074_GB065_pruned$GB065 == '1'] <- '0'
GB074_GB065_pruned$GB065[GB074_GB065_pruned$GB065 == '3'] <- '0'
GB074_GB065_pruned$GB065[GB074_GB065_pruned$GB065 == '2'] <- '1'

table(GB074_GB065_pruned$GB074, GB074_GB065_pruned$GB065)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB074_GB065_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


