#Universal 238. If constituent order is rigid, all determined constituents precede their respective determining 
#constituents; in particular, subjects precede their predicates, verbs their complements (objects, prepositional phrases), 
#prepositions their complements (noun phrases), head nouns their attributive adjectives, relative clauses, and 
#complements (prepositional phrases).

#Relevant Grambank features for some parts of the universal that can be tested (broken down in several parts)

#Version D
#Rigid word order:
#YES for GB136	Is the order of core argument (i.e. S/A/P) constituents fixed?
#Nouns precede their relative clauses
#YES for GB327	Can the relative clause follow the noun?
#GB136: 1 > GB327: 1
  
#Version D
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB136_GB327 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB136", "GB327"))

GB136_GB327_compl <- GB136_GB327[complete.cases(GB136_GB327),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB136_GB327_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB136_GB327_compl_pruned <- GB136_GB327_compl[ !(GB136_GB327_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB136_GB327_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile for B version 

GB136_GB327_compl_pruned2 <- subset(x = GB136_GB327_compl_pruned, select = c("Language_ID", "GB136", "GB327"))

table(GB136_GB327_compl_pruned2$GB136, GB136_GB327_compl_pruned2$GB327)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB136_GB327_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



