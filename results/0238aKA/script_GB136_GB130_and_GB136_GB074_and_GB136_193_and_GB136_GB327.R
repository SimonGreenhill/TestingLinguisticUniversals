#Universal 238. If constituent order is rigid, all determined constituents precede their respective determining 
#constituents; in particular, subjects precede their predicates, verbs their complements (objects, prepositional phrases), 
#prepositions their complements (noun phrases), head nouns their attributive adjectives, relative clauses, and 
#complements (prepositional phrases).

#Relevant Grambank features for some parts of the universal that can be tested (broken down in several parts)

#Version A
#Rigid word order:
#YES for GB136	Is the order of core argument (i.e. S/A/P) constituents fixed?
#Subjects precede predicates
#1 for GB130: What is the pragmatically unmarked order of S and V in intransitive clauses?
#GB136: 1 > GB130: 1


  
#A
library(ape)
  
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB136_GB130 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB136", "GB130"))

GB136_GB130_compl <- GB136_GB130[complete.cases(GB136_GB130),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB136_GB130_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB136_GB130_compl_pruned <- GB136_GB130_compl[ !(GB136_GB130_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB136_GB130_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile for A version 
for(i in 1:nrow(GB136_GB130_compl_pruned)){
  if(GB136_GB130_compl_pruned$GB130[i] == '1'){GB136_GB130_compl_pruned$SV[i] <- 1}
  else(GB136_GB130_compl_pruned$SV[i] <- 0)
}

GB136_GB130_compl_pruned2 <- subset(x = GB136_GB130_compl_pruned, select = c("Language_ID", "GB136", "SV"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB136_GB130_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

