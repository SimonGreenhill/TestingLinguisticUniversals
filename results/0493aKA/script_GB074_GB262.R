# Universal 9. With well more than chance frequency, when question particles or affixes 
# are specified in position by reference to the sentence as a whole, if initial, such elements 
# are found in prepositional languages, and, if final, in postpositional. 

#GB074 prep & GB075 post on adpositions; GB262 cl ini & GB263 cl fin on position of polar 
# interrogative particles, 

# will need to do both seperately, so a A & B version of this universal

#A

# a language has prepositions when:
# YES for GB074	Are there prepositions?

# question particles are sentence intial when:
# YES for GB262	Is there a clause-initial polar interrogative particle?

# GB262:1 > GB074:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB074_GB262 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB262","GB074"))

GB074_GB262_compl <- GB074_GB262[complete.cases(GB074_GB262),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB074_GB262_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB074_GB262_compl_pruned <- GB074_GB262_compl[ !(GB074_GB262_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB074_GB262_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# checks
world_nexus_pruned
nrow(GB074_GB262_compl_pruned)
table(GB074_GB262_compl_pruned$GB074, GB074_GB262_compl_pruned$GB262)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB074_GB262_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


