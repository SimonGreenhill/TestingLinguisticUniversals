#Universal 1434. A large number of tense oppositions correlates with the absence of aspect 
#differentiation.

#Relevant features
#Aspect differentiation
#GB086        Is a morphological distinction between perfective and imperfective aspect available on verbs?

#Tense oppositions: defined as at least two "YES"'s for the following questions
#GB082        Is there overt morphological marking of present tense on verbs?  
#GB083        Is there overt morphological marking on the verb dedicated to past tense?
#GB084        Is there overt morphological marking on the verb dedicated to future tense?
#GB121	Can tense be marked by an inflecting word ("auxiliary verb")?
#GB521	Can tense be marked by a non-inflecting word ("auxiliary particle")?
#GB309        Are there multiple past or multiple future tenses, distinguishing distance from Time of Reference?

# GB309:1 > GB086:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB086_GB082 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB309", "GB086"))

GB086_GB082_compl <- GB086_GB082[complete.cases(GB086_GB082),]

table(GB086_GB082_compl$GB309, GB086_GB082_compl$GB086)

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB086_GB082_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB086_GB082_compl_pruned <- GB086_GB082_compl[ !(GB086_GB082_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB086_GB082_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB086_GB082_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

