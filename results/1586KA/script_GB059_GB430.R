#Universal 1586. An opposition of "alienable" and "inalienable" possession is almost 
#inevitable in languages where possession is regularly head-marked. The alienable-inalienable 
#opposition is almost never signaled by dependent-marked morphology. 

#IF possession is regularly head-marked, THEN there will be an opposition of alienable and inalienable possession. 
#BY CONTRAPOSITION:
#IF there is no opposition of alienable and inalienable possession, THEN possession is regularly dependent-marked .

#Relevant features
#The alienable-inalienable opposition:
#GB059 Is the adnominal possessive construction different for alienable and inalienable nouns?

#Head-marked possession:
#GB431        Can adnominal possession be marked by a prefix on the possessed noun?
#GB433        Can adnominal possession be marked by a suffix on the possessed noun?

#Dependent-marked possession:
#  GB430        Can adnominal possession be marked by a prefix on the possessor?
#  GB432        Can adnominal possession be marked by a suffix on the possessor?

#the opposition between alienable and inalienable possession + head-marked possession
# GB431|GB433:1 > GB059:1

#Version A
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB059_GB430 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB059", "GB431", "GB433"))

GB059_GB430_compl <- GB059_GB430[complete.cases(GB059_GB430),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB059_GB430_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB059_GB430_compl_pruned <- GB059_GB430_compl[ !(GB059_GB430_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB059_GB430_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB431|GB433:1 > GB059:1

for(i in 1:nrow(GB059_GB430_compl_pruned)){
  if((GB059_GB430_compl_pruned$GB431[i] == '1') | (GB059_GB430_compl_pruned$GB433[i] == '1')) {GB059_GB430_compl_pruned$Head_Marking[i] <- 1}
  else(GB059_GB430_compl_pruned$Head_Marking[i] <- 0)
}

GB059_GB430_compl_pruned2 <- subset(x = GB059_GB430_compl_pruned, select = c("Language_ID", "Head_Marking", "GB059"))

table(GB059_GB430_compl_pruned2$Head_Marking, GB059_GB430_compl_pruned2$GB059)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB059_GB430_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



