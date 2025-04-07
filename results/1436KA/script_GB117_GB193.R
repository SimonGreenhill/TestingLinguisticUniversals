#Universal 1436. If in a language the adjective follows the noun, there is an overt copula 
#form (of 'to be') in the present tense form. 

#Relevant features
#Copula
#YES for GB117 Is there a copula for predicate nominals?
#The adjective follows the noun:
#"2" for GB193 What is the order of adnominal property word and noun?
#GB193:2 > GB117:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB117_GB193 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB117", "GB193"))

GB117_GB193_compl <- GB117_GB193[complete.cases(GB117_GB193),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB117_GB193_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB117_GB193_compl_pruned <- GB117_GB193_compl[ !(GB117_GB193_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB117_GB193_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB193:2 > GB117:1

for(i in 1:nrow(GB117_GB193_compl_pruned)){
  if(GB117_GB193_compl_pruned$GB193[i] == '2') {GB117_GB193_compl_pruned$N_Adj[i] <- 1}
  else(GB117_GB193_compl_pruned$N_Adj[i] <- 0)
}


GB117_GB193_compl_pruned2 <- subset(x = GB117_GB193_compl_pruned, select = c("Language_ID", "N_Adj", "GB117"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB117_GB193_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

