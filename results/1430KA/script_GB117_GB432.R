#Universal 1430. If a language has possessive suffixes on nouns, most commonly 
#it will lack overt copula form (of 'to be') in the present tense. 

#Relevant features

#Possessive suffixes - YES for either of the folllowing:
#GB432        Can adnominal possession be marked by a suffix on the possessor?
#GB433        Can adnominal possession be marked by a suffix on the possessed noun?

#The lack of overt copula - "NO" for:
# GB117 Is there a copula for predicate nominals?

#GB432:1 | GB433:1 > GB117:0
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB117_GB432 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB117", "GB432", "GB433"))

GB117_GB432_compl <- GB117_GB432[complete.cases(GB117_GB432),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB117_GB432_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB117_GB432_compl_pruned <- GB117_GB432_compl[ !(GB117_GB432_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB117_GB432_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB432:1 | GB433:1 > GB117:0

for(i in 1:nrow(GB117_GB432_compl_pruned)){
  if((GB117_GB432_compl_pruned$GB432[i] == '1') | (GB117_GB432_compl_pruned$GB433[i] == '1')) {GB117_GB432_compl_pruned$Possessive_suffixes[i] <- 1}
  else(GB117_GB432_compl_pruned$Possessive_suffixes[i] <- 0)
}

for(i in 1:nrow(GB117_GB432_compl_pruned)){
  if(GB117_GB432_compl_pruned$GB117[i] == '0') {GB117_GB432_compl_pruned$No_Copula[i] <- 1}
  else(GB117_GB432_compl_pruned$No_Copula[i] <- 0)
}


GB117_GB432_compl_pruned2 <- subset(x = GB117_GB432_compl_pruned, select = c("Language_ID", "Possessive_suffixes", "No_Copula"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB117_GB432_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
