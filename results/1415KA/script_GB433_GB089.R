#Universal 1415. If heads of possessive constructions agree with 
#their possessors in a given language then verbs agree with subjects 
#in that language.
#Standardized. IF heads of possessive constructions (=possessees) 
#agree with their possessors, THEN verbs agree with subjects.

#Relevant features

#GB089 Can the S argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#GB090 Can the S argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#  GB091 Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#  GB092 Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?
  
#  GB433        Can adnominal possession be marked by a suffix on the possessed noun?
#  GB431        Can adnominal possession be marked by a prefix on the possessed noun?

#GB431:1|GB433:1 > GB091|GB092:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB433_GB089 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB091", "GB092", "GB433", "GB431"))

GB433_GB089_compl <- GB433_GB089[complete.cases(GB433_GB089),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB433_GB089_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB433_GB089_compl_pruned <- GB433_GB089_compl[ !(GB433_GB089_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB433_GB089_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB431:1|GB433:1 > GB091|GB092:1

for(i in 1:nrow(GB433_GB089_compl_pruned)){
  if( (GB433_GB089_compl_pruned$GB091[i] == '1') | (GB433_GB089_compl_pruned$GB092[i] == '1')) {GB433_GB089_compl_pruned$Verbal_Agreement[i] <- 1}
  else(GB433_GB089_compl_pruned$Verbal_Agreement[i] <- 0)
}

for(i in 1:nrow(GB433_GB089_compl_pruned)){
  if( (GB433_GB089_compl_pruned$GB433[i] == '1') | (GB433_GB089_compl_pruned$GB431[i] == '1')) {GB433_GB089_compl_pruned$Possessive_Agreement[i] <- 1}
  else(GB433_GB089_compl_pruned$Possessive_Agreement[i] <- 0)
}

GB433_GB089_compl_pruned2 <- subset(x = GB433_GB089_compl_pruned, select = c("Language_ID", "Possessive_Agreement", "Verbal_Agreement"))

table(GB433_GB089_compl_pruned2$Verbal_Agreement, GB433_GB089_compl_pruned2$Possessive_Agreement)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB433_GB089_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
