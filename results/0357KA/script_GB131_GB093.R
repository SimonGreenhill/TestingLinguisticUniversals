#Universal 357 (used to be 358 in the old version). If in a dominant word order 
#VS the only possible forms are the forms of the V-s conjugation, then the 
#forms like V-o are also the only possible ones.

#languages with VS word order:
#GB131 Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
  
#Subject-agreement markers are V-s in the following languages:
#GB091 Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?
  
#Languages with V-o marking:
#GB093 Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?

library(ape) 

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB131_GB093 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB091","GB092","GB093", "GB094"))

GB131_GB093_compl <- GB131_GB093[complete.cases(GB131_GB093),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB131_GB093_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB131_GB093_compl_pruned <- GB131_GB093_compl[ !(GB131_GB093_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB131_GB093_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#languages with VS word order:
#GB131 Is a pragmatically unmarked constituent order verb-initial for transitive clauses?

#Subject-agreement markers are V-s in the following languages:
#GB091 Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?

#Languages with V-o marking:
#GB093 Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?

for(i in 1:nrow(GB131_GB093_compl_pruned)){
  if((GB131_GB093_compl_pruned$GB131[i] == '1') & (GB131_GB093_compl_pruned$GB132[i] == '0') & 
     (GB131_GB093_compl_pruned$GB133[i] == '0') & (GB131_GB093_compl_pruned$GB091[i] == '1') & 
     (GB131_GB093_compl_pruned$GB092[i] == '0')) {GB131_GB093_compl_pruned$VS_Vs[i] <- 1}
  else(GB131_GB093_compl_pruned$VS_Vs[i] <- 0)
}

for(i in 1:nrow(GB131_GB093_compl_pruned)){
  if((GB131_GB093_compl_pruned$GB093[i] == '1') & (GB131_GB093_compl_pruned$GB094[i] == '0')) {GB131_GB093_compl_pruned$Parg[i] <- 1}
  else(GB131_GB093_compl_pruned$Parg[i] <- 0)
}

GB131_GB093_compl_pruned2 <- subset(x = GB131_GB093_compl_pruned, select = c("Language_ID", "VS_Vs", "Parg"))

# checks
world_nexus_pruned
nrow(GB131_GB093_compl_pruned2)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB131_GB093_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

