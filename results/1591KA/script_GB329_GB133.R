#Universal 1591. Clear cases of internal relative clauses are present only in languages 
#whose basic word order is SOV. 

#Relevant features
#Verb_final - YES for #GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?
#While also NO for:
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?

# Internally-headed relative clauses:
#GB329 Are there internally-headed relative clauses?

#GB329:1 > GB131:0 & GB132:0 & GB133:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB329_GB133 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB329"))

GB329_GB133_compl <- GB329_GB133[complete.cases(GB329_GB133),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB329_GB133_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB329_GB133_compl_pruned <- GB329_GB133_compl[ !(GB329_GB133_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB329_GB133_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

for(i in 1:nrow(GB329_GB133_compl_pruned)){
  if((GB329_GB133_compl_pruned$GB131[i] == '0') & (GB329_GB133_compl_pruned$GB132[i] == '0') & (GB329_GB133_compl_pruned$GB133[i] == '1')) {GB329_GB133_compl_pruned$Verb_final[i] <- 1}
  else(GB329_GB133_compl_pruned$Verb_final[i] <- 0)
}

GB329_GB133_compl_pruned2 <- subset(x = GB329_GB133_compl_pruned, select = c("Language_ID", "GB329", "Verb_final"))

table(GB329_GB133_compl_pruned2$Verb_final, GB329_GB133_compl_pruned2$GB329)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB329_GB133_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
