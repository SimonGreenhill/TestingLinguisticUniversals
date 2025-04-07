#Universal 1372. A lexically distinct form of verb HAVE is generally missing in verb 
#peripheral languages (i.e. SOV, VOS). That is, a verb HAVE is generally confined to SVO languages.

#Relevant features
#GB250        Can predicative possession be expressed with a transitive 'habeo' verb?
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#  GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?        
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#GB250:1 > GB131:0 & GB132:1 & GB133:0

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB250_GB133 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB250"))

GB250_GB133_compl <- GB250_GB133[complete.cases(GB250_GB133),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB250_GB133_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB250_GB133_compl_pruned <- GB250_GB133_compl[ !(GB250_GB133_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB250_GB133_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB250:1 > GB131:0 & GB132:1 & GB133:0

for(i in 1:nrow(GB250_GB133_compl_pruned)){
  if((GB250_GB133_compl_pruned$GB131[i] == '0') & (GB250_GB133_compl_pruned$GB132[i] == '1') & (GB250_GB133_compl_pruned$GB133[i] == '0')) {GB250_GB133_compl_pruned$SVO[i] <- 1}
  else(GB250_GB133_compl_pruned$SVO[i] <- 0)
}

GB250_GB133_compl_pruned2 <- subset(x = GB250_GB133_compl_pruned, select = c("Language_ID", "GB250", "SVO"))

table(GB250_GB133_compl_pruned2$SVO, GB250_GB133_compl_pruned2$GB250)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB250_GB133_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
