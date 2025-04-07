#Universal 1550. In verb-initial languages the dominant order of relative clauses is always 
#postnominal. 

#Relevant features
#Verb_initial - YES for GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses? 
#While also NO for:
#GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#Postnominal relative clauses:
#GB327 Can the relative clause follow the noun?

#GB131:1 & GB132:0 & GB133:0 > GB327:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB131_GB327 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB327"))

GB131_GB327_compl <- GB131_GB327[complete.cases(GB131_GB327),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB131_GB327_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB131_GB327_compl_pruned <- GB131_GB327_compl[ !(GB131_GB327_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB131_GB327_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB131:1 & GB132:0 & GB133:0 > GB327:1

for(i in 1:nrow(GB131_GB327_compl_pruned)){
  if((GB131_GB327_compl_pruned$GB131[i] == '1') & (GB131_GB327_compl_pruned$GB132[i] == '0') & (GB131_GB327_compl_pruned$GB133[i] == '0')) {GB131_GB327_compl_pruned$Verb_initial[i] <- 1}
  else(GB131_GB327_compl_pruned$Verb_initial[i] <- 0)
}

GB131_GB327_compl_pruned2 <- subset(x = GB131_GB327_compl_pruned, select = c("Language_ID", "Verb_initial", "GB327"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB131_GB327_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


