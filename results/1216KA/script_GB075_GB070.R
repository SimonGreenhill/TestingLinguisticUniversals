#Universal 1216. If there are (local) postpositions, then there will also be nominal case 
#inflection (not necessarily including local cases, though).

#Relevant features
#GB075        Are there postpositions?
#GB070        Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
#GB072        Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?

#GB075:1 > GB070:1 | GB072:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB075_GB070 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB070", "GB072", "GB075"))

GB075_GB070_compl <- GB075_GB070[complete.cases(GB075_GB070),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB075_GB070_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB075_GB070_compl_pruned <- GB075_GB070_compl[ !(GB075_GB070_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB075_GB070_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB075:1 > GB070:1 | GB072:1

for(i in 1:nrow(GB075_GB070_compl_pruned)){
  if(GB075_GB070_compl_pruned$GB070[i] == '1' | GB075_GB070_compl_pruned$GB072[i] == '1') {GB075_GB070_compl_pruned$Case_inflection[i] <- 1}
  else(GB075_GB070_compl_pruned$Case_inflection[i] <- 0)
}

GB075_GB070_compl_pruned2 <- subset(x = GB075_GB070_compl_pruned, select = c("Language_ID", "GB075", "Case_inflection"))

table(GB075_GB070_compl_pruned2$GB075, GB075_GB070_compl_pruned2$Case_inflection)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB075_GB070_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
