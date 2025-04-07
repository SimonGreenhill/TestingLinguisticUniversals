#Universal 1111. Lack of inflectional morphology implies fixed word order of direct nominal 
#arguments. The converse is not true, hardly even a tendency.

#IF case inflection on direct nominal arguments gets lost, THEN their word order becomes fixed, but not vice versa.

#Relevant features
#GB136        Is the order of core argument (i.e. S/A/P) constituents fixed?
#GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
#GB072	Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?	

#GB070:0 & GB072:0 > GB136:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB136 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB136","GB070", "GB072"))

GB070_GB136_compl <- GB070_GB136[complete.cases(GB070_GB136),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB070_GB136_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB070_GB136_compl_pruned <- GB070_GB136_compl[ !(GB070_GB136_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB070_GB136_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

for(i in 1:nrow(GB070_GB136_compl_pruned)){
  if((GB070_GB136_compl_pruned$GB070[i] == 0) & (GB070_GB136_compl_pruned$GB072[i] == 0))
  {GB070_GB136_compl_pruned$Condition_2[i] <- 1}
  else(GB070_GB136_compl_pruned$Condition_2[i] <- 0)
}

GB070_GB136_compl_pruned2 <- subset(x = GB070_GB136_compl_pruned, select = c("Language_ID", "Condition_2", "GB136"))
table(GB070_GB136_compl_pruned2$Condition_2, GB070_GB136_compl_pruned2$GB136)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB136_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
