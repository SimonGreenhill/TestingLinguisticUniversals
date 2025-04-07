# Info about Universal. 
# Universal 41. If in a language the verb follows both the nominal subject and 
#nominal object as the dominant order, the language almost always has a case system. 

# GB133: Is a pragmatically unmarked constituent order verb-final for transitive clauses?  
# GB070:Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?  
# GB071 Are there morphological cases for pronominal core arguments (i.e. S/A/P)?
# GB072	Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?
# GB073	Are there morphological cases for oblique independent personal pronouns (i.e. not S/A/P)?

#GB133:1 > GB070|GB071|GB072|GB073:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB133_case <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132","GB133", "GB070","GB071","GB072","GB073"))

GB133_case_compl <- GB133_case[complete.cases(GB133_case),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB133_case_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB133_case_compl_pruned <- GB133_case_compl[ !(GB133_case_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB133_case_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile
for(i in 1:nrow(GB133_case_compl_pruned)){
  if(GB133_case_compl_pruned$GB070[i] == 1 | GB133_case_compl_pruned$GB071[i] == 1 | GB133_case_compl_pruned$GB072[i] == 1 | GB133_case_compl_pruned$GB073[i] == 1){GB133_case_compl_pruned$case[i] <- 1}
  else(GB133_case_compl_pruned$case[i] <- 0)
}

for(i in 1:nrow(GB133_case_compl_pruned)){
  if(GB133_case_compl_pruned$GB131[i] == 0 & GB133_case_compl_pruned$GB132[i] == 0 & GB133_case_compl_pruned$GB133[i] == 1){GB133_case_compl_pruned$worder[i] <- 1}
  else(GB133_case_compl_pruned$worder[i] <- 0)
}

GB133_case_compl_pruned2 <- subset(x = GB133_case_compl_pruned, select = c("Language_ID", "worder","case"))


# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB133_case_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

