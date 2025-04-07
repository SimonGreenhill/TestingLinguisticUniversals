#Universal 573 (used to be 576 in the old version). If a language has obligatory 
#marking of (in)definiteness, then it has no obligatory marking of numeral 
#classification (but not vice versa).

#Relevant features
#Marking of (in)definiteness
#GB020        Are there definite or specific articles?        
#  GB021        Do indefinite nominals commonly have indefinite articles?
#Presence of classifiers
#  GB057        Are there numeral classifiers?

#if GB020|GB021:1 > GB057:0

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB020_GB057 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB020", "GB021", "GB057"))

GB020_GB057_compl <- GB020_GB057[complete.cases(GB020_GB057),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB020_GB057_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB020_GB057_compl_pruned <- GB020_GB057_compl[ !(GB020_GB057_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB020_GB057_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile
##if GB020|GB021:1 > GB057:0
for(i in 1:nrow(GB020_GB057_compl_pruned)){
  if((GB020_GB057_compl_pruned$GB020[i] == '1') | (GB020_GB057_compl_pruned$GB021[i] == '1')){GB020_GB057_compl_pruned$Definiteness[i] <- 1}
  else(GB020_GB057_compl_pruned$Definiteness[i] <- 0)
}

GB020_GB057_compl_pruned2 <- subset(x = GB020_GB057_compl_pruned, select = c("Language_ID", "Definiteness", "GB057"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB020_GB057_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

