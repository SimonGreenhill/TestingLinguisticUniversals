# Info about Universal. 
# If there is case-inflection on nouns, there is also case-inflection
# on some pronouns.

# GB070	Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?	Jakob	1365	Values
# GB071	Are there morphological cases for pronominal core arguments (i.e. S/A/P)?
# GB073	Are there morphological cases for oblique independent personal pronouns (i.e. not S/A/P)?

# GB070:1 > GB071:1 | GB073:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB071_GB073 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB070", "GB071","GB073"))

GB070_GB071_GB073 <- GB070_GB071_GB073[complete.cases(GB070_GB071_GB073),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB070_GB071_GB073$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB070_GB071_GB073_pruned <- GB070_GB071_GB073[ !(GB070_GB071_GB073$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB070_GB071_GB073$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(GB070_GB071_GB073_pruned)){
  if((GB070_GB071_GB073_pruned$GB071[i] == '1' | GB070_GB071_GB073_pruned$GB073[i] == '1')){GB070_GB071_GB073_pruned$proncase[i] <- 1}
  else(GB070_GB071_GB073_pruned$proncase[i] <- 0)
}
table(GB070_GB071_GB073_pruned$GB070, GB070_GB071_GB073_pruned$proncase)

GB070_GB071_GB073_2 <- subset(x = GB070_GB071_GB073_pruned, select = c("Language_ID", "GB070", "proncase"))


# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB071_GB073_2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


