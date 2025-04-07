#Universal 232
#If constituent order is rigid and there is no case inflection, then there is a 
# definite article, but not vice versa.

#GB136 Is the order of core argument (i.e. S/A/P) constituents fixed?
#GB020 Are there definite or specific articles?
#GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)? - Languages with case inflection
#GB072 Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?


#Rigid constituent word order:
#YES for GB136 Is the order of core argument (i.e. S/A/P) constituents fixed?

#Case Inflection is absent:
#NO for both of the following:
#GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)? 
#GB072 Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?

#Definite article present:
#YES for GB020 Are there definite or specific articles?

#GB136: 1 & GB070: 0 & GB072: 0 > GB020: 0

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB136_GB020 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB136", "GB020", "GB070","GB072"))

GB136_GB020_compl <- GB136_GB020[complete.cases(GB136_GB020),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB136_GB020_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB136_GB020_compl_pruned <- GB136_GB020_compl[ !(GB136_GB020_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB136_GB020_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile
for(i in 1:nrow(GB136_GB020_compl_pruned)){
  if((GB136_GB020_compl_pruned$GB136[i] == '1') & (GB136_GB020_compl_pruned$GB070[i] == '0') & (GB136_GB020_compl_pruned$GB072[i] == '0')){GB136_GB020_compl_pruned$Fixed[i] <- 1}
  else(GB136_GB020_compl_pruned$Fixed[i] <- 0)
}

GB136_GB020_compl_pruned2 <- subset(x = GB136_GB020_compl_pruned, select = c("Language_ID", "Fixed", "GB020"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB136_GB020_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


