# Info about Universal. 
#When the descriptive adjective precedes the noun, the demonstrative and the numeral, 
#with overwhelmingly more than chance frequency, do likewise. 

#GB193 on adjectives, GB024 on numeral, GB025 on demonstratives, correlated evolution

# Variety A, numerals
#GB193:1 > GB024:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB193_GB024 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB193", "GB024"))

GB193_GB024_compl <- GB193_GB024[complete.cases(GB193_GB024),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB193_GB024_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB193_GB024_compl_pruned <- GB193_GB024_compl[ !(GB193_GB024_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB193_GB024_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

GB193_GB024_compl_pruned$GB193[GB193_GB024_compl_pruned$GB193 == '2'] <- '0'
GB193_GB024_compl_pruned$GB193[GB193_GB024_compl_pruned$GB193 == '3'] <- '0'

table(GB193_GB024_compl_pruned$GB193)

GB193_GB024_compl_pruned$GB024[GB193_GB024_compl_pruned$GB024 == '2'] <- '0'
GB193_GB024_compl_pruned$GB024[GB193_GB024_compl_pruned$GB024 == '3'] <- '0'

table(GB193_GB024_compl_pruned$GB024)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB193_GB024_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



