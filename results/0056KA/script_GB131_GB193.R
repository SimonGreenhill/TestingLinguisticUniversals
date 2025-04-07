# Info about Universal. 
# Universal 17. With overwhelmingly more than chance frequency, 
#languages with dominant order VSO have the adjective after the noun. 

# GB131 on clause initial V, GB193 on adjectives, correlated evolution

#GB193 Values - Feature GB193: What is the order of adnominal property word and noun?
#0	they cannot be used attributively	23
#1	ANM - N	295
#2	N - ANM	695
#3	both	168
#?	Not known	70

# GB131:1 > GB193:2

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB131_GB193 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131","GB132","GB133","GB193"))

GB131_GB193_compl <- GB131_GB193[complete.cases(GB131_GB193),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB131_GB193_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB131_GB193_compl_pruned <- GB131_GB193_compl[ !(GB131_GB193_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB131_GB193_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(GB131_GB193_compl_pruned)){
  if((GB131_GB193_compl_pruned$GB131[i] == '1') &  (GB131_GB193_compl_pruned$GB132[i] == '0') & (GB131_GB193_compl_pruned$GB133[i] == '0')){GB131_GB193_compl_pruned$worder[i] <- 1}
  else(GB131_GB193_compl_pruned$worder[i] <- 0)
}

GB131_GB193_compl_pruned$GB193[GB131_GB193_compl_pruned$GB193 == '1'] <- '0'
GB131_GB193_compl_pruned$GB193[GB131_GB193_compl_pruned$GB193 == '3'] <- '0'
GB131_GB193_compl_pruned$GB193[GB131_GB193_compl_pruned$GB193 == '2'] <- '1'

GB131_GB193_compl_pruned2 <- subset(x = GB131_GB193_compl_pruned, select = c("Language_ID", "worder", "GB193"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB131_GB193_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


