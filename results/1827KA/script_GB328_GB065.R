#Universal 1827. If a language shows the order RelNoun or AdjNoun in its noun phrase, 
#it is very likely to also show PossNoun, but not vice versa. 

#Relevant features
#PossNoun - 1 (YES): possessor-possessed
#GB065 What is the pragmatically unmarked order of adnominal possessor noun and possessed noun?
#AdjNoun - 1 (YES):
# GB193 What is the order of adnominal property word and noun?
#RelNoun - YES for:
# GB328  Can the relative clause precede the noun?

#GB328:1 | GB193:1 > GB065:1

library(ape)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB328_GB065 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB065", "GB193", "GB328"))

GB328_GB065_compl <- GB328_GB065[complete.cases(GB328_GB065),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB328_GB065_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB328_GB065_compl_pruned <- GB328_GB065_compl[ !(GB328_GB065_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB328_GB065_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(GB328_GB065_compl_pruned)){
  if((GB328_GB065_compl_pruned$GB193[i] == '1') | (GB328_GB065_compl_pruned$GB328[i] == '1')){GB328_GB065_compl_pruned$RelNoun_AdjNoun[i] <- 1}
  else(GB328_GB065_compl_pruned$RelNoun_AdjNoun[i] <- 0)
}

for(i in 1:nrow(GB328_GB065_compl_pruned)){
if((GB328_GB065_compl_pruned$GB065[i] == '1')){GB328_GB065_compl_pruned$PossNoun[i] <- 1}
else(GB328_GB065_compl_pruned$PossNoun[i] <- 0)
}

GB328_GB065_compl_pruned2 <- subset(x = GB328_GB065_compl_pruned, select = c("Language_ID", "RelNoun_AdjNoun", "PossNoun"))

world_nexus_pruned
nrow(GB328_GB065_compl_pruned2)
table(GB328_GB065_compl_pruned2$RelNoun_AdjNoun, GB328_GB065_compl_pruned2$PossNoun)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB328_GB065_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

