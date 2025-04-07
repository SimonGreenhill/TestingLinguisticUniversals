#Universal 328 (used to be 329 in the old version). If a verb agrees with a subject in prefix, 
# then verbs have affixes or clitics which agree with the direct object.

#Relevant features:
#Subject
#GB090 Can the S argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#GB092 Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#Object:
#GB093 Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#GB094 Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#GB090:1|GB092:1 > GB093:1|GB094:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB090_GB093 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB092", "GB093", "GB094"))

GB090_GB093_compl <- GB090_GB093[complete.cases(GB090_GB093),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB090_GB093_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB090_GB093_compl_pruned <- GB090_GB093_compl[ !(GB090_GB093_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB090_GB093_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

for(i in 1:nrow(GB090_GB093_compl_pruned)){
  if((GB090_GB093_compl_pruned$GB093[i] == '1') | (GB090_GB093_compl_pruned$GB094[i] == '1')) {GB090_GB093_compl_pruned$Object[i] <- 1}
  else(GB090_GB093_compl_pruned$Object[i] <- 0)
}

GB090_GB093_compl_pruned2 <- subset(x = GB090_GB093_compl_pruned, select = c("Language_ID", "GB092", "Object"))

# checks

world_nexus_pruned
nrow(GB090_GB093_compl_pruned2)
table(GB090_GB093_compl_pruned2$Object, GB090_GB093_compl_pruned2$GB092)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB090_GB093_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

