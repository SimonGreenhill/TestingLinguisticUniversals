# universal
# univ 3 GB074 on prepositions, GB131 on verb-initial word order
# Universal 3. Languages with dominant VSO order are always prepositional. 

# relevant Grambank questions
# GB074	Are there prepositions?
# GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
# GB133 Is a pragmatically unmarked constituent order verb-final for transitive clauses?
# GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?	Harald	1410	Values
# GB132	Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
# GB136: Is the order of core argument (i.e. S/A/P) constituents fixed?
# GB130 What is the pragmatically unmarked order of S and V in intransitive clauses?

# a language has prepositions when:
# YES for GB074	Are there prepositions?

# VSO order is determined when:
# YES for GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
# NO for GB132	Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
# NO for GB133 Is a pragmatically unmarked constituent order verb-final for transitive clauses?
# '2' for GB130 What is the pragmatically unmarked order of S and V in intransitive clauses?

#GB131:1 & GB132:0 & GB133:0 > GB074:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB074_GB131 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB074", "GB131","GB132","GB133"))

GB074_GB131_compl <- GB074_GB131[complete.cases(GB074_GB131),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB074_GB131_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB074_GB131_compl_pruned <- GB074_GB131_compl[ !(GB074_GB131_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB074_GB131_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(GB074_GB131_compl_pruned)){
  if((GB074_GB131_compl_pruned$GB131[i] == '1') &  (GB074_GB131_compl_pruned$GB132[i] == '0') & (GB074_GB131_compl_pruned$GB133[i] == '0')){GB074_GB131_compl_pruned$VSO[i] <- 1}
  else(GB074_GB131_compl_pruned$VSO[i] <- 0)
}

GB074_GB131_compl_pruned2 <- subset(x = GB074_GB131_compl_pruned, select = c("Language_ID", "VSO", "GB074"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB074_GB131_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



