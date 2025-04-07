# universal
# univ 4 GB075 on postpositions, GB133 on clause final V
#Universal 4. With overwhelmingly greater than chance frequency, 
#languages with normal SOV order are postpositional. 

# relevant Grambank questions
# GB074	Are there prepositions?
# GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
# GB133 Is a pragmatically unmarked constituent order verb-final for transitive clauses?
# GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?	Harald	1410	Values
# GB132	Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
# GB136: Is the order of core argument (i.e. S/A/P) constituents fixed?
# GB130 What is the pragmatically unmarked order of S and V in intransitive clauses?

# a language has postpositions when:
# YES for GB075	Are there postpositions?

# V final order is determined when:
# YES for GB133 Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#GB131:0 & GB132:0 & GB133:1 > GB075:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB075_GB133 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB075", "GB131", "GB132","GB133"))

GB075_GB133_compl <- GB075_GB133[complete.cases(GB075_GB133),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB075_GB133_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB075_GB133_compl_pruned <- GB075_GB133_compl[ !(GB075_GB133_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB075_GB133_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(GB075_GB133_compl_pruned)){
  if((GB075_GB133_compl_pruned$GB131[i] == "0") & (GB075_GB133_compl_pruned$GB132[i] == "0") & (GB075_GB133_compl_pruned$GB133[i] == "1") ){GB075_GB133_compl_pruned$verbf[i] <- 1}
  else(GB075_GB133_compl_pruned$verbf[i] <- 0)
}

GB075_GB133_compl_pruned2 <- subset(x = GB075_GB133_compl_pruned, select = c("Language_ID", "verbf","GB075"))

# checks

world_nexus_pruned
nrow(GB075_GB133_compl_pruned2)
table(GB075_GB133_compl_pruned2$verbf, GB075_GB133_compl_pruned2$GB075)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB075_GB133_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

