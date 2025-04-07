# Info about Universal. 

# Nominal modifiers (such as relative, adjectival, and attributive expressions) follow nouns in VO languages and precede nouns in OV languages

# GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?	
# GB132	Is a pragmatically unmarked constituent order verb-medial for transitive clauses?	
# GB133	Is a pragmatically unmarked constituent order verb-final for transitive clauses?

# GB327	Can the relative clause follow the noun?
# GB328	Can the relative clause precede the noun?

# Variety A - 1 ############################################################

# VO > N-Rel
# (GB131:1 | GB132:1) & GB133:0 > GB327:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

VO_GB327 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB327"))

VO_GB327 <- VO_GB327[complete.cases(VO_GB327),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(VO_GB327$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

VO_GB327_pruned <- VO_GB327[ !(VO_GB327$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, VO_GB327$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(VO_GB327_pruned)){
  if((VO_GB327_pruned$GB131[i] == '1' |  VO_GB327_pruned$GB132[i] == '1') & (VO_GB327_pruned$GB133[i] == '0')){VO_GB327_pruned$VO[i] <- 1}
  else(VO_GB327_pruned$VO[i] <- 0)
}

table(VO_GB327_pruned$VO, VO_GB327_pruned$GB327)

VO_GB327_pruned2 <- subset(x = VO_GB327_pruned, select = c("Language_ID", "VO", "GB327"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(VO_GB327_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



