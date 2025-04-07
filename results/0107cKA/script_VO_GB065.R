# Info about Universal. 
# Nominal modifiers (such as relative, adjectival, and attributive expressions) 
#follow nouns in VO languages and precede nouns in OV languages
#IF basic order is VO , THEN relative clauses, attributive adjectives, and attributive nouns follow their head nouns.

# GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?	
# GB132	Is a pragmatically unmarked constituent order verb-medial for transitive clauses?	
# GB133	Is a pragmatically unmarked constituent order verb-final for transitive clauses?

# GB065	What is the pragmatically unmarked order of adnominal possessor noun and possessed noun?

# 1	Possessor-Possessed	581
# 2	Possessed-Possessor	510
# 3	both	287
# ?	Not known

# Variety C ############################################################

# (GB131:1 | GB132:1) & GB133:0 > GB065:2

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

VO_GB065 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB065"))

VO_GB065 <- VO_GB065[complete.cases(VO_GB065),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(VO_GB065$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

VO_GB065_pruned <- VO_GB065[ !(VO_GB065$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, VO_GB065$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(VO_GB065_pruned)){
  if((VO_GB065_pruned$GB131[i] == '1' |  VO_GB065_pruned$GB132[i] == '1') & (VO_GB065_pruned$GB133[i] == '0')){VO_GB065_pruned$VO[i] <- 1}
  else(VO_GB065_pruned$VO[i] <- 0)
}

table(VO_GB065_pruned$VO, VO_GB065_pruned$GB065)
VO_GB065_pruned$GB065[VO_GB065_pruned$GB065 == '1'] <- '0'
VO_GB065_pruned$GB065[VO_GB065_pruned$GB065 == '3'] <- '0'

table(VO_GB065_pruned$VO, VO_GB065_pruned$GB065)

VO_GB065_pruned2 <- subset(x = VO_GB065_pruned, select = c("Language_ID", "VO", "GB065"))

for(i in 1:length(VO_GB065_pruned2$GB065)){
  if(VO_GB065_pruned2$GB065[i] =="2")
    {VO_GB065_pruned2$GB065[i] <- "1"}}

table(VO_GB065_pruned2$VO, VO_GB065_pruned2$GB065)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(VO_GB065_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



