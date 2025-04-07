# Info about Universal. 
# In consistent OV languages, interrogative markers follow verbs, occupying final position in the sentence.
# IF basic order is consistently OV, THEN interrogative markers follow the verb and are sentence-final.

# GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?	Harald	1410	Values
# GB132	Is a pragmatically unmarked constituent order verb-medial for transitive clauses?	Harald	1376	Values
# GB133	Is a pragmatically unmarked constituent order verb-final for transitive clauses?

# GB263	Is there a clause-final polar interrogative particle?

#OV > Sent-Q
#GB131:0 & (GB132:1 | GB133:1) > GB263:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

VO_GB263 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB263"))

VO_GB263 <- VO_GB263[complete.cases(VO_GB263),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(VO_GB263$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

VO_GB263 <- VO_GB263[ !(VO_GB263$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, VO_GB263$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(VO_GB263)){
  if((VO_GB263$GB131[i] == '0') & (VO_GB263$GB132[i] == '1' | VO_GB263$GB133[i] == '1')){VO_GB263$OV[i] <- 1}
  else(VO_GB263$OV[i] <- 0)
}
VO_GB263_pruned <- VO_GB263[ !(VO_GB263$Language_ID %in% dropdata), ]

table(VO_GB263_pruned$OV, VO_GB263_pruned$GB263)

VO_GB263_pruned2 <- subset(x = VO_GB263_pruned, select = c("Language_ID", "OV", "GB263"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(VO_GB263_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

