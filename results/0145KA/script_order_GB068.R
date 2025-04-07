# Info about Universal. 
# SOV correlates positively with non-verbal status of predicative adjectives, 
#whereas SVO and VSO have a tendency to favour verbal predicative adjectives.

#IF basic order is SVO or VSO, THEN predicative adjectives tend to be verbal, and vice versa.

# GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?	
# GB132	Is a pragmatically unmarked constituent order verb-medial for transitive clauses?	
# GB133	Is a pragmatically unmarked constituent order verb-final for transitive clauses?

# GB068 Do core adjectives (defined semantically as property concepts such as value, shape, age, dimension) act like verbs in predicative position?

#GB131:1 | GB132:1 & GB133:0 > GB068:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

order_GB068 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB068"))

order_GB068 <- order_GB068[complete.cases(order_GB068),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(order_GB068$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

order_GB068 <- order_GB068[ !(order_GB068$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, order_GB068$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

for(i in 1:nrow(order_GB068)){
  if((order_GB068$GB131[i] == '1' | order_GB068$GB132[i] == '1') & (order_GB068$GB133[i] == '0')){order_GB068$VO[i] <- 1}
  else(order_GB068$VO[i] <- 0)
}

table(order_GB068$VO, order_GB068$GB068)

order_GB068_2 <- subset(x = order_GB068, select = c("Language_ID", "VO", "GB068"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(order_GB068_2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


