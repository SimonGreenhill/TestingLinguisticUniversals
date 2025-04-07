#Universal 1114. VO languages are exceptionlessly Comp-initial. OV languages exemplify both 
#final complementizers and initial complementizers.

#IF basic word order is VO, THEN complementizers are clause-initial.
#OR, BY CONTRAPOSITION:
#IF complementizers are clause-final, THEN basic word order is OV.

#Relevant features
#GB421        Is there a preposed complementizer in complements of verbs of thinking and/or knowing?
#GB422        Is there a postposed complementizer in complements of verbs of thinking and/or knowing?
  
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#GB132 Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#Version A: VO languages are exceptionlessly Comp-initial GB131:1|GB132:1 & GB133:0 > GB421:1
#VO languages:
#YES for either of #GB131 Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#OR GB132 Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#While also NO for GB133

#Comp-initial:
#YES for GB421 Is there a preposed complementizer in complements of verbs of thinking and/or knowing?

#(GB131:1 | GB132:1) & GB133:0  > GB421:1
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")
GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB421_GB131 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB421"))

GB421_GB131_compl <- GB421_GB131[complete.cases(GB421_GB131),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB421_GB131_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB421_GB131_compl_pruned <- GB421_GB131_compl[ !(GB421_GB131_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB421_GB131_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
# GB131:1|GB132:1 & GB133:0 > GB421:1

for(i in 1:nrow(GB421_GB131_compl_pruned)){
  if(((GB421_GB131_compl_pruned$GB131[i] == '1') | (GB421_GB131_compl_pruned$GB132[i] == '1')) & (GB421_GB131_compl_pruned$GB133[i] == '0')) {GB421_GB131_compl_pruned$VO[i] <- 1}
  else(GB421_GB131_compl_pruned$VO[i] <- 0)
}

GB421_GB131_compl_pruned2 <- subset(x = GB421_GB131_compl_pruned, select = c("Language_ID", "VO", "GB421"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB421_GB131_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

