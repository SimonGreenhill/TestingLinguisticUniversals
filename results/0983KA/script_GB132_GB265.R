#Universal 983 (used to be 987 in the old version). If a language has an exceed comparative, then it is SVO.

#Relevant features:
#An exceed comparative
#GB265 Is there a comparative construction that includes a form that elsewhere means 'surpass, exceed'?
  
  #Word order
  #YES for GB132 Is a pragmatically unmarked constituent order verb-medial for transitive clauses?:
  #while NO for both:
  #  GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses? 
  #GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?

#GB265:1 > GB131:0 & GB132:1 & GB133:0
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB132_GB265 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB265"))

GB132_GB265_compl <- GB132_GB265[complete.cases(GB132_GB265),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB132_GB265_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB132_GB265_compl_pruned <- GB132_GB265_compl[ !(GB132_GB265_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB132_GB265_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB265:1 > GB131:0 & GB132:1 & GB133:0

for(i in 1:nrow(GB132_GB265_compl_pruned)){
  if((GB132_GB265_compl_pruned$GB131[i] == '0') & (GB132_GB265_compl_pruned$GB132[i] == '1') & (GB132_GB265_compl_pruned$GB133[i] == '0')) {GB132_GB265_compl_pruned$SVO[i] <- 1}
  else(GB132_GB265_compl_pruned$SVO[i] <- 0)
}

GB132_GB265_compl_pruned2 <- subset(x = GB132_GB265_compl_pruned, select = c("Language_ID", "GB265", "SVO"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB132_GB265_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


