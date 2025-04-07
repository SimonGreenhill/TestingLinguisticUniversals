#UNiversal 1534. In verb-initial languages, the question particle, if any, occurs sentence 
#initial in yes-no questions. 

#Relevant features
#Verb-initial - YES for
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#While NO for 
#GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#Clause-initial question particle
#GB262 Is there a clause-initial polar interrogative particle?

#GB131:1 & GB132:0 & GB133:0 > GB262:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB262_GB131 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB262"))

GB262_GB131_compl <- GB262_GB131[complete.cases(GB262_GB131),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB262_GB131_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB262_GB131_compl_pruned <- GB262_GB131_compl[ !(GB262_GB131_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB262_GB131_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB131:1 & GB132:0 & GB133:0 > GB262:1

for(i in 1:nrow(GB262_GB131_compl_pruned)){
  if((GB262_GB131_compl_pruned$GB131[i] == '1') & (GB262_GB131_compl_pruned$GB132[i] == '0') & (GB262_GB131_compl_pruned$GB133[i] == '0')) {GB262_GB131_compl_pruned$Verb_initial[i] <- 1}
  else(GB262_GB131_compl_pruned$Verb_initial[i] <- 0)
}

GB262_GB131_compl_pruned2 <- subset(x = GB262_GB131_compl_pruned, select = c("Language_ID", "Verb_initial", "GB262"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB262_GB131_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


