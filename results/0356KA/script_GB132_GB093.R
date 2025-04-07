#Universal 356 (used to be 357 in the old version). In the languages where the verb agrees with 
#subject and object, if forms  like V-s are the only possible ones, then the dominant order is SV.

#Relevant features
#Picking languages with SV word order (= not verb-initial) - YES for either of them:
#GB132 Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133 Is a pragmatically unmarked constituent order verb-final for transitive clauses?
  
#Languages that have verbs agreeing with objects:
#GB093 Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#GB094	Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?

#Subject-agreement markers are V-s in the following languages:
#GB091 Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?

# (GB091:1 & GB092:0) & (GB093:1 | GB094:1) > (GB132:1 | GB133:1) 

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB132_GB093 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB132", "GB133", "GB093", "GB094", "GB091","GB092"))

GB132_GB093_compl <- GB132_GB093[complete.cases(GB132_GB093),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB132_GB093_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB132_GB093_compl_pruned <- GB132_GB093_compl[ !(GB132_GB093_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB132_GB093_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

# IF 1 for 
#GB091 Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#&# IF 1 for 
#GB093 Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#OR
#GB094	Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?
# THEN YES for either of them:
#GB132 Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133 Is a pragmatically unmarked constituent order verb-final for transitive clauses?

for(i in 1:nrow(GB132_GB093_compl_pruned)){
  if(((GB132_GB093_compl_pruned$GB091[i] == '1') & (GB132_GB093_compl_pruned$GB092[i] == '0')) & ((GB132_GB093_compl_pruned$GB093[i] == '1') | (GB132_GB093_compl_pruned$GB094[i] == '1'))) {GB132_GB093_compl_pruned$Agreement[i] <- 1}
  else(GB132_GB093_compl_pruned$Agreement[i] <- 0)
}

for(i in 1:nrow(GB132_GB093_compl_pruned)){
  if((GB132_GB093_compl_pruned$GB132[i] == '1') | (GB132_GB093_compl_pruned$GB133[i] == '1')) {GB132_GB093_compl_pruned$SV[i] <- 1}
  else(GB132_GB093_compl_pruned$SV[i] <- 0)
}

GB132_GB093_compl_pruned2 <- subset(x = GB132_GB093_compl_pruned, select = c("Language_ID", "Agreement", "SV"))

world_nexus_pruned
nrow(GB132_GB093_compl_pruned2)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB132_GB093_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


  