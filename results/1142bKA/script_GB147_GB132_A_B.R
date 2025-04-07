#Universal 1142. Passivization is prominent in SVO languages, but not at 
#all in OV languages.

# no passive > SOV

#Relevant features
#Passive present (passivisation)
#GB147        Is there a morphological passive marked on the lexical verb?      
#  GB304        Can the agent be expressed overtly in a passive clause?
#  GB302        Is there a phonologically free passive marker ("particle" or "auxiliary")?
  
 # GB131	Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
 # GB132        Is a pragmatically unmarked constituent order verb-medial for transitive clauses?        
# GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?

#Testing the claim for two word orders: SVO (Version A) and SOV (Version B)
#GB147:1 | GB304:1 | GB302:1 > GB131:0 & GB132:1 & GB133:0
#GB147:0 & GB304:0 & GB302:0 > GB131:0 & GB132:0 & GB133:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB147_GB132 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB131", "GB132", "GB133", "GB147", "GB302", "GB304"))

GB147_GB132_compl <- GB147_GB132[complete.cases(GB147_GB132),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB147_GB132_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB147_GB132_compl_pruned <- GB147_GB132_compl[ !(GB147_GB132_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB147_GB132_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
#GB147:0 & GB304:0 & GB302:0 > GB131:0 & GB132:0 & GB133:1

for(i in 1:nrow(GB147_GB132_compl_pruned)){
  if((GB147_GB132_compl_pruned$GB131[i] == '0') & (GB147_GB132_compl_pruned$GB132[i] == '0') & (GB147_GB132_compl_pruned$GB133[i] == '1')) {GB147_GB132_compl_pruned$Verb_final[i] <- 1}
  else(GB147_GB132_compl_pruned$Verb_final[i] <- 0)
}

for(i in 1:nrow(GB147_GB132_compl_pruned)){
  if(GB147_GB132_compl_pruned$GB147[i] == '0' & GB147_GB132_compl_pruned$GB302[i] == '0' & GB147_GB132_compl_pruned$GB304[i] == '0') {GB147_GB132_compl_pruned$NoPassive[i] <- 1}
  else(GB147_GB132_compl_pruned$NoPassive[i] <- 0)
}


GB147_GB132_compl_pruned2 <- subset(x = GB147_GB132_compl_pruned, select = c("Language_ID", "NoPassive", "Verb_final"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB147_GB132_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

