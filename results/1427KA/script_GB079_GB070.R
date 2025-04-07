#Universal 1427. If in a language there are no prefixes, then there will be inflectional 
#(i.e. not prepositional) local cases.

#Relevant features
#GB070	Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?	
#  GB071	Are there morphological cases for pronominal core arguments (i.e. S/A/P)?
#  GB072	Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?	
#  GB073	Are there morphological cases for independent oblique personal pronominal arguments (i.e. not S/A/P)?
  
#  GB079        Do verbs have prefixes/proclitics, other than those that only mark A, S or P (do include portmanteau: A & S + TAM)?
#  GB090        Can the S argument be indexed by a prefix/proclitic on the verb in the simple main clause?        
#  GB092        Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?        
#  GB094        Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?        
#  GB430        Can adnominal possession be marked by a prefix on the possessor?        
#  GB431        Can adnominal possession be marked by a prefix on the possessed noun?

# GB079:0 & GB090:0 & GB092:0 & GB094:0 & GB430:0 & GB431:0 > GB072|GB073:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB079_GB070 <- subset(x = GB_wide_strict, select = c("Language_ID","GB072", "GB073", "GB079", "GB090", "GB092", "GB094", "GB430", "GB431"))

GB079_GB070_compl <- GB079_GB070[complete.cases(GB079_GB070),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB079_GB070_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB079_GB070_compl_pruned <- GB079_GB070_compl[ !(GB079_GB070_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB079_GB070_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 
# GB079:0 & GB090:0 & GB092:0 & GB094:0 & GB430:0 & GB431:0 > GB072|GB073:1

for(i in 1:nrow(GB079_GB070_compl_pruned)){
  if((GB079_GB070_compl_pruned$GB079[i] == '0') & (GB079_GB070_compl_pruned$GB090[i] == '0') & (GB079_GB070_compl_pruned$GB092[i] == '0') & (GB079_GB070_compl_pruned$GB094[i] == '0') & (GB079_GB070_compl_pruned$GB430[i] == '0') & (GB079_GB070_compl_pruned$GB431[i] == '0')) {GB079_GB070_compl_pruned$No_prefixes[i] <- 1}
  else(GB079_GB070_compl_pruned$No_prefixes[i] <- 0)
}

for(i in 1:nrow(GB079_GB070_compl_pruned)){
  if((GB079_GB070_compl_pruned$GB072[i] == '1') | (GB079_GB070_compl_pruned$GB073[i] == '1')) {GB079_GB070_compl_pruned$Cases[i] <- 1}
  else(GB079_GB070_compl_pruned$Cases[i] <- 0)
}

GB079_GB070_compl_pruned2 <- subset(x = GB079_GB070_compl_pruned, select = c("Language_ID", "No_prefixes", "Cases"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB079_GB070_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")
