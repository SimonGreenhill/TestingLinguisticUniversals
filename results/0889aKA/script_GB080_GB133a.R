#Universal 889 (used to be 892 in the old version). OV languages tend to have 
#suffixes and VO languages prefixes.

#Standardized
#IF the word order is OV, THEN there are suffixes.
#IF the word order is VO, THEN there are prefixes.

#Relevant features
#GB131        Is a pragmatically unmarked constituent order verb-initial for transitive clauses?
#GB132	Is a pragmatically unmarked constituent order verb-medial for transitive clauses?
#GB133        Is a pragmatically unmarked constituent order verb-final for transitive clauses?
  
#  Prefixing:
#  GB079        Do verbs have prefixes/proclitics, other than those that only mark A, S or P (do include portmanteau: A & S + TAM)?
#  GB090        Can the S argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#  GB092        Can the A argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#  GB094        Can the P argument be indexed by a prefix/proclitic on the verb in the simple main clause?
#  GB430        Can adnominal possession be marked by a prefix on the possessor?
#  GB431        Can adnominal possession be marked by a prefix on the possessed noun?
  
#  Suffixing:
#  GB080        Do verbs have suffixes/enclitics, other than those that only mark A, S or P (do include portmanteau: A & S + TAM)?
#  GB089        Can the S argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#  GB091        Can the A argument be indexed by a suffix/enclitic on the verb in the simple main clause?
#  GB093        Can the P argument be indexed by a suffix/enclitic on the verb in the simple main clause?
# GB432        Can adnominal possession be marked by a suffix on the possessor?
#  GB433        Can adnominal possession be marked by a suffix on the possessed noun?

#Version A: OV (or verb-final) + suffixes
# GB131:0 & (GB132:1 | GB133:1) > GB080|GB089|GB091|GB093|GB432|GB433:1
#Version B: VO (or verb-medial/verb-initial = non-verb-final) + prefixes
# (GB131:1 | GB132:1) & GB133:0 > GB079|GB090|GB092|GB094|GB430|GB431:1
#Version A

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB080_GB133 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB080", "GB089", "GB091", "GB093", "GB432", "GB433", "GB131", "GB132", "GB133"))

for(i in 1:nrow(GB080_GB133)){
  summ <- sum(c(as.numeric(GB080_GB133$GB080[i]), as.numeric(GB080_GB133$GB089[i]), as.numeric(GB080_GB133$GB091[i]), as.numeric(GB080_GB133$GB093[i]), as.numeric(GB080_GB133$GB432[i]), as.numeric(GB080_GB133$GB433[i])), na.rm = T)
  if(summ > 0 ){GB080_GB133$Suffixes[i] <- 1}
  else(GB080_GB133$Suffixes[i] <- 0)
}

GB080_GB133 <- subset(x = GB080_GB133, select = c("Language_ID", "GB131", "GB132", "GB133", "Suffixes"))

GB080_GB133_compl <- GB080_GB133[complete.cases(GB080_GB133),]

for(i in 1:nrow(GB080_GB133_compl)){
  if(GB080_GB133_compl$GB131[i] == 0 & (GB080_GB133_compl$GB132[i] == 1 | GB080_GB133_compl$GB133[i] == 1)){GB080_GB133_compl$worder[i] <- 1}
  else(GB080_GB133_compl$worder[i] <- 0)
}

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB080_GB133_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB080_GB133_compl_pruned <- GB080_GB133_compl[ !(GB080_GB133_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB080_GB133_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

GB080_GB133_compl_pruned2 <- subset(x = GB080_GB133_compl_pruned, select = c("Language_ID", "worder","Suffixes"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB080_GB133_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

  