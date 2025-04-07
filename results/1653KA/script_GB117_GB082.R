#Universal 1653. If a language is non-tensed, the nominal predicate does not require a copula. 

#Relevant features:

#Non-tensed languages - NO for all of the following:
# GB082        Is there overt morphological marking of present tense on verbs?        
#GB083        Is there overt morphological marking on the verb dedicated to past tense?        
#GB084        Is there overt morphological marking on the verb dedicated to future tense?        
#GB110        Is there verb suppletion for tense or aspect?           
#GB121        Can tense be marked by an inflecting word ("auxiliary verb")?        
#GB521        Can tense be marked by a non-inflecting word ("auxiliary particle")?
#Copula:
#GB117 Is there a copula for predicate nominals?

# GB083:0 > GB117:0

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB117_GB082 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB083", "GB117"))

GB117_GB082_compl <- GB117_GB082[complete.cases(GB117_GB082),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB117_GB082_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB117_GB082_compl_pruned <- GB117_GB082_compl[ !(GB117_GB082_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB117_GB082_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

world_nexus_pruned
nrow(GB117_GB082_compl_pruned)
table(GB117_GB082_compl_pruned$GB083, GB117_GB082_compl_pruned$GB117)

GB117_GB082_compl_pruned$GB083[GB117_GB082_compl_pruned$GB083 == '0'] <- 'no'
GB117_GB082_compl_pruned$GB083[GB117_GB082_compl_pruned$GB083 == '1'] <- '0'
GB117_GB082_compl_pruned$GB083[GB117_GB082_compl_pruned$GB083 == 'no'] <- '1'

GB117_GB082_compl_pruned$GB117[GB117_GB082_compl_pruned$GB117 == '0'] <- 'no'
GB117_GB082_compl_pruned$GB117[GB117_GB082_compl_pruned$GB117 == '1'] <- '0'
GB117_GB082_compl_pruned$GB117[GB117_GB082_compl_pruned$GB117 == 'no'] <- '1'

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB117_GB082_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

