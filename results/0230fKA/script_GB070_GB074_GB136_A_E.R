#UNiversal 230. IF constituent order is rigid, THEN cases are absent, and vice versa; ; 
#If cases are absent, THEN the use of prepositions is extensive, and vice versa;; 
#IF order is rigid, THEN the use of prepositions is extensive, and vice versa. : 
#IF constituent order is flexible, THEN cases are present, and vice versa; ; 
#IF cases are present, THEN the use of prepositions is sparse, and vice versa; ; 
#IF order is flexible, THEN the use of prepositions is sparse, and vice versa.

#Version F: Flexible word order + no prepositions
#Flexible WO - NO:
#GB136	Is the order of core argument (i.e. S/A/P) constituents fixed?
#No prepositions - NO:
#GB074	Are there prepositions?
#GB136:0 > GB074:0

#Version F
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB074_GB136 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB074", "GB136"))

GB070_GB074_GB136_compl <- GB070_GB074_GB136[complete.cases(GB070_GB074_GB136),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB070_GB074_GB136_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB070_GB074_GB136_compl_pruned <- GB070_GB074_GB136_compl[ !(GB070_GB074_GB136_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB070_GB074_GB136_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile
#GB136:0 > GB074:0
for(i in 1:nrow(GB070_GB074_GB136_compl_pruned)){
  if(GB070_GB074_GB136_compl_pruned$GB074[i] == '0'){GB070_GB074_GB136_compl_pruned$No_Prepositions[i] <- 1}
  else(GB070_GB074_GB136_compl_pruned$No_Prepositions[i] <- 0)
}

for(i in 1:nrow(GB070_GB074_GB136_compl_pruned)){
  if(GB070_GB074_GB136_compl_pruned$GB136[i] == '0'){GB070_GB074_GB136_compl_pruned$Flexible_WO[i] <- 1}
  else(GB070_GB074_GB136_compl_pruned$Flexible_WO[i] <- 0)
}

GB070_GB074_GB136_compl_pruned2 <- subset(x = GB070_GB074_GB136_compl_pruned, select = c("Language_ID", "Flexible_WO","No_Prepositions"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB074_GB136_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


