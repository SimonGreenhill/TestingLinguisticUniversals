#Universal 521 (used to be 523 in the old version). In more than one language, 
#and possibly in all, if the adnominal adjective agrees with the noun, so does 
#the adnominal demonstrative.

#Relevant features
#Version A (gender)
#GB170        Can an adnominal property word agree with the noun in gender/noun class?        
#GB171        Can an adnominal demonstrative agree with the noun in gender/noun class?  
  
#Version B (number)
#  GB184        Can an adnominal property word agree with the noun in number?
#GB185        Can an adnominal demonstrative agree with the noun in number?

#XX merged into one: 
#GB170:1 | GB184:1 > GB171:1 | GB185:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB170_GB185 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB170", "GB171","GB184","GB185"))

GB170_GB185_compl <- GB170_GB185[complete.cases(GB170_GB185),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB170_GB185_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB170_GB185_compl_pruned <- GB170_GB185_compl[ !(GB170_GB185_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB170_GB185_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile 

for(i in 1:nrow(GB170_GB185_compl_pruned)){
  if((GB170_GB185_compl_pruned$GB170[i] == '1') | (GB170_GB185_compl_pruned$GB184[i] == '1')) {GB170_GB185_compl_pruned$Agree_adj[i] <- 1}
  else(GB170_GB185_compl_pruned$Agree_adj[i] <- 0)
}

for(i in 1:nrow(GB170_GB185_compl_pruned)){
  if((GB170_GB185_compl_pruned$GB171[i] == '1') | (GB170_GB185_compl_pruned$GB185[i] == '1')) {GB170_GB185_compl_pruned$Agree_dem[i] <- 1}
  else(GB170_GB185_compl_pruned$Agree_dem[i] <- 0)
}

GB170_GB185_compl_pruned2 <- subset(x = GB170_GB185_compl_pruned, select = c("Language_ID", "Agree_adj", "Agree_dem"))

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB170_GB185_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

