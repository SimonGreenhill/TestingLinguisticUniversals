#Universal 1743. Only in languages that have no overt articles do 
#non-specific direct  objects fail to be accusative-marked 
#(or, in general, fail to be marked in the same way as specific 
#direct objects).

#IF there is no nominal case marking, THEN there will be an article.

#Relevant features
#No overt articles - NO for all of the following:
#GB020 Are there definite or specific articles?
#GB021 Do indefinite nominals commonly have indefinite articles?
#GB022	Are there prenominal articles?	
#GB023	Are there postnominal articles?

#No accusative-marked objects:
#GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?

#GB070:0 & GB072:0 > GB020|GB021|GB022|GB023:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB070_GB020 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB020", "GB021", "GB022", "GB023", "GB070","GB072"))

for(i in 1:nrow(GB070_GB020)){
  summ <- sum(c(as.numeric(GB070_GB020$GB020[i]), 
                as.numeric(GB070_GB020$GB021[i]), 
                as.numeric(GB070_GB020$GB022[i]), 
                as.numeric(GB070_GB020$GB023[i])), na.rm = T)
  if(summ > 0 ){GB070_GB020$art[i] <- 1}
  else(GB070_GB020$art[i] <- 0)
}

GB070_GB020_2 <- subset(x = GB070_GB020, select = c("Language_ID", "art", "GB070","GB072"))
GB070_GB020_compl <- GB070_GB020_2[complete.cases(GB070_GB020_2),]

for(i in 1:nrow(GB070_GB020_compl)){
  if((GB070_GB020_compl$GB070[i] == '0') & (GB070_GB020_compl$GB072[i] == '0')){GB070_GB020_compl$No_Accusative_Marking[i] <- 1}
  else(GB070_GB020_compl$No_Accusative_Marking[i] <- 0)
}

GB070_GB020_compl2 <- subset(x = GB070_GB020_compl, select = c("Language_ID", "No_Accusative_Marking", "art"))

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB070_GB020_compl2$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB070_GB020_compl_pruned <- GB070_GB020_compl2[ !(GB070_GB020_compl2$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB070_GB020_compl2$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB070_GB020_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

