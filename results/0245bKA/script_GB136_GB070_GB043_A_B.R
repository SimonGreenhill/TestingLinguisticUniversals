#Universal 245. If constituent order is free without limits, then nominal words are inflected, but not vice versa; if constituent order is rigid without inversions, then there is no nominal inflection, but again not vice versa.

#Nouns are inflected:
#YES for at least one of the following:
#GB070 Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?
#GB072 Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?
#GB042	Is there productive overt morphological singular marking on nouns?
#GB043	Is there productive morphological dual marking on nouns?
#GB044	Is there productive morphological plural marking on nouns?
#GB165	Is there productive morphological trial marking on nouns?
#GB166	Is there productive morphological paucal marking on nouns?

#Free word order:
#NO for the following:
#GB136 Is the order of core argument (i.e. S/A/P) constituents fixed?

#Version B
#No inflection for nominal words and constituent order is rigid
#GB136:1 & GB260:0 > sum(GB070:1,GB072:1,GB042:1,GB043:1,GB044:1,GB046:1,GB165:1,GB166:1) = 0

#Version B: 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB136_GB070_GB043 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB136", "GB260","GB070","GB072","GB042","GB043", "GB044", "GB046","GB165", "GB166"))

for(i in 1:nrow(GB136_GB070_GB043)){
  summ <- sum(c(as.numeric(GB136_GB070_GB043$GB070[i]), 
                as.numeric(GB136_GB070_GB043$GB072[i]), 
                as.numeric(GB136_GB070_GB043$GB042[i]), 
                as.numeric(GB136_GB070_GB043$GB043[i]), 
                as.numeric(GB136_GB070_GB043$GB044[i]),
                as.numeric(GB136_GB070_GB043$GB046[i]),
                as.numeric(GB136_GB070_GB043$GB165[i]),
                as.numeric(GB136_GB070_GB043$GB166[i])), na.rm = T)
  if(summ > 0 ){GB136_GB070_GB043$Inflected_Noun[i] <- 1}
  else(GB136_GB070_GB043$Inflected_Noun[i] <- 0)
}

for(i in 1:nrow(GB136_GB070_GB043)){
  if(GB136_GB070_GB043$Inflected_Noun[i] == '0'){GB136_GB070_GB043$No_Inflection[i] <- 1}
  else(GB136_GB070_GB043$No_Inflection[i] <- 0)
}

GB136_GB070_GB043_compl <- GB136_GB070_GB043[complete.cases(GB136_GB070_GB043),]

for(i in 1:nrow(GB136_GB070_GB043_compl)){
  if(GB136_GB070_GB043_compl$GB136[i] == '1' & GB136_GB070_GB043_compl$GB260[i] == '0'){GB136_GB070_GB043_compl$Rigid_order[i] <- 1}
  else(GB136_GB070_GB043_compl$Rigid_order[i] <- 0)
}

GB136_GB070_GB043_compl <- subset(x = GB136_GB070_GB043_compl, select = c("Language_ID", "Rigid_order", "No_Inflection"))

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB136_GB070_GB043_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB136_GB070_GB043_compl_pruned <- GB136_GB070_GB043_compl[ !(GB136_GB070_GB043_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB136_GB070_GB043_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB136_GB070_GB043_compl_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")

