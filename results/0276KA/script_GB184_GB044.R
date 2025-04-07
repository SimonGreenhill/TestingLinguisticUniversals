#Universal 276 (used to be 277) If there is a regular number distinction in the forms of the 
#adjective or of the verb, there is the same distinction in the forms of the noun of  the 
#given language.

#Relevant Grambank features:
#Adjective:
#GB184 Can an adnominal property word agree with the noun in number?
#Noun (regular distinctions):
#GB044 Is there productive morphological plural marking on nouns?
# GB042    Is there productive overt morphological singular marking on nouns?   
# GB043    Is there productive morphological dual marking on nouns?   
# GB165    Is there productive morphological trial marking on nouns?
# GB166    Is there productive morphological paucal marking on nouns?

# GB184:1 > GB042|GB043|GB044|GB165|GB166: 1
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB184_GB044 <- subset(x = GB_wide_strict, select = c("Language_ID", "GB184", "GB044", "GB042", "GB043", "GB165", "GB166"))

for(i in 1:nrow(GB184_GB044)){
  summ <- sum(c(as.numeric(GB184_GB044$GB044[i]), as.numeric(GB184_GB044$GB042[i]), as.numeric(GB184_GB044$GB043[i]), as.numeric(GB184_GB044$GB165[i]), (GB184_GB044$GB166[i])), na.rm = T)
  if(summ > 0 ){GB184_GB044$Number_Noun[i] <- 1}
  else(GB184_GB044$Number_Noun[i] <- 0)
}

GB184_GB044c <- subset(x = GB184_GB044, select = c("Language_ID", "GB184", "Number_Noun"))

GB184_GB044_compl <- GB184_GB044[complete.cases(GB184_GB044c),]

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB184_GB044_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB184_GB044_compl_pruned <- GB184_GB044_compl[ !(GB184_GB044_compl$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, GB184_GB044_compl$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

GB184_GB044_compl_pruned2 <- subset(x = GB184_GB044_compl_pruned, select = c("Language_ID", "GB184", "Number_Noun"))

world_nexus_pruned
nrow(GB184_GB044_compl_pruned2)
table(GB184_GB044_compl_pruned2$GB184, GB184_GB044_compl_pruned2$Number_Noun)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB184_GB044_compl_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


  
  