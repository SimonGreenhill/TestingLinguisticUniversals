#Universal 237. If adjectives inflect for any category, nouns inflect for this category as well.
#The universal can be tested for two categories: number (version A) and gender (version B)

#A
#IF     Number in adjectives:
#GB184	Can an adnominal property word agree with the noun in number?
#THEN   Number in nouns:
#YES at least for one of the following:
#number
# GB041    Are there several nouns (more than three) which are suppletive for number?
# GB042    Is there productive overt morphological singular marking on nouns?   
# GB043    Is there productive morphological dual marking on nouns?   
# GB044    Is there productive morphological plural marking on nouns?
# GB165    Is there productive morphological trial marking on nouns?
# GB166    Is there productive morphological paucal marking on nouns?
# GB316    Is singular number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB317    Is dual number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB318    Is plural number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB319    Is trial number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB320    Is paucal number regularly marked in the noun phrase by a dedicated phonologically free element?


#A
library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

GB184_GB044_compl <- subset(x = GB_wide_strict, select = c("Language_ID", "GB184", "GB041", "GB042", "GB043", "GB044", "GB165", "GB166"))

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(GB184_GB044_compl$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

GB184_GB044_compl_pruned <- GB184_GB044_compl[ !(GB184_GB044_compl$Language_ID %in% dropdata), ]

for(i in 1:nrow(GB184_GB044_compl_pruned)){
  summ <- sum(c(as.numeric(GB184_GB044_compl_pruned$GB041[i]), 
                as.numeric(GB184_GB044_compl_pruned$GB042[i]), 
                as.numeric(GB184_GB044_compl_pruned$GB043[i]), 
                as.numeric(GB184_GB044_compl_pruned$GB044[i]), 
                as.numeric(GB184_GB044_compl_pruned$GB165[i]), 
                as.numeric(GB184_GB044_compl_pruned$GB166[i])), na.rm = T)
  if(summ > 0 ){GB184_GB044_compl_pruned$Number[i] <- 1}
  else(GB184_GB044_compl_pruned$Number[i] <- 0)
}

# prepare datafile for A version 
GB184_GB044_compl_pruned2 <- subset(x = GB184_GB044_compl_pruned, select = c("Language_ID", "GB184", "Number"))

GB184_GB044_compl_pruned3 <- GB184_GB044_compl_pruned2[complete.cases(GB184_GB044_compl_pruned2),]

# remove tree tips for which there is no data:
droptips <- setdiff(world_nexus$tip.label, GB184_GB044_compl_pruned3$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(GB184_GB044_compl_pruned3, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


