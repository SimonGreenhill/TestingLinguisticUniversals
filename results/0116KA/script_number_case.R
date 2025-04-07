# Info about Universal. 
# number is, as has been seen, closer to the stem and generally present when case is present, 
#while the opposite relation holds far more rarely.
# IF there is the category of case, THEN there is also the category of number.

#number
# GB041	Are there several nouns (more than three) which are suppletive for number?
# GB042	Is there productive overt morphological singular marking on nouns?	
# GB043	Is there productive morphological dual marking on nouns?	
# GB044	Is there productive morphological plural marking on nouns?
# GB165	Is there productive morphological trial marking on nouns?	Hedvig	1365	Values
# GB166	Is there productive morphological paucal marking on nouns?
# GB316	Is singular number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB317	Is dual number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB318	Is plural number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB319	Is trial number regularly marked in the noun phrase by a dedicated phonologically free element?
# GB320	Is paucal number regularly marked in the noun phrase by a dedicated phonologically free element?

# not used
# GB184: Can an adnominal property word agree with the noun in number?
# GB185: Can an adnominal demonstrative agree with the noun in number?
# GB186: Can an article agree with the noun in number?

#case
# GB070	Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?	Jakob	1365	Values
# GB071	Are there morphological cases for pronominal core arguments (i.e. S/A/P)?	Jakob	1239	Values
# GB072	Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?	Jakob	1250	Values
# GB073	Are there morphological cases for oblique independent personal pronouns (i.e. not S/A/P)?

#case > number
#sum(GB070:1,GB071:1,GB072:1,GB073:1) > 0, sum(GB041:1,GB042:1,GB043:1,GB044:1,GB165:1,GB166:1,GB316:1,GB317:1,GB318:1,GB319:1,GB320:1) > 1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

number_case <- subset(x = GB_wide_strict, select = c("Language_ID", "GB070", "GB071","GB072","GB073",
                                                     "GB041","GB042","GB043","GB044","GB165","GB166","GB316","GB317","GB318","GB319","GB320"))

for(i in 1:nrow(number_case)){
  summ <- sum(c(as.numeric(number_case$GB070[i]), as.numeric(number_case$GB071[i]), as.numeric(number_case$GB072[i]), as.numeric(number_case$GB073[i])), na.rm = T)
  if(summ > 0 ){number_case$case[i] <- 1}
  else(number_case$case[i] <- 0)
}


for(i in 1:nrow(number_case)){
  summ <- sum(c(as.numeric(number_case$GB041[i]),as.numeric(number_case$GB042[i]),as.numeric(number_case$GB043[i]),as.numeric(number_case$GB044[i]), as.numeric(number_case$GB165[i]), as.numeric(number_case$GB166[i]), as.numeric(number_case$GB316[i]), as.numeric(number_case$GB317[i]), as.numeric(number_case$GB318[i]), as.numeric(number_case$GB319[i]), as.numeric(number_case$GB320[i])), na.rm = T)
  if(summ > 0 ){number_case$number[i] <- 1}
  else(number_case$number[i] <- 0)
}


# prune tree
world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

# remove data that is not in the tree:

dropdata <- setdiff(number_case$Language_ID, world_nexus$tip.label)

number_case_pruned <- number_case[ !(number_case$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, number_case$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

# prepare datafile

number_case_pruned2 <- subset(x = number_case_pruned, select = c("Language_ID","case","number"))

# write files

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(number_case_pruned2, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")



