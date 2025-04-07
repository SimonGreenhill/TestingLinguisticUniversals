# Info about Universal. 
# The more developed a case system is, the less is its system of verbal tenses.

# GB070	Are there morphological cases for non-pronominal core arguments (i.e. S/A/P)?	
# GB071	Are there morphological cases for pronominal core arguments (i.e. S/A/P)?	
# GB072	Are there morphological cases for oblique non-pronominal NPs (i.e. not S/A/P)?	
# GB073	Are there morphological cases for oblique independent personal pronouns (i.e. not S/A/P)?

# GB082	Is there overt morphological marking of present tense on verbs?	
# GB083	Is there overt morphological marking on the verb dedicated to past tense?	
# GB084	Is there overt morphological marking on the verb dedicated to future tense?

#sum(GB070:1,GB071:1,GB072:1,GB073:1) < 2, sum(GB082:1,GB083:1,GB084:1) > 2 | GB309:1

library(ape)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("../..")

GB_wide_strict <- read.csv("GB_wide_strict.tsv", sep = "\t")

case_tense <- subset(x = GB_wide_strict, select = c("Language_ID", "GB070", "GB071","GB072","GB073","GB082","GB083","GB084", "GB309"))

# prepare datafile

for(i in 1:nrow(case_tense)){
  summ <- sum(c(as.numeric(case_tense$GB070[i]), as.numeric(case_tense$GB071[i]), as.numeric(case_tense$GB072[i]), as.numeric(case_tense$GB073[i])), na.rm = T)
  if(summ < 2 ){case_tense$case[i] <- 1}
  else(case_tense$case[i] <- 0)
}

for(i in 1:nrow(case_tense)){
  summ <- sum(c(as.numeric(case_tense$GB082[i]), as.numeric(case_tense$GB083[i]), as.numeric(case_tense$GB084[i])), na.rm = T)
  if(summ >= 2 ){case_tense$tense[i] <- 1}
  else(case_tense$tense[i] <- 0)
}

case_tense2 <- subset(x = case_tense, select = c("Language_ID", "case","tense","GB309"))
case_tense2 <- case_tense2[complete.cases(case_tense2),]

for(i in 1:nrow(case_tense2)){
 if((case_tense2$tense[i] == "1") | (case_tense2$GB309[i] == "1")){case_tense2$tense2[i] <- 1}
  else(case_tense2$tense2[i] <- 0)
}

case_tense3 <- subset(x = case_tense2, select = c("Language_ID", "case","tense2"))

# prune tree

world_nexus <- read.nexus("EDGE6635-merged-relabelled.tree") #table(duplicated(world_nexus$tip.label))
for(i in 1:6635){
  world_nexus$tip.label[i] <- substr(world_nexus$tip.label[i][1],start = 1, stop = 8)
} #table(duplicated(world_nexus$tip.label)) tells us this doesn't introduce duplicates

dropdata <- setdiff(case_tense3$Language_ID, world_nexus$tip.label)

# remove data that is not in the tree:

case_tense3_pruned <- case_tense3[ !(case_tense3$Language_ID %in% dropdata), ]

# remove tree tips for which there is no data:

droptips <- setdiff(world_nexus$tip.label, case_tense3_pruned$Language_ID)

world_nexus_pruned <- drop.tip(world_nexus, droptips)

world_nexus_pruned
nrow(case_tense3_pruned)
table(case_tense3_pruned$case, case_tense3_pruned$tense2)

# write files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.table(case_tense3_pruned, file = "BT_data.txt", sep = "\t", quote = F, row.names = F, col.names = F)

write.nexus(world_nexus_pruned, file = "pruned_tree.tree")


